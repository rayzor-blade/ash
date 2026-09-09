//! Compiled-to-compiled snapshots. Not the interpreter's de-SSA register ABI.
//!
//! A layout names SSA values in one immutable AIR version, at the point
//! after header phis and before header instructions. Source and destination
//! retain that version; neither reconstructs identities from register numbers.
use std::collections::BTreeSet;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use air::v2::ir::{BlockId, CellId, TypeRef, ValueId};
use air::v2::{liveness::Liveness, CfgInfo};
use anyhow::{bail, Result};

use crate::air_pipeline::Optimized;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Input {
    Value(ValueId),
    Cell(CellId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Slot {
    pub input: Input,
    pub ty: TypeRef,
}

pub struct Layout {
    pub id: u64,
    pub findex: usize,
    pub header: BlockId,
    pub pc: usize,
    pub air: Arc<Optimized>,
    /// One eight-byte slot per scalar, raw low bits for narrow values.
    /// Vector snapshots are refused until both sides support wider slots.
    pub slots: Vec<Slot>,
}

impl Layout {
    pub fn new(findex: usize, air: Arc<Optimized>, header: BlockId) -> Result<Self> {
        static NEXT: AtomicU64 = AtomicU64::new(1);
        let f = &air.ir;
        let Some(block) = f.blocks.get(header.idx()) else {
            bail!("invalid re-tier header")
        };
        let cfg = CfgInfo::build(f);
        let liveness = Liveness::analyze(f, &cfg);
        let mut values: BTreeSet<ValueId> = liveness.live_in(header).clone();
        values.extend(block.phis.iter().map(|phi| phi.dst));
        let mut slots = Vec::new();
        for v in values {
            let data = &f.values[v.idx()];
            if data.lanes > 1 {
                bail!("re-tier snapshot needs vector v{}", v.0);
            }
            slots.push(Slot {
                input: Input::Value(v),
                ty: data.ty,
            });
        }
        // Cells are mutable storage, not an SSA value sharing their register.
        // Conservatively include all cells, reading their CURRENT contents.
        for (i, cell) in f.cells.iter().enumerate() {
            slots.push(Slot {
                input: Input::Cell(CellId(i as u32)),
                ty: cell.ty,
            });
        }
        if slots.len() > crate::osr::MAX_OSR_LIVE_INS {
            bail!("re-tier snapshot too large");
        }
        Ok(Self {
            id: NEXT.fetch_add(1, Ordering::Relaxed),
            findex,
            header,
            pc: air.ser.block_pcs[header.idx()],
            air,
            slots,
        })
    }
}

/// A publication slot belongs to a layout, not merely to (findex, pc).
/// Holding the Arc also prevents AIR identity reuse after cache invalidation.
pub struct Site {
    pub layout: Arc<Layout>,
    target: AtomicU64,
}

impl Site {
    pub fn new(layout: Layout) -> Self {
        Self {
            layout: Arc::new(layout),
            target: AtomicU64::new(0),
        }
    }
    pub fn address(&self) -> u64 {
        &self.target as *const AtomicU64 as u64
    }
    pub fn target(&self) -> u64 {
        self.target.load(Ordering::Acquire)
    }
    pub fn publish(&self, layout: &Arc<Layout>, code: u64) -> Result<()> {
        if !Arc::ptr_eq(layout, &self.layout) || code == 0 {
            bail!("re-tier target does not match the exit's snapshot layout");
        }
        self.target.store(code, Ordering::Release);
        Ok(())
    }
}

/// Scheduling hook for regression tests; no counter/helper in ordinary code.
pub fn test_after() -> Option<u64> {
    static AFTER: std::sync::OnceLock<Option<u64>> = std::sync::OnceLock::new();
    *AFTER.get_or_init(|| {
        std::env::var("ASH_TEST_RETIER_AFTER")
            .ok()?
            .parse::<u64>()
            .ok()
            .filter(|n| *n > 0)
    })
}

/// Wait at a chosen poll for the broker, rather than relying on compile timing.
/// Both pointers are embedded by codegen; the count belongs to this activation.
pub unsafe extern "C" fn test_poll(site: *const Site, count: *mut u64) -> u64 {
    let site = &*site;
    *count += 1;
    if *count < test_after().unwrap_or(1) {
        return 0;
    }
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
    loop {
        let target = site.target();
        if target != 0 {
            return target;
        }
        if std::time::Instant::now() >= deadline {
            eprintln!(
                "[retier] test publication timeout layout={}",
                site.layout.id
            );
            std::process::abort();
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

pub extern "C" fn record_transfer(layout: u64, osr: u64) {
    eprintln!(
        "[retier] taken layout={layout} source={}",
        if osr == 0 { "ordinary" } else { "osr" }
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use air::opcodes::{Opcode, RefInt, Reg};
    use air::v2::{lower::lower, serialize::serialize};

    fn fixture() -> (Arc<Optimized>, BlockId) {
        let ir = lower(
            &[
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Int {
                    dst: Reg(1),
                    ptr: RefInt(1),
                },
                Opcode::Incr { dst: Reg(0) },
                Opcode::JSLt {
                    a: Reg(0),
                    b: Reg(1),
                    offset: -2,
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            &[TypeRef(0), TypeRef(0)],
        )
        .unwrap();
        let header = BlockId(crate::osr::analyze(&ir).entry_headers[0]);
        let ser = serialize(&ir).unwrap();
        (Arc::new(Optimized { ir, ser }), header)
    }

    #[test]
    fn snapshot_contains_header_phis_and_external_live_ins() {
        let (air, header) = fixture();
        let layout = Layout::new(7, air.clone(), header).unwrap();
        let live = Liveness::analyze(&air.ir, &CfgInfo::build(&air.ir));
        assert!(!live.live_in(header).is_empty());
        assert!(!air.ir.blocks[header.idx()].phis.is_empty());
        for &v in live.live_in(header) {
            assert!(layout.slots.iter().any(|s| s.input == Input::Value(v)));
        }
        for phi in &air.ir.blocks[header.idx()].phis {
            assert!(layout
                .slots
                .iter()
                .any(|s| s.input == Input::Value(phi.dst)));
        }
    }

    #[test]
    fn two_ssa_inputs_with_the_same_register_keep_distinct_slots() {
        let (mut air, header) = fixture();
        let ir = &mut Arc::get_mut(&mut air).unwrap().ir;
        for value in &mut ir.values {
            value.reg = 0;
        }
        let layout = Layout::new(7, air, header).unwrap();
        assert!(layout.slots.len() >= 2);
        let values: BTreeSet<_> = layout
            .slots
            .iter()
            .filter_map(|s| match s.input {
                Input::Value(v) => Some(v),
                _ => None,
            })
            .collect();
        assert_eq!(values.len(), layout.slots.len());
    }

    #[test]
    fn equal_counts_and_pcs_do_not_authorize_another_layout() {
        let (air, header) = fixture();
        let a = Site::new(Layout::new(7, air.clone(), header).unwrap());
        let b = Site::new(Layout::new(7, air, header).unwrap());
        assert_eq!(a.layout.pc, b.layout.pc);
        assert_eq!(a.layout.slots, b.layout.slots);
        assert!(a.publish(&b.layout, 123).is_err());
        assert_eq!(a.target(), 0);
        assert!(a.publish(&a.layout, 0).is_err());
        a.publish(&a.layout, 123).unwrap();
        assert_eq!(a.target(), 123);
    }

    #[test]
    fn unsupported_snapshot_widths_are_refused() {
        let (mut air, header) = fixture();
        let ir = &mut Arc::get_mut(&mut air).unwrap().ir;
        let phi = ir.blocks[header.idx()].phis[0].dst;
        ir.values[phi.idx()].lanes = 4;
        assert!(Layout::new(7, air, header).is_err());
    }

    #[test]
    fn mutable_cells_are_inputs_not_aliased_ssa_registers() {
        let (mut air, header) = fixture();
        Arc::get_mut(&mut air)
            .unwrap()
            .ir
            .cells
            .push(air::v2::ir::CellData {
                reg: 0,
                ty: TypeRef(0),
                reason: air::v2::ir::PinReason::TrapWritten,
            });
        let layout = Layout::new(7, air, header).unwrap();
        assert_eq!(
            layout.slots.last().unwrap(),
            &Slot {
                input: Input::Cell(CellId(0)),
                ty: TypeRef(0)
            }
        );
        assert!(layout
            .slots
            .iter()
            .any(|s| matches!(s.input, Input::Value(_))));
    }
}
