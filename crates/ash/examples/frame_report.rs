//! How big would an interpreter frame be if slots were reused?
//!
//! The AIR walker gives every SSA value its own frame slot, and a frame is
//! cleared and refilled on every call, so the slot count is per-call cost on
//! the hottest path there is. Values whose live ranges do not overlap could
//! share a slot. This reports, per function, the slots used now against the
//! peak number simultaneously live -- the floor any reuse scheme would aim
//! for -- so the saving is known before a pass is written for it.
//!
//! Usage: frame_report <file.hl>

use air::v2::analysis::CfgInfo;
use air::v2::ir::BlockId;
use air::v2::liveness::Liveness;
use ash_core::air_pipeline::{optimized, AshModule};

fn main() {
    ash_core::native_lib::init_std_library().expect("init std");
    let Some(path) = std::env::args().nth(1) else {
        eprintln!("usage: frame_report <file.hl>");
        std::process::exit(2);
    };
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))
        .expect("decode bytecode");
    let m = AshModule::new(&bc);

    let (mut slots_now, mut slots_peak, mut counted) = (0usize, 0usize, 0usize);
    let mut worst: Vec<(usize, usize, usize)> = Vec::new();
    for f in &bc.functions {
        let Ok(o) = optimized(&m, f) else { continue };
        let ir = &o.ir;
        if ir.values.is_empty() {
            continue;
        }
        let cfg = CfgInfo::build(ir);
        let live = Liveness::analyze(ir, &cfg);
        // Peak pressure: live-in plus everything the block defines, which is
        // the most that can be simultaneously live inside it.
        let mut peak = 0usize;
        for (b, blk) in ir.blocks.iter().enumerate() {
            let mut n = live.live_in(BlockId(b as u32)).len();
            n += blk.phis.len();
            n += blk.instrs.iter().filter(|i| i.dst().is_some()).count();
            peak = peak.max(n);
        }
        let now = ir.values.len() + ir.cells.len();
        slots_now += now;
        slots_peak += peak + ir.cells.len();
        counted += 1;
        worst.push((
            now.saturating_sub(peak + ir.cells.len()),
            now,
            peak + ir.cells.len(),
        ));
    }

    worst.sort_by_key(|&(saving, _, _)| std::cmp::Reverse(saving));
    println!("{counted} functions");
    println!("slots today:      {slots_now}");
    println!("peak live + cells:{slots_peak}");
    if slots_now > 0 {
        println!(
            "reusable:         {} ({:.1}%)",
            slots_now - slots_peak,
            100.0 * (slots_now - slots_peak) as f64 / slots_now as f64
        );
    }
    println!("\nlargest savings (slots_now -> floor):");
    for (saving, now, peak) in worst.iter().take(8) {
        println!("  {now:5} -> {peak:5}   saves {saving}");
    }
}
