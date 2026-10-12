//! Opt-in standalone preparation. The compiler may eliminate unreachable,
//! confined objects under Ash's allocation-elision convention: allocation
//! counts, collection/finalizer scheduling, resource exhaustion and profiling
//! timing are not semantic barriers. Value results, observable memory effects
//! and ordinary exceptions remain barriers. No host policy is inferred here.
use std::collections::BTreeMap;

use air::v2::tree_fusion::{self, Limits, TreeModule};
use anyhow::{Result, ensure};

use crate::air_pipeline::AshModule;
use crate::bytecode::DecodedBytecode;
use crate::generated::{self, Declaration, Key};
use crate::types::TypeRef;

/// Only the standalone CLI before guest execution may establish this contract.
/// The linked Ash runtime has no replaceable allocator/plugin loader; native
/// host code, registration hooks, and reload would void that assumption.
/// Hosts must not call this while another program/runtime is active.
pub fn prepare_standalone(input: &DecodedBytecode) -> Result<(DecodedBytecode, Vec<String>)> {
    ensure!(
        crate::native_lib::std_is_static(),
        "tree fusion requires the linked standalone Ash runtime"
    );
    ensure!(
        !ash_std::obj::has_host_object_policy(),
        "tree fusion refuses active host object policy"
    );
    ensure!(
        input.host_modules.is_empty()
            && input.host_classes.is_empty()
            && input.host_natives.is_empty(),
        "tree fusion refuses hosted modules"
    );
    ensure!(
        input
            .natives
            .iter()
            .all(|n| crate::native_lib::is_runtime_lib(&n.lib)),
        "tree fusion refuses external native libraries"
    );
    ensure!(
        input.natives.iter().all(|n| crate::native_lib::host_native(
            n.lib.strip_prefix('?').unwrap_or(&n.lib),
            &n.name
        )
        .is_none()),
        "tree fusion refuses host native overrides"
    );
    ensure!(
        !input.natives.iter().any(|n| matches!(
            n.name.as_str(),
            "set_object_tails"
                | "set_object_drops"
                | "set_object_room"
                | "set_reload_callback"
                | "set_reload_apply"
                | "setup_reload_check"
        )),
        "tree fusion refuses runtime registration hooks"
    );
    prepare(input)
}

fn prepare(input: &DecodedBytecode) -> Result<(DecodedBytecode, Vec<String>)> {
    let module = AshModule::for_tree_fusion(input);
    let limits = Limits::default();
    let mut declarations = BTreeMap::new();
    let mut callers = Vec::new();
    let mut report = vec![
        "prototype; standalone allocation-elision contract; no hot reload or serialization".into(),
    ];
    let mut skipped = 0;
    for (fi, f) in input.functions.iter().enumerate().take(4096) {
        if f.op_count() > limits.body_ops || f.regs.len() > limits.body_ops * 8 {
            skipped += 1;
            continue;
        }
        let Some(body) = module.lowered_body(f.findex as usize) else {
            skipped += 1;
            continue;
        };
        let survey = tree_fusion::survey(&module, &body, limits);
        let mut candidate = false;
        if survey.truncated {
            report.push(format!(
                "caller={} refused: survey budget exceeded",
                f.findex
            ));
            continue;
        }
        for site in survey.sites {
            if site.shape.is_some() && site.blockers.is_empty() {
                let consumer = site.consumer.expect("proved fold");
                let (args, _) = module.signature(site.producer).expect("proved signature");
                let (_, ret) = module.signature(consumer).expect("proved signature");
                declarations
                    .entry((site.producer, consumer))
                    .or_insert_with(|| Declaration {
                        key: Key {
                            pass: "tree-fusion".into(),
                            version: 1,
                            sources: vec![site.producer, consumer],
                            specialization: vec![1],
                        },
                        args: args.iter().map(|t| t.0 as usize).collect(),
                        ret: ret.0 as usize,
                    });
                candidate = true;
            } else {
                report.push(format!(
                    "caller={} producer={} consumer={:?} refused: {}",
                    f.findex,
                    site.producer,
                    site.consumer,
                    site.blockers.join("; ")
                ));
            }
        }
        if candidate {
            callers.push((fi, body));
        }
    }
    let transaction = (|| -> Result<(DecodedBytecode, usize)> {
        let mut output = generated::prepare(
            input,
            declarations.into_values(),
            generated::Limits::default(),
            |d, ids| {
                tree_fusion::build_helper(
                    &module,
                    d.key.sources[0],
                    d.key.sources[1],
                    ids[&d.key],
                    limits,
                )
                .map_err(anyhow::Error::msg)
            },
        )?;
        let helpers: BTreeMap<_, _> = output
            .generated_helpers
            .iter()
            .map(|h| {
                (
                    (h.declaration.key.sources[0], h.declaration.key.sources[1]),
                    h.findex,
                )
            })
            .collect();
        let mut count = 0;
        for (fi, body) in callers {
            let (body, n) = tree_fusion::rewrite_caller(&module, &body, &helpers, limits)
                .map_err(anyhow::Error::msg)?;
            if n == 0 {
                continue;
            }
            let mut ser = air::v2::serialize::serialize_with_int_base(&body, input.ints.len())?;
            let offset = output.ints.len() - input.ints.len();
            for op in &mut ser.ops {
                if let crate::opcodes::Opcode::Int { ptr, .. } = op
                    && ptr.0 >= input.ints.len()
                {
                    ptr.0 += offset;
                }
            }
            let debug = generated::source_positions(&body, &ser);
            output.ints.extend(ser.new_ints);
            let f = &mut output.functions[fi];
            f.set_ops(ser.ops);
            f.set_debug(debug);
            f.regs = ser
                .reg_types
                .iter()
                .map(|t| TypeRef(t.0 as usize))
                .collect();
            count += n;
        }
        output.body_facts.take();
        output.tree_fusion_runtime = count > 0;
        Ok((output, count))
    })();
    match transaction {
        Ok((output, count)) => {
            for h in &output.generated_helpers {
                let f = output
                    .functions
                    .iter()
                    .find(|f| f.findex as usize == h.findex)
                    .unwrap();
                report.push(format!(
                    "helper={} producer={} consumer={} ops={} regs={} node_allocations={}",
                    h.findex,
                    h.declaration.key.sources[0],
                    h.declaration.key.sources[1],
                    f.op_count(),
                    f.regs.len(),
                    f.ops()
                        .iter()
                        .filter(|op| matches!(op, crate::opcodes::Opcode::New { .. }))
                        .count()
                ));
            }
            report.push(format!(
                "transformed_sites={count} helpers={} skipped_bodies={skipped} module_truncated={}",
                output.generated_helpers.len(),
                input.functions.len() > 4096
            ));
            Ok((output, report))
        }
        Err(e) => {
            report.push(format!(
                "transaction refused: {e:#}; original program retained"
            ));
            Ok((input.clone(), report))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::BytecodeDecoder;

    #[test]
    fn preparation_is_atomic_and_runtime_contract_does_not_escape_serialization() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("test/tests/bench_binary_trees.hl");
        crate::native_lib::choose_std_linkage(&path);
        crate::native_lib::init_std_library().unwrap();
        let bc = BytecodeDecoder::decode(&path).unwrap();
        let old = crate::bytecode_encode::encode(&bc, 5).unwrap();
        let (fused, report) = prepare(&bc).unwrap();
        assert!(
            report
                .last()
                .unwrap()
                .contains("transformed_sites=3 helpers=1")
        );
        assert_eq!(fused.generated_helpers.len(), 1);
        let helper = fused
            .functions
            .iter()
            .find(|f| f.findex as usize == fused.generated_helpers[0].findex)
            .unwrap();
        assert!(
            !helper
                .ops()
                .iter()
                .any(|o| matches!(o, crate::opcodes::Opcode::New { .. }))
        );
        assert!(
            crate::bytecode_encode::encode(&fused, 5)
                .unwrap_err()
                .to_string()
                .contains("cannot be serialized")
        );
        assert!(!crate::reload::diff_bytecode(&fused, &bc).is_safe());
        assert_eq!(old, crate::bytecode_encode::encode(&bc, 5).unwrap());
        let mut malformed = bc.clone();
        // Leave the proven producer/consumer/constructor intact, then make
        // registration fail after discovery by breaking an unrelated ID.
        malformed
            .functions
            .iter_mut()
            .find(|f| f.findex > 25)
            .unwrap()
            .findex = -1;
        let before = malformed.compute_function_hashes();
        let (fallback, report) = prepare(&malformed).unwrap();
        assert!(report.last().unwrap().contains("original program retained"));
        assert!(fallback.generated_helpers.is_empty());
        assert_eq!(before, fallback.compute_function_hashes());
    }
}
