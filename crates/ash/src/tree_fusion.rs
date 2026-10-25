//! Bounded standalone preparation. The compiler may eliminate unreachable,
//! confined objects under Ash's allocation-elision convention: allocation
//! counts, collection/finalizer scheduling, resource exhaustion and profiling
//! timing are not semantic barriers. Value results, observable memory effects
//! and ordinary exceptions remain barriers. No host policy is inferred here.
use std::borrow::Cow;
use std::collections::BTreeMap;

use air::v2::tree_fusion::{self, Limits, TreeModule};
use anyhow::{Result, ensure};

use crate::air_pipeline::AshModule;
use crate::bytecode::DecodedBytecode;
use crate::generated::{self, Declaration, Key};
use crate::opcodes::Opcode;
use crate::types::TypeRef;

// Module-wide bounds supplement per-proof and generated-code limits.
const MAX_FUNCTIONS: usize = 4096;
const MAX_SURVEYS: usize = 128;
const MAX_CALLER_AIR: usize = 8 * 1024 * 1024;

static DISABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Turn tree fusion off for this process, for the `--no-tree-fusion` switch.
pub fn disable() {
    DISABLED.store(true, std::sync::atomic::Ordering::Relaxed);
}

/// Whether [`disable`] has not been called.
pub fn enabled() -> bool {
    !DISABLED.load(std::sync::atomic::Ordering::Relaxed)
}

/// Only the standalone CLI before guest execution may establish this contract.
/// The linked Ash runtime has no replaceable allocator/plugin loader; native
/// host code, registration hooks, and reload would void that assumption.
/// Hosts must not call this while another program/runtime is active.
/// No-candidate and refused transactions borrow the original module; callers
/// can retain it without cloning metadata. Contract failures return an error.
pub fn prepare_standalone(
    input: &DecodedBytecode,
) -> Result<(Cow<'_, DecodedBytecode>, Vec<String>)> {
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

fn prepare(input: &DecodedBytecode) -> Result<(Cow<'_, DecodedBytecode>, Vec<String>)> {
    prepare_bounded(input, MAX_SURVEYS, MAX_CALLER_AIR)
}

fn prepare_bounded(
    input: &DecodedBytecode,
    max_surveys: usize,
    max_caller_air: usize,
) -> Result<(Cow<'_, DecodedBytecode>, Vec<String>)> {
    let module = AshModule::for_tree_fusion(input);
    let limits = Limits::default();
    let mut declarations = BTreeMap::new();
    let mut callers = Vec::new();
    let mut report =
        vec!["standalone allocation-elision contract; no hot reload or serialization".into()];
    let mut skipped = 0;
    let mut surveyed = 0;
    let mut caller_air = 0;
    let mut module_truncated = input.functions.len() > MAX_FUNCTIONS;
    let mut tree_types = BTreeMap::new();
    for (fi, f) in input.functions.iter().enumerate().take(MAX_FUNCTIONS) {
        if f.op_count() > limits.body_ops || f.regs.len() > limits.body_ops * 8 {
            skipped += 1;
            continue;
        }
        // Cheap necessary condition: a direct call returning a recursive
        // object. Avoid lowering ordinary numeric/dispatch workloads at startup.
        let possible = f.ops().iter().any(|op| {
            let dst = match op {
                Opcode::Call0 { dst, .. }
                | Opcode::Call1 { dst, .. }
                | Opcode::Call2 { dst, .. }
                | Opcode::Call3 { dst, .. }
                | Opcode::Call4 { dst, .. }
                | Opcode::CallN { dst, .. } => dst,
                _ => return false,
            };
            let Some(ty) = f.regs.get(dst.0 as usize) else {
                return false;
            };
            *tree_types.entry(ty.0).or_insert_with(|| {
                let ty = air::v2::ir::TypeRef(ty.0 as u32);
                module
                    .plain_fields(ty)
                    .is_some_and(|fields| fields.contains(&ty))
            })
        });
        if !possible {
            continue;
        }
        if surveyed == max_surveys {
            module_truncated = true;
            break;
        }
        surveyed += 1;
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
            caller_air += body.allocated_bytes();
            if caller_air > max_caller_air
                || declarations.len() > generated::Limits::default().helpers
            {
                report
                    .push("module proof/helper budget exceeded; original program retained".into());
                return Ok((Cow::Borrowed(input), report));
            }
            callers.push((fi, body));
        }
    }
    report.push(format!("surveyed_bodies={surveyed} retained_caller_air_bytes={caller_air} module_truncated={module_truncated}"));
    if declarations.is_empty() {
        report.push(format!("transformed_sites=0 helpers=0 skipped_bodies={skipped} module_truncated={module_truncated}"));
        return Ok((Cow::Borrowed(input), report));
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
                module_truncated
            ));
            Ok((Cow::Owned(output), report))
        }
        Err(e) => {
            report.push(format!(
                "transaction refused: {e:#}; original program retained"
            ));
            Ok((Cow::Borrowed(input), report))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::BytecodeDecoder;

    #[test]
    fn active_host_object_policy_is_refused_in_fresh_process() {
        const CHILD: &str = "ASH_TEST_TREE_FUSION_HOST_POLICY";
        let Ok(policy) = std::env::var(CHILD) else {
            // These registrations are process-wide and intentionally cannot
            // be reset. Never contaminate the other compiler/runtime tests.
            for policy in ["room", "tails", "drops"] {
                let out = std::process::Command::new(std::env::current_exe().unwrap())
                    .args([
                        "--exact",
                        "tree_fusion::tests::active_host_object_policy_is_refused_in_fresh_process",
                        "--nocapture",
                    ])
                    .env(CHILD, policy)
                    .output()
                    .unwrap();
                assert!(
                    out.status.success(),
                    "{policy}: {} {}",
                    String::from_utf8_lossy(&out.stdout),
                    String::from_utf8_lossy(&out.stderr)
                );
            }
            return;
        };
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("test/tests/bench_binary_trees.hl");
        crate::native_lib::choose_std_linkage(&path);
        crate::native_lib::init_std_library().unwrap();
        let bc = BytecodeDecoder::decode(&path).unwrap();
        assert!(!ash_std::obj::has_host_object_policy());
        // Only the descriptor's address is registered; no guest object is
        // allocated or callback invoked in these refusal tests.
        let mut ty: ash_std::hl::hl_type = unsafe { std::mem::zeroed() };
        match policy.as_str() {
            "room" => assert!(ash_std::obj::hlp_set_object_room(16)),
            "tails" => unsafe {
                ash_std::obj::hlp_set_object_tails(
                    &ash_std::obj::ObjectTail {
                        t: &mut ty,
                        bytes: 8,
                    },
                    1,
                );
            },
            "drops" => unsafe {
                ash_std::obj::hlp_set_object_drops(&(&mut ty as *mut _), 1);
            },
            _ => panic!("unknown policy"),
        }
        let error = prepare_standalone(&bc)
            .err()
            .expect("active host policy must refuse");
        assert!(
            error.to_string().contains("active host object policy"),
            "{error}"
        );
    }

    #[test]
    fn standalone_contract_rejects_host_metadata_external_natives_and_registration_hooks() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("test/tests/bench_binary_trees.hl");
        crate::native_lib::choose_std_linkage(&path);
        crate::native_lib::init_std_library().unwrap();
        let bc = BytecodeDecoder::decode(&path).unwrap();
        let mut hosted = bc.clone();
        hosted
            .host_modules
            .push(crate::host_module::RegisteredModule(
                crate::host_module::HostModule {
                    lib: "test".into(),
                    classes: vec![],
                },
            ));
        assert!(
            prepare_standalone(&hosted)
                .err()
                .expect("policy must refuse")
                .to_string()
                .contains("hosted modules")
        );
        let mut overridden = bc.clone();
        overridden.host_natives.insert(
            ("std".into(), "gc_major".into()),
            crate::native_lib::HostNative::plain(0),
        );
        assert!(
            prepare_standalone(&overridden)
                .err()
                .expect("policy must refuse")
                .to_string()
                .contains("hosted modules")
        );
        for (lib, name, message) in [
            (
                "external",
                "pure_looking_function",
                "external native libraries",
            ),
            ("std", "set_object_tails", "registration hooks"),
            ("std", "set_object_drops", "registration hooks"),
            ("std", "set_object_room", "registration hooks"),
            ("std", "set_reload_callback", "registration hooks"),
            ("std", "set_reload_apply", "registration hooks"),
            ("std", "setup_reload_check", "registration hooks"),
        ] {
            let mut input = bc.clone();
            input.natives[0].lib = lib.into();
            input.natives[0].name = name.into();
            assert!(
                prepare_standalone(&input)
                    .err()
                    .expect("policy must refuse")
                    .to_string()
                    .contains(message),
                "{lib}/{name}"
            );
            assert!(input.generated_helpers.is_empty());
        }
        // The proof view remains usable when ordinary inliner bodies are
        // withheld. Runtime permissions still belong to that explicit view.
        let module = AshModule::for_tree_fusion(&bc).without_callees();
        assert!(air::v2::module::ModuleInfo::callee(&module, 24).is_none());
        assert!(tree_fusion::analyze_pair(&module, 24, 23, Limits::default()).is_ok());
        assert!(
            tree_fusion::build_helper(&AshModule::new(&bc), 24, 23, 999, Limits::default())
                .is_err()
        );
    }

    #[test]
    fn non_tree_program_keeps_original_module_without_metadata_clone() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("test/tests/bench_fib.hl");
        crate::native_lib::choose_std_linkage(&path);
        crate::native_lib::init_std_library().unwrap();
        let bc = BytecodeDecoder::decode(&path).unwrap();
        let (prepared, report) = prepare(&bc).unwrap();
        assert!(matches!(prepared, Cow::Borrowed(_)));
        assert!(
            report
                .iter()
                .any(|line| line.contains("retained_caller_air_bytes=0"))
        );
        assert!(prepared.generated_helpers.is_empty());
    }

    #[test]
    fn module_budgets_retain_original_program() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("test/tests/bench_binary_trees.hl");
        crate::native_lib::choose_std_linkage(&path);
        crate::native_lib::init_std_library().unwrap();
        let bc = BytecodeDecoder::decode(&path).unwrap();
        let old = crate::bytecode_encode::encode(&bc, 5).unwrap();
        for (surveys, bytes, reason) in [
            (0, MAX_CALLER_AIR, "module_truncated=true"),
            (MAX_SURVEYS, 0, "module proof/helper budget exceeded"),
        ] {
            let (fallback, report) = prepare_bounded(&bc, surveys, bytes).unwrap();
            assert!(
                report.iter().any(|line| line.contains(reason)),
                "{report:?}"
            );
            assert!(matches!(fallback, Cow::Borrowed(_)));
            assert!(fallback.generated_helpers.is_empty());
            assert!(!fallback.tree_fusion_runtime);
            assert_eq!(old, crate::bytecode_encode::encode(&fallback, 5).unwrap());
        }
    }

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
