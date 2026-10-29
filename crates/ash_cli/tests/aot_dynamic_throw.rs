//! Linux's DWARF walker must cross the x86-64 System V dynamic-call bridge.
#![cfg(all(feature = "llvm", target_os = "linux", target_arch = "x86_64"))]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout};
use std::process::{Command, Output};
use std::time::Duration;

fn run(command: Command) -> Output {
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    result.output
}

#[test]
fn reflected_throws_keep_the_caller_in_the_exception_stack() {
    if !haxe_available() {
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("dynamic-throw.hl");
    std::fs::write(
        dir.path().join("DynamicThrow.hx"),
        r#"
class DynamicThrow {
    @:keep public static var field(get, set):Int;
    static function get_field():Int throw "getter";
    static function set_field(v:Int):Int throw "setter";

    @:keep static function many(a:Int, b:Int, c:Int, d:Int, e:Int,
                              f:Int, g:Int, h:Int, i:Int):Int {
        return a + 10*b + 100*c + 1000*d + 10000*e + 100000*f
            + 1000000*g + 10000000*h + 100000000*i;
    }
    @:keep static function throwMany(a:Int, b:Int, c:Int, d:Int, e:Int,
                                   f:Int, g:Int, h:Int, i:Int):Int {
        if (many(a,b,c,d,e,f,g,h,i) != 987654321) throw "bad stack arguments";
        throw "stack arguments";
    }
    @:keep static function mixed(i:Int, a:Float, f:hl.F32, j:Int,
                               b:Float, g:hl.F32):Float return i+a+f+j+b+g;
    @:keep static function small(a:hl.F32, b:hl.F32):hl.F32 return a+b;

    @:neverInline static function check(expected:String, action:Void->Void) {
        try {
            action();
        } catch (e:Dynamic) {
            if (e != expected) throw e;
            // A walk stopped at the bridge loses the Haxe caller, even when
            // the bogus return address happens to be zero instead of crashing.
            if (haxe.CallStack.exceptionStack().length < 2) throw "missing caller";
            Sys.println(expected + " caught");
            return;
        }
        throw "did not throw";
    }
    static function main() {
        check("getter", function() { Reflect.getProperty(DynamicThrow, "field"); });
        check("setter", function() { Reflect.setProperty(DynamicThrow, "field", 123); });
        var args:Array<Dynamic> = [1,2,3,4,5,6,7,8,9];
        check("stack arguments", function() {
            Reflect.callMethod(DynamicThrow, Reflect.field(DynamicThrow, "throwMany"), args);
        });
        if (Reflect.callMethod(DynamicThrow, Reflect.field(DynamicThrow, "many"), args)
                != 987654321) throw "bad integer result";
        if (Reflect.callMethod(DynamicThrow, Reflect.field(DynamicThrow, "mixed"),
                [1, 2.5, 3.25, 4, 5.5, 6.25]) != 22.5) throw "bad mixed result";
        if (Reflect.callMethod(DynamicThrow, Reflect.field(DynamicThrow, "small"),
                [1.25, 2.5]) != 3.75) throw "bad f32 result";
        Sys.println("results ok");
    }
}
"#,
    )
    .unwrap();
    let mut haxe = Command::new("haxe");
    haxe.arg("-cp")
        .arg(dir.path())
        .args(["-main", "DynamicThrow", "-hl"])
        .arg(&hl);
    run(haxe);

    for linkage in ["static", "dynamic"] {
        let exe = dir.path().join(format!("dynamic-throw-{linkage}"));
        let mut build = Command::new(ash_cli_bin());
        build
            .arg("--build")
            .arg(&exe)
            .arg(&hl)
            .env("ASH_STD_LINKAGE", linkage)
            .env("ASH_LIBHL", "embedded")
            .env_remove("ASH_RUNTIME");
        run(build);
        let result = run(Command::new(exe));
        assert_eq!(
            String::from_utf8_lossy(&result.stdout).replace("\r\n", "\n"),
            "getter caught\nsetter caught\nstack arguments caught\nresults ok\n",
            "{linkage}: {}",
            render_output(&result)
        );
    }
}
