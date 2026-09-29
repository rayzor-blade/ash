package ash.macro;

#if macro
import haxe.Json;
import haxe.io.Path;
import haxe.macro.Compiler;
import haxe.macro.Context;
import sys.FileSystem;
import sys.io.File;
import sys.io.Process;

/** Copy the bundled host HDLL beside HashLink bytecode; Ash ignores the copy. */
class NativeInstall {
	public static function stage():Void {
		if (!Context.defined("hl") || Context.defined("hlc") || Context.defined("ash_simd_no_hdll")) return;
		Context.onAfterGenerate(() -> {
			var platform = hostPlatform();
			var module = Context.resolvePath("ash/macro/NativeInstall.hx");
			var root = Path.directory(Path.directory(Path.directory(module)));
			var manifest:Dynamic = Json.parse(File.getContent(Path.join([root, "native/hdlls.json"])));
			var entry:Dynamic = Reflect.field(manifest, platform);
			if (entry == null) {
				Context.fatalError('ash-simd has no simd.hdll for $platform', Context.currentPos());
				return;
			}
			var source = Path.join([root, entry.packagePath]);
			if (!FileSystem.exists(source)) {
				Context.fatalError('ash-simd is missing $source; install the release Haxelib ZIP', Context.currentPos());
				return;
			}
			var output = Compiler.getOutput();
			var destination = Path.join([Path.directory(output), "simd.hdll"]);
			if (Path.normalize(source) != Path.normalize(destination)) File.copy(source, destination);
		});
	}

	static function hostPlatform():String {
		var os = switch (Sys.systemName()) {
			case "Linux": "linux";
			case "Mac": "macos";
			case "Windows": "windows";
			case other: other.toLowerCase();
		};
		var arch:String;
		if (os == "windows") {
			arch = Sys.getEnv("PROCESSOR_ARCHITEW6432");
			if (arch == null) arch = Sys.getEnv("PROCESSOR_ARCHITECTURE");
		} else {
			var process = new Process("uname", ["-m"]);
			arch = StringTools.trim(process.stdout.readAll().toString());
			var status = process.exitCode();
			process.close();
			if (status != 0) throw "ash-simd could not detect the CPU architecture";
		}
		arch = switch (arch == null ? "" : arch.toLowerCase()) {
			case "amd64", "x86_64": "x86_64";
			case "arm64", "aarch64": "aarch64";
			case other: other;
		};
		return '$os-$arch';
	}
}
#end
