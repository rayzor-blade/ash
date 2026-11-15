typedef AshTastNativeApi = {
	function countOnGenerate():Void;
	function exportOnGenerate(path:String):Void;
	function exportAfterFilters(path:String):Void;
	function countAfterFilters():Void;
}

class AshTastNative {
	static var _plugin:AshTastNativeApi;

	static public function plugin():AshTastNativeApi {
		if (_plugin == null) {
			_plugin = eval.vm.Context.loadPlugin(pluginPath());
			// The typer drops metadata on an expression it does not know unless this is defined, and a
			// back end reads `@:ash.*` hints there. Init macros run after the typer exists, so a
			// macro cannot set it; the build has to.
			if (!haxe.macro.Context.defined("retain-untyped-meta"))
				haxe.macro.Context.warning("ash_tast: add -D retain-untyped-meta, or the typer drops @:ash.* metadata on expressions",
					haxe.macro.Context.currentPos());
		}
		return _plugin;
	}

	static public function init():Void
		plugin().countOnGenerate();

	/** The native counterpart of `ash.tast.Export.init`: writes the TAST file at `path`. */
	static public function export(path:String):Void
		plugin().exportOnGenerate(path);

	/** Writes the program as the HashLink generator receives it: after DCE and every type filter. */
	static public function exportFinal(path:String):Void
		plugin().exportAfterFilters(path);

	static public function countFinal():Void
		plugin().countAfterFilters();

	static function pluginPath():String {
		var file = (function(?p:haxe.PosInfos) return p.fileName)();
		var dir = haxe.io.Path.directory(haxe.io.Path.directory(file));
		return haxe.io.Path.join([dir, 'cmxs', Sys.systemName(), 'plugin.cmxs']);
	}
}
