typedef AshTastNativeApi = {
	function countOnGenerate():Void;
}

class AshTastNative {
	static var _plugin:AshTastNativeApi;

	static public function plugin():AshTastNativeApi {
		if (_plugin == null)
			_plugin = eval.vm.Context.loadPlugin(pluginPath());
		return _plugin;
	}

	static public function init():Void
		plugin().countOnGenerate();

	static function pluginPath():String {
		var file = (function(?p:haxe.PosInfos) return p.fileName)();
		var dir = haxe.io.Path.directory(haxe.io.Path.directory(file));
		return haxe.io.Path.join([dir, 'cmxs', Sys.systemName(), 'plugin.cmxs']);
	}
}
