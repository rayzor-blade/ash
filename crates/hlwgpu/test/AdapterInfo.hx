/**
	Asks the machine what GPU it has. The whole of milestone 1: the library
	loads, a handle survives a round trip, a request is polled to completion,
	and a string comes back out of the library's heap.
**/
class AdapterInfo {
	static function main() {
		var instance = wgpu.Instance.create();
		if (!instance.ok) {
			Sys.println("no WebGPU on this machine");
			Sys.exit(1);
		}

		var adapter = instance.adapter();
		if (!adapter.ok) {
			Sys.println("no adapter matched");
			Sys.exit(1);
		}

		Sys.println("adapter:  " + adapter.name);
		Sys.println("backend:  " + adapter.backend.toString());
		Sys.println("max 2D:   " + adapter.limit(MaxTextureDimension2D));
		Sys.println("bindings: " + adapter.limit(MaxBindGroups));

		adapter.destroy();
		instance.destroy();
		Sys.println("destroyed cleanly");
	}
}
