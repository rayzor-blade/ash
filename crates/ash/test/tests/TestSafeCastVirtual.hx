// A value typed as an INTERFACE is a virtual: a wrapper around the object,
// with the object itself at offset 8. Casting it back to its class must
// unwrap the wrapper; a tier that copied the pointer instead applied the
// class's field offsets to the wrapper, read a method pointer where a field
// should be, and stored through it into code. Every cast below runs once,
// so only a whole-module compile ever executes it compiled -- which is why
// the parity rows include the JIT and the AOT smoke test includes this file.
interface Store {
	function get(key:String):Int;
}

class Entry {
	public var name:String;
	public var a:Int;
	public var b:Int;
	public var c:Int;
	public function new(name:String) {
		this.name = name;
		a = 1; b = 2; c = 3;
	}
}

interface Named {
	function label():String;
}

class LocalStore implements Store implements Named {
	// three fields BEFORE the one the test reads, so its offset is past the
	// wrapper's own header and lands in the virtual's field table when unwrapping is skipped
	public var x:Int;
	public var y:Int;
	public var z:Int;
	public var root:Entry;
	public var count:Int;
	public function new() {
		x = 10; y = 20; z = 30;
		root = new Entry("root");
		count = 0;
	}
	public function get(key:String):Int {
		count++;
		return key.length;
	}
	public function label():String return "local:" + root.name;
}

class OtherStore implements Store {
	public function new() {}
	public function get(key:String):Int return -1;
}

class TestSafeCastVirtual {
	static var store:Store;
	static var other:Store;

	static function main() {
		store = new LocalStore();
		other = new OtherStore();

		// checked cast interface -> class, then a field read and a field store
		var local:LocalStore = cast(store, LocalStore);
		trace("z=" + local.z);
		trace("root=" + local.root.name);
		local.root.name = "renamed";
		trace("root after store=" + cast(store, LocalStore).root.name);
		trace("count before=" + local.count + " get=" + store.get("abc") + " count after=" + local.count);

		// Std.downcast: same cast, null on mismatch
		var d = Std.downcast(store, LocalStore);
		trace("downcast=" + (d != null ? d.root.name : "null"));
		var miss = Std.downcast(other, LocalStore);
		trace("mismatch=" + (miss == null ? "null" : "object"));

		// a failing checked cast throws
		try {
			var bad:LocalStore = cast(other, LocalStore);
			trace("bad=" + bad.z);
		} catch (e:Dynamic) {
			trace("cast failed as expected");
		}

		// interface -> another interface the class implements (virtual -> virtual)
		var named:Named = cast(store, Named);
		trace("label=" + named.label());

		// unsafe cast of an interface value
		var u:LocalStore = cast store;
		trace("unsafe z=" + u.z + " y=" + u.y);

		// Std.isOfType through the interface
		trace("isOf=" + Std.isOfType(store, LocalStore) + "," + Std.isOfType(other, LocalStore));
	}
}
