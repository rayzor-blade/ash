// A nongeneric interface call must use the virtual method slot without
// allocating an argument array or boxing its Int argument. Bound method
// extraction is an intentional allocation and serves as a control.
class TestInterfaceCallAlloc {
	static var held:Int->Void;
	static inline var CALLS = 8192;
	static inline var WINDOWS = 7;

	static function allocated(run:Void->Void):Float {
		var best = Math.POSITIVE_INFINITY;
		for (_ in 0...WINDOWS) {
			var empty = hl.Gc.stats().totalAllocated;
			var reading = hl.Gc.stats().totalAllocated - empty;
			var before = hl.Gc.stats().totalAllocated;
			run();
			best = Math.min(best, hl.Gc.stats().totalAllocated - before - reading);
		}
		return Math.max(0, best);
	}

	static function main():Void {
		var concrete = new AllocCounter();
		var base:AllocBase = concrete;
		var viaInterface:AllocInterface = concrete;
		for (_ in 0...512) {
			concrete.tick(1);
			base.tick(1);
			viaInterface.tick(1);
			hl.Gc.stats();
		}
		var cached = viaInterface.tick;
		var direct = allocated(() -> { for (_ in 0...CALLS) concrete.tick(1); });
		var baseCall = allocated(() -> { for (_ in 0...CALLS) base.tick(1); });
		var interfaceCall = allocated(() -> { for (_ in 0...CALLS) viaInterface.tick(1); });
		var readMethod = allocated(() -> { for (_ in 0...CALLS) held = viaInterface.tick; });
		var cachedCall = allocated(() -> { for (_ in 0...CALLS) cached(1); });
		var total = concrete.total;
		if (total != 230912 || held == null) throw "Interface dispatch changed";

		// The receiver may change after the caller compiled. The slot must be
		// loaded from each current view rather than captured at compile time.
		var other = new AllocOther();
		var second:AllocInterface = other;
		var current:AllocInterface = viaInterface;
		var switched = allocated(() -> {
			for (i in 0...CALLS) {
				current = i % 2 == 0 ? viaInterface : second;
				current.tick(1);
			}
		});
		if (concrete.total != total + 28672 || other.total != 57344)
			throw "Mutable receiver dispatch changed";
		var dynamicTotal = 0;
		var dynamicObject:Dynamic = {tick: function(amount:Int) dynamicTotal += amount};
		var dynamicView:AllocInterface = dynamicObject;
		dynamicView.tick(7);
		if (dynamicTotal != 7) throw "Dynamic object dispatch changed";
		Sys.println(haxe.Json.stringify({calls:CALLS, direct:direct, base:baseCall,
			viaInterface:interfaceCall, readMethod:readMethod,
			cachedCall:cachedCall, switched:switched, dynamicTotal:dynamicTotal, total:total}));
	}
}

private interface AllocInterface {
	function tick(amount:Int):Void;
}

private class AllocBase {
	public var total:Int = 0;
	public function new() {}
	public function tick(amount:Int):Void total += amount;
}

private class AllocCounter extends AllocBase implements AllocInterface {}

private class AllocOther implements AllocInterface {
	public var total:Int = 0;
	public function new() {}
	public function tick(amount:Int):Void total += amount * 2;
}
