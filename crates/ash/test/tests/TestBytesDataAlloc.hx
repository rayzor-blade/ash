// HashLink's Bytes.getData() creates a short-lived BytesDataImpl wrapper.
// Casting the result to hl.Bytes must recover its existing pointer without
// allocating one wrapper per call.
class TestBytesDataAlloc {
	static inline var CALLS = 8192;

	static function allocated(run:Void->Void):Float {
		var best = Math.POSITIVE_INFINITY;
		for (_ in 0...7) {
			var empty = hl.Gc.stats().totalAllocated;
			var reading = hl.Gc.stats().totalAllocated - empty;
			var before = hl.Gc.stats().totalAllocated;
			run();
			best = Math.min(best, hl.Gc.stats().totalAllocated - before - reading);
		}
		return Math.max(0, best);
	}

	static function main():Void {
		var bytes = haxe.io.Bytes.alloc(64);
		var cached:hl.Bytes = bytes.getData();
		var target = new hl.Bytes(64);
		var getter = () -> {
			for (i in 0...CALLS) {
				var raw:hl.Bytes = bytes.getData();
				if (raw != cached) throw "getData changed pointer identity";
				raw.setI32(0, i);
			}
		};
		var direct = () -> { for (i in 0...CALLS) cached.setI32(0, i); };
		var blit = () -> { for (i in 0...CALLS) target.blit(0, cached, 0, 64); };
		for (_ in 0...5) { getter(); direct(); blit(); }
		var getterBytes = allocated(getter);
		var cachedBytes = allocated(direct);
		var blitBytes = allocated(blit);
		if (cached.getI32(0) != CALLS - 1 || target.getI32(0) != CALLS - 1)
			throw "Byte aliasing or copy changed";
		var data = bytes.getData();
		if (data.bytes != cached || data.length != 64)
			throw "BytesData wrapper changed";
		var caught = false;
		var missing:haxe.io.Bytes = null;
		try {
			var raw:hl.Bytes = missing.getData();
			raw.setI32(0, 1);
		} catch (_:Dynamic) {
			caught = true;
		}
		if (!caught) throw "Null getData did not throw";
		Sys.println(haxe.Json.stringify({calls:CALLS, getData:getterBytes,
			cached:cachedBytes, blit:blitBytes}));
	}
}
