// Address-taken locals (`hl.Ref`) through the AIR v2 cell machinery.
//
// Lowering pins a `Ref`-taken register to a memory cell, and CellForwarding
// then forwards `CellSet -> CellGet` pairs. MBHaxe (marblegame.hl) reached
// two defects in that pass that no other corpus program did, because its hot
// math helpers pass `hl.Ref` out-parameters around:
//
//   1. the forwarded `CellGet` was deleted without compacting the value
//      table, so the verifier's "every value is defined" totality check
//      refused the function ("value vN is never defined") whenever DCE had
//      nothing else to trigger a compaction — i.e. exactly in small,
//      straight-line functions like these;
//   2. a forward whose target was itself a forwarded load (a `x = y` move
//      between two pinned locals, once GVN folds the Copy) left uses of a
//      deleted definition ("value vN is used after its definition was
//      removed").
//
// Each function below is one opcode shape from the marblegame refusal list;
// the printed values also check the cells still read/write correctly.
class TestRefCells {
	var flag:Bool;
	var scale:Float;

	function new() {
		flag = false;
		scale = 3.0;
	}

	// marblegame f310 "init": field -> pinned local, in-place `Not`
	// (dst == src), address taken, then a call. The Not's CellGet is the
	// forwarded load; nothing else in the function is dead.
	function notInPlace():Bool {
		var b = flag;
		b = !b;
		toggle(hl.Ref.make(b));
		return b;
	}

	function toggle(r:hl.Ref<Bool>) {
		r.set(!r.get());
	}

	// marblegame f6227 "initFromScene": a `x = y` move between two pinned
	// float locals plus read-modify-write through them, feeding Ref
	// out-params. After GVN folds the Copy, the second forward's target is
	// itself a forwarded load — the chain case.
	function chain():Float {
		var x = 1.0;
		var y = 1.0;
		x = y;
		x = x * scale;
		y = y * 0.5;
		addBoth(hl.Ref.make(x), hl.Ref.make(y));
		return x + y;
	}

	function addBoth(a:hl.Ref<Float>, b:hl.Ref<Float>) {
		a.set(a.get() + 0.5);
		b.set(b.get() + 0.25);
	}

	// marblegame f8767: Ref-pinned locals in a function that also has a
	// trap region (everything written inside the try is pinned too).
	function refWithTrap():Float {
		var v = scale;
		v = v * 2.0;
		try {
			addBoth(hl.Ref.make(v), hl.Ref.make(v));
		} catch (e:Dynamic) {
			v = -1.0;
		}
		return v;
	}

	// marblegame f6043 "sample": an `Incr` loop counter beside an
	// address-taken accumulator that is read-modified-written per iteration.
	function refWithLoop():Float {
		var acc = 0.0;
		for (i in 0...4) {
			acc = acc + i;
			addBoth(hl.Ref.make(acc), hl.Ref.make(acc));
		}
		return acc;
	}

	static function main() {
		var t = new TestRefCells();
		Sys.println("not=" + t.notInPlace());
		Sys.println("chain=" + t.chain());
		Sys.println("trap=" + t.refWithTrap());
		Sys.println("loop=" + t.refWithLoop());
	}
}
