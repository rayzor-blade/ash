import sys.thread.Lock;
import sys.thread.Thread;

private class Cell {
    public var next:Cell;
    public var value:Int;
    public function new(next:Cell, value:Int) { this.next = next; this.value = value; }
    public function fold():Int { return next == null ? value : value + next.fold(); }
}
class TestTreeFusionSuspended {
    static function chain(value:Int, depth:Int):Cell {
        if (depth > 0) return new Cell(chain(value + 1, depth - 1), value);
        return new Cell(null, value);
    }
    static function main() {
        var ready = new Lock();
        var proceed = new Lock();
        var done = new Lock();
        var result = 0;
        Thread.create(() -> {
            var keep = chain(99, 8);
            var expected = keep.fold();
            ready.release();
            proceed.wait();
            hl.Gc.major();
            result = chain(7, 5).fold();
            if (keep.fold() != expected || expected != 927) throw 'suspended root lost';
            done.release();
        });
        if (!ready.wait(5)) throw 'worker did not suspend';
        hl.Gc.major();
        proceed.release();
        if (!done.wait(5)) throw 'worker did not resume';
        if (result != 57) throw 'wrong fused worker result';
        Sys.println('tree-suspended=$result');
    }
}
