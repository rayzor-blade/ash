private class Link {
    public var value:Int;
    public var next:Link;
    public function new(value:Int, next:Link) { this.value = value; this.next = next; }
    public function fold():Int {
        if (next == null) return -value;
        return (next.fold() ^ value) + 17;
    }
}

private class Branch {
    public var third:Branch;
    public var value:Int;
    public var first:Branch;
    public var second:Branch;
    public function new(value:Int, a:Branch, b:Branch, c:Branch) {
        this.third = c; this.value = -value; this.first = a; this.second = b;
    }
    public function fold():Int {
        if (second == null) return value;
        // Consume in a different order than construction, with noncommutative arithmetic.
        return (third.fold() - first.fold()) ^ (value | second.fold());
    }
}

class TestTreeFusionMatrix {
    static var savedLink:Link;
    static var savedBranch:Branch;
    static function chain(seed:Int, depth:Int):Link {
        if (0 < depth) return new Link(seed, chain(seed * 1664525 + 1013904223, depth - 1));
        return new Link(seed, null);
    }
    static function tree(depth:Int, seed:Int):Branch {
        if (depth <= 0) return new Branch(seed, null, null, null);
        return new Branch(seed, tree(depth - 1, seed + 7), tree(depth - 1, seed * 3), tree(depth - 1, seed - 11));
    }
    static inline function inlineFold(seed:Int, depth:Int):Int { return chain(seed, depth).fold(); }
    static function cases():Int {
        var seed = 0x80000000;
        var check = 0;
        for (i in 0...128) {
            var depth = i % 6 - 1;
            savedLink = chain(seed, depth);
            var expectedLink = savedLink.fold();
            var actualLink = inlineFold(seed, depth);
            if (actualLink != expectedLink) throw 'link mismatch $i';
            savedBranch = tree(depth, seed);
            var expectedBranch = savedBranch.fold();
            var actualBranch = tree(depth, seed).fold();
            if (actualBranch != expectedBranch) throw 'branch mismatch $i';
            // Both retained objects must survive collections after fused calls.
            if (i % 16 == 0) hl.Gc.major();
            if (savedLink.fold() != expectedLink || savedBranch.fold() != expectedBranch) throw 'lost roots';
            check = (check ^ actualLink) + actualBranch;
            seed = seed * 1664525 + 1013904223;
        }
        return check;
    }
    static function hot():Int {
        // Keep a local reference live across helper calls and an OSR hand-off.
        var keep = chain(37, 5);
        var expected = keep.fold();
        var total = 0;
        for (batch in 0...100) {
            for (i in 0...100) total += chain(i, 64).fold();
            if (keep.fold() != expected) throw 'lost local root';
            Sys.sleep(0.01);
        }
        hl.Gc.major();
        if (keep.fold() != expected) throw 'lost local after collection';
        return total;
    }
    static function main() {
        var result = Sys.args().length == 0 ? cases() : hot();
        Sys.println('tree-matrix=$result');
    }
}
