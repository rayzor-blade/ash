private class Twig {
    public var value:Int;
    public var east:Twig;
    public var west:Twig;
    public function new(value:Int, east:Twig, west:Twig) {
        this.value=value; this.east=east; this.west=west;
    }
    public function fold():Int {
        if (east==null) return value;
        return (east.fold() ^ value) + west.fold();
    }
}

private class CountedTwig {
    public static var made:Int=0;
    public var left:CountedTwig;
    public var right:CountedTwig;
    public function new(left:CountedTwig,right:CountedTwig) {
        made++; this.left=left; this.right=right;
    }
    public function fold():Int {
        if (left==null) return 1;
        return left.fold()+right.fold();
    }
}

class TestTreeFusion {
    static var retained:Twig;
    static function grow(seed:Int,depth:Int):Twig {
        if (depth>0) return new Twig(seed, grow(seed*3+1,depth-1),grow(7-seed,depth-1));
        return new Twig(seed,null,null);
    }
    static function counted(depth:Int):CountedTwig {
        if (depth>0) return new CountedTwig(counted(depth-1),counted(depth-1));
        return new CountedTwig(null,null);
    }
    static function main() {
        var check=0;
        for (depth in -1...7) {
            for (seed in [0,1,-9,0x7fffffff,0x80000000]) {
                retained=grow(seed,depth);
                var expected=retained.fold();
                var actual=grow(seed,depth).fold();
                if (expected!=actual) throw 'fusion mismatch $seed/$depth: $expected != $actual';
                check^=actual;
            }
        }
        if (grow(9,0x80000000).fold()!=9) throw 'negative depth must be a leaf';
        // Construction is observable, so this adjacent pair must be refused.
        var leaves=counted(4).fold();
        if (leaves!=16 || CountedTwig.made!=31) throw 'lost constructor effect';
        Sys.println('tree-fusion check=$check made=${CountedTwig.made}');
    }
}
