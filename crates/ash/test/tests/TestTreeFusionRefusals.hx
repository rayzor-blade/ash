private class Node {
    public var value:Int;
    public var left:Node;
    public var right:Node;
    public function new(value:Int, left:Node, right:Node) {
        this.value = value; this.left = left; this.right = right;
    }
    public function fold():Int {
        if (left == null) return value;
        return value + left.fold() - right.fold();
    }
    public function skipping():Int { return value; }
    public function identity():Int { return left == right ? 1 : 2; }
    public function mutate():Int {
        value += 1;
        if (left == null) return value;
        return left.mutate() + right.mutate();
    }
    public function throwing():Int {
        if (left == null) throw 'leaf';
        return left.throwing() + right.throwing();
    }
    public function nullRead():Int { return left.value; }
}
private class Observed {
    public static var events:String = '';
    public var left:Observed;
    public var right:Observed;
    public function new(value:Int, left:Observed, right:Observed) {
        events += '$value,';
        if (value == -3) throw 'constructor';
        this.left = left; this.right = right;
    }
    public function fold():Int {
        if (left == null) return 1;
        return left.fold() + right.fold();
    }
}
class TestTreeFusionRefusals {
    static var escaped:Node;
    static function grow(value:Int, depth:Int):Node {
        if (depth > 0) return new Node(value, grow(value - 1, depth - 1), grow(value + 1, depth - 1));
        return new Node(value, null, null);
    }
    static function shared(value:Int, depth:Int):Node {
        if (depth > 0) { var child = shared(value + 1, depth - 1); return new Node(value, child, child); }
        return new Node(value, null, null);
    }
    static function observed(value:Int, depth:Int):Observed {
        if (depth > 0) return new Observed(value, observed(value - 1, depth - 1), observed(value + 1, depth - 1));
        return new Observed(value, null, null);
    }
    static function runEffects():Void {
        Sys.println('observed=${observed(0, 2).fold()} events=${Observed.events}');
        Observed.events = '';
        try { observed(-2, 1).fold(); } catch (e:Dynamic) { Sys.println('caught=$e events=${Observed.events}'); }
        try { grow(1, 1).throwing(); } catch (e:Dynamic) { Sys.println('caught=$e'); }
        try { grow(1, 0).nullRead(); } catch (e:Dynamic) { Sys.println('caught=$e'); }
    }
    static function runAliases():Void {
        var a = grow(7, 3);
        var fold = a.fold();
        escaped = a;
        Sys.println('escape=$fold/${escaped.value}');
        Sys.println('shared=${shared(3, 4).fold()} identity=${grow(1, 2).identity()}');
        Sys.println('skip=${grow(2, 3).skipping()} mutate=${grow(2, 2).mutate()} dynamic=${(cast grow(2, 2):Dynamic).fold()}');
    }
    static function diverge(depth:Int):Node {
        if (depth > 0) return new Node(depth, diverge(depth - 1), diverge(depth - 1));
        while (true) {} // Even a consumer that skips children must preserve this.
        return null;
    }
    static function main() {
        if (Sys.args().length > 0) { Sys.println('starting divergent producer'); Sys.stdout().flush(); diverge(1).skipping(); return; }
        runEffects(); runAliases();
    }
}
