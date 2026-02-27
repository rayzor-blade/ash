interface INoise {
    public function noise():String;
}

class Animal implements INoise {
    public var name(default, set):String;
    public var count:Int = 0;

    public function new(name:String) {
        this.name = name;
    }

    function set_name(v:String):String {
        count++;
        return name = v;
    }

    public function noise():String {
        return "???";
    }
}

class Dog extends Animal {
    public function new(name:String) {
        super(name);
    }

    override public function noise():String {
        return "woof";
    }
}

class TestFeatureOO {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function main():Void {
        var a:Animal = new Dog("rex");
        assertEq("dispatch", "woof", a.noise());
        a.name = "rex2";
        assertEq("property_count", 2, a.count); // constructor set + assignment

        var n:INoise = a;
        assertEq("iface_dispatch", "woof", n.noise());

        Sys.println("OK TestFeatureOO");
    }
}
