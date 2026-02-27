class MiniUser {
    public var name:String;
    public var age:Int;

    public function new(name:String, age:Int) {
        this.name = name;
        this.age = age;
    }
}

class TestStdReflectType {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function assertTrue(label:String, cond:Bool):Void {
        if (!cond) {
            throw new haxe.Exception(label + " expected true");
        }
    }

    static function main():Void {
        var obj:Dynamic = {};
        Reflect.setField(obj, "name", "Alice");
        Reflect.setField(obj, "age", 30);
        assertEq("field_name", "Alice", Reflect.field(obj, "name"));
        assertTrue("has_age", Reflect.hasField(obj, "age"));
        Reflect.deleteField(obj, "age");
        assertTrue("age_deleted", !Reflect.hasField(obj, "age"));

        var u = new MiniUser("Bob", 25);
        assertEq("className", "MiniUser", Type.getClassName(Type.getClass(u)));
        assertTrue("isOfType", Std.isOfType(u, MiniUser));
        assertEq("compare", -1, Reflect.compare(3, 5));

        var created:MiniUser = Type.createInstance(MiniUser, ["Cara", 40]);
        assertEq("create_name", "Cara", created.name);
        assertEq("create_age", 40, created.age);

        var fields = Type.getInstanceFields(MiniUser);
        assertTrue("fields_has_name", fields.indexOf("name") >= 0);
        assertTrue("fields_has_age", fields.indexOf("age") >= 0);

        Sys.println("OK TestStdReflectType");
    }
}
