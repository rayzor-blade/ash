class TestHdll {
    @:hlNative("mytest", "add")
    static function nativeAdd(a:Int, b:Int):Int {
        return 0;
    }

    @:hlNative("mytest", "mul")
    static function nativeMul(a:Int, b:Int):Int {
        return 0;
    }

    @:hlNative("mytest", "hello")
    static function nativeHello():Void {}

    static function main() {
        nativeHello();
        Sys.println("add: " + nativeAdd(10, 32));
        Sys.println("mul: " + nativeMul(6, 7));
        Sys.println("hdll test done");
    }
}
