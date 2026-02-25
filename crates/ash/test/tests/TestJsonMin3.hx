import haxe.Json;

class TestJsonMin3 {
    static function main():Void {
        Sys.println("test1");
        var a = Json.parse('{"x":10}');
        Sys.println("ok1");
        Sys.println("test2");
        var b = Json.parse('{"y":20}');
        Sys.println("ok2");
        Sys.println("done");
    }
}
