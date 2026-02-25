import haxe.Json;

class TestJsonMin {
    static function main():Void {
        Sys.println("before parse");
        var str = '{"x":10}';
        Sys.println("str ok");
        var parsed = Json.parse(str);
        Sys.println("parsed ok");
    }
}
