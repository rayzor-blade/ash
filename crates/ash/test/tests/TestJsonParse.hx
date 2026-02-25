import haxe.Json;

class TestJsonParse {
    static function main():Void {
        Sys.println("--- json parse test ---");
        var parsed = Json.parse('{"x":10,"y":20}');
        Sys.println("parsed ok");
        Sys.println("x: " + Reflect.field(parsed, "x"));
        Sys.println("y: " + Reflect.field(parsed, "y"));
        Sys.println("done");
    }
}
