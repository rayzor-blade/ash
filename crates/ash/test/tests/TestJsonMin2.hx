import haxe.Json;

class TestJsonMin2 {
    static function main():Void {
        Sys.println("parse 1 pair");
        var a = Json.parse('{"x":10}');
        Sys.println("ok");
        Sys.println("parse 2 pairs");
        var b = Json.parse('{"x":10,"y":20}');
        Sys.println("ok");
        Sys.println("parse array");
        var c = Json.parse("[1,2,3]");
        Sys.println("done");
    }
}
