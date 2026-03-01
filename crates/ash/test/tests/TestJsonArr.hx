import haxe.Json;

class TestJsonArr {
    static function main():Void {
        Sys.println("--- json arr test ---");

        Sys.println("test1: int");
        var s1 = Json.stringify(42);
        Sys.println("int: " + s1);

        Sys.println("test2: string");
        var s2 = Json.stringify("hello");
        Sys.println("string: " + s2);

        Sys.println("test3: array");
        var arr = Json.parse("[1,2,3]");
        Sys.println("parsed");
        var s3 = Json.stringify(arr);
        Sys.println("arr: " + s3);

        Sys.println("done");
    }
}
