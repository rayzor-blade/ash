import haxe.Json;

enum Color {
	Red;
	Green;
	Blue;
	Rgb(r:Int, g:Int, b:Int);
}

class TestStdlib {
	static function factorial(n:Int):Int {
		if (n <= 1)
			return 1;
		return n * factorial(n - 1);
	}

	static function fibonacci(n:Int):Int {
		if (n <= 1)
			return n;
		return fibonacci(n - 1) + fibonacci(n - 2);
	}

	static function describeColor(c:Color):String {
		return switch (c) {
			case Red: "Red";
			case Green: "Green";
			case Blue: "Blue";
			case Rgb(r, g, b): "RGB(" + r + "," + g + "," + b + ")";
		};
	}

	static function testStrings():Void {
		Sys.println("--- strings ---");

		var str = "Hello, HashLink!";
		Sys.println("len: " + str.length);
		Sys.println("upper: " + str.toUpperCase());
		Sys.println("lower: " + str.toLowerCase());
		Sys.println("sub: " + str.substr(7, 8));
		Sys.println("index: " + str.indexOf("Hash"));
		Sys.println("char5: " + str.charAt(5));

		var numStr = "12345";
		var num = Std.parseInt(numStr);
		Sys.println("parseInt: " + num);

		var floatStr = "3.14159";
		var pi = Std.parseFloat(floatStr);
		Sys.println("parseFloat: " + pi);

		var joined = [1, 2, 3, 4, 5].map(n -> Std.string(n)).join(", ");
		Sys.println("joined: " + joined);
	}

	static function testArrays():Void {
		Sys.println("--- arrays ---");

		var arr = [10, 20, 30, 40, 50];
		Sys.println("len: " + arr.length);
		Sys.println("first: " + arr[0]);
		Sys.println("last: " + arr[arr.length - 1]);

		arr.push(60);
		Sys.println("after push: " + arr.length);

		var doubled:Array<Int> = arr.map(n -> n * 2);
		Sys.println("doubled[0]: " + doubled[0]);
		Sys.println("doubled[5]: " + doubled[5]);

		var evens = arr.filter(n -> n % 20 == 0);
		Sys.println("evens len: " + evens.length);

		var shuffled = [5, 2, 8, 1, 9, 3, 7, 4, 6];
		shuffled.sort((a, b) -> a - b);
		Sys.println("sorted: " + shuffled.map(n -> Std.string(n)).join(","));

		var sum = 0;
		for (n in arr) {
			sum += n;
		}
		Sys.println("sum: " + sum);
	}

	static function testMaps():Void {
		Sys.println("--- maps ---");

		var ages = new Map<String, Int>();
		ages.set("Alice", 30);
		ages.set("Bob", 25);
		ages.set("Charlie", 35);
		Sys.println("Alice: " + ages.get("Alice"));
		Sys.println("Bob: " + ages.get("Bob"));
		Sys.println("exists Charlie: " + ages.exists("Charlie"));
		Sys.println("exists Diana: " + ages.exists("Diana"));

		ages.set("Diana", 28);
		ages.remove("Bob");

		var count = 0;
		for (_ in ages) {
			count++;
		}
		Sys.println("count: " + count);
	}

	static function testEnums():Void {
		Sys.println("--- enums ---");

		var colors = [Red, Green, Blue, Rgb(255, 128, 0)];
		for (color in colors) {
			Sys.println(describeColor(color));
		}

		var c = Rgb(100, 150, 200);
		switch (c) {
			case Rgb(r, g, b):
				var brightness = Std.int((r + g + b) / 3);
				Sys.println("brightness: " + brightness);
			default:
				Sys.println("not rgb");
		}
	}

	static function testClosures():Void {
		Sys.println("--- closures ---");

		var multiplier = 5;
		var multiplyBy = function(x:Int):Int {
			return x * multiplier;
		};
		Sys.println("5*10: " + multiplyBy(10));

		var counter = 0;
		var increment = function():Void {
			counter++;
		};
		increment();
		increment();
		increment();
		Sys.println("counter: " + counter);

		var apply = function(fn:Int->Int, values:Array<Int>):Array<Int> {
			return values.map(fn);
		};
		var squared = apply(x -> x * x, [1, 2, 3, 4, 5]);
		Sys.println("squared: " + squared.map(n -> Std.string(n)).join(","));
	}

	static function testExceptions():Void {
		Sys.println("--- exceptions ---");

		try {
			throw "test error";
		} catch (e:String) {
			Sys.println("caught: " + e);
		}

		try {
			var arr = [1, 2, 3];
			if (arr.length < 10) {
				throw new haxe.Exception("too short");
			}
		} catch (e:haxe.Exception) {
			Sys.println("caught: " + e.message);
		}

		// Nested try/catch
		var result = "none";
		try {
			try {
				throw "inner";
			} catch (e:String) {
				result = "inner caught";
				throw "outer";
			}
		} catch (e:String) {
			result = result + ", outer caught";
		}
		Sys.println("nested: " + result);
	}

	static function testMath():Void {
		Sys.println("--- math ---");
		Sys.println("5! = " + factorial(5));
		Sys.println("10! = " + factorial(10));
		Sys.println("fib(10) = " + fibonacci(10));
		Sys.println("fib(20) = " + fibonacci(20));
		Sys.println("sqrt(144) = " + Math.sqrt(144));
		Sys.println("abs(-42) = " + Math.abs(-42));
		Sys.println("max(3,7) = " + Math.max(3, 7));
		Sys.println("min(3,7) = " + Math.min(3, 7));
		Sys.println("floor(3.7) = " + Math.floor(3.7));
		Sys.println("ceil(3.2) = " + Math.ceil(3.2));
	}

	static function testJson():Void {
		Sys.println("--- json ---");

		var obj:Dynamic = {};
		Reflect.setField(obj, "name", "Alice");
		Reflect.setField(obj, "age", 30);
		var jsonStr = Json.stringify(obj);
		Sys.println("encode: " + jsonStr);

		var parsed = Json.parse('{"x":10,"y":20}');
		Sys.println("x: " + Reflect.field(parsed, "x"));
		Sys.println("y: " + Reflect.field(parsed, "y"));

		var arr = Json.parse("[1,2,3]");
		Sys.println("arr: " + Json.stringify(arr));
	}

	static function testCasting():Void {
		Sys.println("--- casting ---");

		var i:Int = 42;
		var f:Float = i;
		Sys.println("int to float: " + f);

		var f2:Float = 3.99;
		var i2:Int = Std.int(f2);
		Sys.println("float to int: " + i2);

		var s = Std.string(12345);
		Sys.println("int to string: " + s);
		Sys.println("string len: " + s.length);
	}

	static function main():Void {
		Sys.println("=== stdlib test ===");
		testStrings();
		testArrays();
		testMaps();
		testEnums();
		testClosures();
		testExceptions();
		testMath();
		testJson();
		testCasting();
		Sys.println("=== done ===");
	}
}
