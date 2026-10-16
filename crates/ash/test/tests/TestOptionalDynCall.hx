// A method with optional parameters called through Dynamic without them: the
// omitted parameters read as null in every execution mode, including once the
// caller has been compiled.
class TestOptionalDynCall {
	static function failure(code:String, message:String):Dynamic
		return {error: {code: code, message: message}};

	static function probe(result:Dynamic):Int
		return result.error.message.indexOf("SCENE_BODY_SCALE");

	static function main() {
		var result:Dynamic = failure("REFUSED", "error SCENE_BODY_SCALE at entities");
		var total = 0;
		for (i in 0...20000)
			total += probe(result);
		Sys.println("hot indexOf total: " + total);
		Sys.println("indexOf: " + result.error.message.indexOf("SCENE_BODY_SCALE"));
		Sys.println("indexOf from: " + result.error.message.indexOf("e", 3));
		Sys.println("substr: " + result.error.message.substr(2));
		Sys.println("substr len: " + result.error.message.substr(2, 4));
		Sys.println("lastIndexOf: " + result.error.message.lastIndexOf("e"));
		var parts:Dynamic = {list: "a,b,c"};
		Sys.println("split: " + parts.list.split(",").length);
	}
}
