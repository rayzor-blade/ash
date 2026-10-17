// CASE #16: a native primitive raises with hl_throw inside a try.
//
// The throw must reach the catch with the thrown value intact.
class Cb16 {
    @:hlNative("cb16test", "raise")
    static function raise():Void {}

    static function main() {
        Sys.println("case16: a native raises through hl_throw");
        try {
            raise();
            Sys.println("FAIL: the raise was not caught");
        } catch (e:Dynamic) {
            var said = Std.string(e);
            Sys.println(said == "raised by a native" ? "PASS: caught " + said : "FAIL: caught " + said);
        }
    }
}
