// The regexp cache recognises its subject by address and terminator. A
// StringBuf's toString shares the buffer, and adding after it moves the end:
// the second string must be matched as what it is. Then a loop of advancing
// matches over a large subject, which must not cost the subject's size per
// match.
class TestRegexSubject {
	static function main() {
		var sb = new StringBuf();
		sb.add("abc");
		var r = ~/def/;
		var s1 = sb.toString();
		Sys.println("1 " + r.match(s1));
		sb.add("def");
		var s2 = sb.toString();
		Sys.println("2 " + r.match(s2));
		Sys.println("3 " + r.matchedPos().pos);

		var big = new StringBuf();
		for (i in 0...200000)
			big.add("xyz abc ");
		var text = big.toString();
		var re = ~/abc/;
		var n = 0;
		var pos = 0;
		var t = haxe.Timer.stamp();
		while (re.matchSub(text, pos)) {
			n++;
			var p = re.matchedPos();
			pos = p.pos + p.len;
		}
		var took = haxe.Timer.stamp() - t;
		Sys.println("matches " + n);
		Sys.println(took < 5.0 ? "fast" : "slow " + took);
	}
}
