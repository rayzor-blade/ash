// hl.Bytes has no type header, so storing one in a Dynamic slot (an
// Array<hl.Bytes> is backed by one) must box it, and reading it back must
// unbox the same buffer. Prints checksums of what each read sees.
class TestDynBytes {
	static function main() {
		var bufs:Array<hl.Bytes> = [];
		for (k in 0...3) {
			var b = new hl.Bytes(16);
			for (i in 0...16)
				b[i] = (k * 16 + i + 1) & 0xff;
			bufs.push(b);
		}
		var byIndex = 0;
		for (k in 0...bufs.length)
			for (i in 0...16)
				byIndex += bufs[k][i] * (i + 1);
		var iterated = 0;
		for (b in bufs)
			if (b != null)
				for (i in 0...16)
					iterated += b[i];
		var d:Dynamic = bufs[1];
		var back:hl.Bytes = d;
		Sys.println('byIndex=$byIndex iterated=$iterated dynamic=${back[0]},${back[15]}');
	}
}
