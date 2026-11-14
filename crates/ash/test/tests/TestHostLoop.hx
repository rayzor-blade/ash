// Waits the way a window host's wait does: each wait hands the thread back to
// the host's loop, which resumes the program on its next turn.
class TestHostLoop {
	@:hlNative("hostloop", "hostloop_wait") static function wait():Int { return 0; }
	static function main() {
		for (i in 0...3) Sys.println("step " + i + " turn " + wait());
		Sys.println("done");
	}
}
