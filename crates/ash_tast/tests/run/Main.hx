// A program both genhl and nAIR can compile: the result is the exit code, so two runs can be
// compared without printing.
class Main {
	static function fib(n:Int):Int
		return n < 2 ? n : fib(n - 1) + fib(n - 2);

	static function main() {
		Sys.exit(fib(32) & 255);
	}
}
