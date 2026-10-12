class TestGeneratedHelper {
    static function sum(n:Int):Int {
        if (n <= 0) return 3;
        return sum(n - 1) + n;
    }

    static function main() {
        var check = 0;
        // Keep calling across broker installs, with time for the LLVM tier.
        for (batch in 0...100) {
            for (i in 0...100) check += sum(512);
            Sys.sleep(0.01);
        }
        Sys.println('generated-check=$check');
        if (check != 1313310000) throw 'wrong generated helper result: $check';
    }
}
