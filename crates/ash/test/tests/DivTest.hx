class DivTest {
    static inline var MaxIterations = 1000;

    static function main() {
        var a = 500;
        var result = a / MaxIterations;
        Sys.println("500/1000 = " + result);
        var b = 1;
        Sys.println("1/1000 = " + (b / MaxIterations));
        Sys.println("999/1000 = " + (999 / MaxIterations));
    }
}
