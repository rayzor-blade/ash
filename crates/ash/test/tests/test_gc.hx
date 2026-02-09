class Main {
    static function allocateMany() {
        var i = 0;
        while (i < 1000) {
            var s = "item_" + i;
            if (i % 100 == 0) {
                Sys.println("alloc: " + i);
            }
            i++;
        }
    }

    static function main() {
        allocateMany();
        Sys.println("gc_test_done");
    }
}
