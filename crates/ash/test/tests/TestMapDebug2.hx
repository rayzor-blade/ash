class TestMapDebug2 {
    static function main():Void {
        var m = new Map<String, Int>();
        m.set("a", 1);
        m.set("b", 2);
        m.set("c", 3);

        // Try Array.push approach  
        var arr = new Array<Int>();
        arr.push(10);
        arr.push(20);
        arr.push(30);
        Sys.println("arr len: " + arr.length);
        
        // Simple iteration over the array
        var sum = 0;
        for (v in arr) {
            sum += v;
        }
        Sys.println("arr sum: " + sum);

        // Now map iteration 
        var count = 0;
        for (v in m) {
            count += v;
        }
        Sys.println("map sum: " + count);
        
        Sys.println("done");
    }
}
