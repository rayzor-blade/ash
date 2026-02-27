class TestIndexOf {
    static function main():Void {
        var s = "Hello, HashLink!";
        var idx = s.indexOf("Hash");
        Sys.println("indexOf: " + idx);
        
        // Try simpler cases too
        var s2 = "abcdef";
        var idx2 = s2.indexOf("cd");
        Sys.println("indexOf2: " + idx2);
        
        // Single char
        var idx3 = s2.indexOf("a");
        Sys.println("indexOf3: " + idx3);
    }
}
