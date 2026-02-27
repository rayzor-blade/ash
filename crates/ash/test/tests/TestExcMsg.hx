class TestExcMsg {
    static function main():Void {
        try {
            throw "test error";
        } catch (e:String) {
            Sys.println("caught string: [" + e + "]");
        }
        
        try {
            throw "another error";  
        } catch (e:Dynamic) {
            Sys.println("caught dynamic: [" + e + "]");
        }
        
        try {
            throw new haxe.Exception("my message");
        } catch (e:haxe.Exception) {
            Sys.println("caught exception: [" + e.message + "]");
        }
        
        Sys.println("done");
    }
}
