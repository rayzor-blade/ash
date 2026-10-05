// A worker thread turns an object into a string before its `toString` has
// been compiled.
class Named {
    var name:String;

    public function new(name:String) {
        this.name = name;
    }

    public function toString():String {
        return "Named(" + name + ")";
    }
}

class WorkerToString {
    static function main() {
        var lock = new sys.thread.Lock();
        var text = null;
        sys.thread.Thread.create(() -> {
            text = "" + new Named("worker");
            lock.release();
        });
        Sys.println(lock.wait(5.0) ? text : "timeout");
    }
}
