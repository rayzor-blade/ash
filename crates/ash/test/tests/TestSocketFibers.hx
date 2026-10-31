import sys.net.Host;
import sys.net.Socket;
import sys.thread.Thread;

// Threads parked in a socket read, accept and select must not stop a sibling
// thread from running, even when all share one scheduler thread.
class TestSocketFibers {
	static var ticks = 0;

	static function listener():Socket {
		var l = new Socket();
		l.bind(new Host("127.0.0.1"), 0);
		l.listen(4);
		return l;
	}

	static function main() {
		var l = listener();
		var client = new Socket();
		client.connect(new Host("127.0.0.1"), l.host().port);
		var server = l.accept();
		var idle = listener();

		Thread.create(() -> {
			try {
				server.input.readByte();
			} catch (e:Dynamic) {}
		});
		Thread.create(() -> {
			try {
				idle.accept();
			} catch (e:Dynamic) {}
		});
		Thread.create(() -> {
			try {
				Socket.select([server], null, null, 30.0);
			} catch (e:Dynamic) {}
		});
		Thread.create(() -> {
			while (true) {
				ticks++;
				Sys.sleep(0.01);
			}
		});
		Sys.sleep(0.5);
		var seen = ticks;
		Sys.println(seen >= 5 ? "ran" : "starved " + seen);
		Sys.exit(0);
	}
}
