// The socket path haxe.net.WebSocket drives: a non-blocking connect settled
// with select(), then a send/recv round trip. MBHaxe's lobby client is built
// on exactly these natives, so a failure here is a failure there.
import sys.net.Host;
import sys.net.Socket;
import sys.thread.Thread;

class TestSocketSelect {
	static function main() {
		var port = 34567;

		// Server side, on its own thread: accept one client and echo a line.
		var server = new Socket();
		server.bind(new Host("127.0.0.1"), port);
		server.listen(1);
		Thread.create(() -> {
			try {
				var client = server.accept();
				var line = client.input.readLine();
				client.output.writeString("echo:" + line + "\n");
				client.output.flush();
				client.close();
			} catch (e:Dynamic) {
				Sys.println("FAIL server: " + e);
			}
		});

		// Client side, the way a websocket client drives it.
		var sock = new Socket();
		try {
			sock.setBlocking(false);
		} catch (e:Dynamic) {
			Sys.println("FAIL setBlocking(false): " + e);
			return;
		}

		var connected = false;
		try {
			sock.connect(new Host("127.0.0.1"), port);
			connected = true;
		} catch (e:Dynamic) {
			// Expected on a non-blocking socket: settle it with select().
			var sel = Socket.select([], [sock], [], 5.0);
			connected = sel.write.length > 0;
			if (!connected) {
				Sys.println("FAIL connect never became writable: " + e);
				return;
			}
		}
		Sys.println("connect ok (writable=" + connected + ")");

		sock.setBlocking(true);
		sock.output.writeString("hello\n");
		sock.output.flush();

		var ready = Socket.select([sock], [], [], 5.0);
		if (ready.read.length == 0) {
			Sys.println("FAIL select() never reported the socket readable");
			return;
		}

		var reply = sock.input.readLine();
		sock.close();
		server.close();

		if (reply != "echo:hello") {
			Sys.println("FAIL wrong reply: " + reply);
			return;
		}
		Sys.println("OK TestSocketSelect " + reply);
	}
}
