// Same socket calls, but the peer is a separate process, so nothing depends
// on a second Haxe thread getting scheduled.
import sys.net.Host;
import sys.net.Socket;

class TestSocketClient {
	static function main() {
		var port = Std.parseInt(Sys.args()[0]);
		var sock = new Socket();
		sock.connect(new Host("127.0.0.1"), port);
		Sys.println("connect ok");
		sock.output.writeString("hello\n");
		sock.output.flush();
		var ready = Socket.select([sock], [], [], 5.0);
		if (ready.read.length == 0) {
			Sys.println("FAIL select() never reported readable");
			return;
		}
		var reply = sock.input.readLine();
		sock.close();
		Sys.println("OK TestSocketClient " + reply);
	}
}
