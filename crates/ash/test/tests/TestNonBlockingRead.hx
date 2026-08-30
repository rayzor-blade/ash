// The exact shape colyseus-websocket's SocketSys drives: blocking connect,
// then setBlocking(false), then select() + readBytes. A read with no data
// pending must raise Blocked -- SocketSys closes the socket for any other
// error, which surfaces to the player as "failed to connect".
import haxe.io.Bytes;
import haxe.io.Error;
import sys.net.Host;
import sys.net.Socket;

class TestNonBlockingRead {
	static function main() {
		var port = Std.parseInt(Sys.args()[0]);
		var sock = new Socket();
		sock.connect(new Host("127.0.0.1"), port);
		sock.setBlocking(false);
		Sys.println("connected, non-blocking");

		// The peer is deliberately silent, so this read has nothing to return.
		var buf = Bytes.alloc(1024);
		var outcome = "no exception (returned normally)";
		try {
			var n = sock.input.readBytes(buf, 0, buf.length);
			outcome = "returned " + n + " bytes with nothing sent";
		} catch (e:Error) {
			outcome = switch (e) {
				case Blocked: "Error.Blocked  <-- correct";
				case Custom(Blocked): "Error.Custom(Blocked)  <-- accepted";
				case _: "Error." + Std.string(e) + "  <-- WRONG, SocketSys closes on this";
			}
		} catch (e:Dynamic) {
			outcome = (e == "Blocking")
				? "\"Blocking\"  <-- accepted"
				: Std.string(e) + "  <-- WRONG, SocketSys closes on this";
		}
		Sys.println("readBytes with no data: " + outcome);

		// select() must also report the socket as not-readable here.
		var r = Socket.select([sock], [], [], 0.4);
		Sys.println("select readable when silent: " + r.read.length + " (expected 0)");
		sock.close();
		Sys.println("OK TestNonBlockingRead");
	}
}
