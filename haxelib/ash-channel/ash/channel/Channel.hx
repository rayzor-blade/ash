package ash.channel;

import haxe.io.Bytes;

/** Raised by `send` and `recv` once the channel is closed. */
class ChannelClosed extends haxe.Exception {
	public function new()
		super("channel closed");
}

/**
	Byte messages between this program and a page agent.

	A program in a browser runs in a Worker that never returns to its own event
	loop, so it cannot hold a WebSocket. The page can. `Channel.open("name")`
	shares a block of the program's memory with the page agent `name`
	(`<name>.mjs` beside the page, built on `channel-agent.mjs`), and the agent
	owns the connection. Messages are opaque byte strings; one `send` is one
	message for the agent, and one `recv` is one message from it.

	Needs the shared-memory build (`--target wasm32-wasip1-threads`) on a
	cross-origin-isolated page, and `ash_channel.wasm` beside the program.
**/
class Channel {
	/** The page agent has a connection open. */
	public var isOpen(get, never):Bool;

	/** The connection closed, from either side. Messages already received can still be taken. */
	public var isClosed(get, never):Bool;

	final handle:Int;

	function new(handle:Int)
		this.handle = handle;

	/**
		Open a channel served by the page agent `agent`: letters, digits, `_`
		and `-`, at most 64. `capacity` is the size of each of the two rings in
		bytes, a power of two from 4096 to 67108864; one message may take at
		most half of it, less 8 bytes, capped at 16777204 bytes. Throws if the page would not start the
		agent: no `ashAgent` hook, or memory that is not shared.
	**/
	public static function open(agent:String, capacity:Int = 65536):Channel {
		final name = Bytes.ofString(agent);
		final handle = nativeOpen(name.getData(), name.length, capacity);
		if (handle == 0)
			throw 'cannot open channel "$agent": the page did not start the agent '
				+ '(it needs a shared-memory build on a cross-origin-isolated page, '
				+ 'and a capacity that is a power of two from 4096 to 67108864)';
		return new Channel(handle);
	}

	/**
		Queue `message` for the agent. Waits while the ring is full. Throws
		`ChannelClosed` once the channel is closed, and a string for a message
		that can never fit.
	**/
	public function send(message:Bytes):Void {
		var code:Int;
		while ((code = nativeSend(handle, message.getData(), 0, message.length)) == 1)
			Sys.sleep(0.001);
		switch code {
			case 0:
			case 2:
				throw 'a message of ${message.length} bytes does not fit in the channel';
			default:
				throw new ChannelClosed();
		}
	}

	/**
		The next message from the agent, waiting for it: the current fiber
		parks and the others run. Throws `ChannelClosed` once the channel is
		closed and nothing is left to take.
	**/
	public function recv():Bytes {
		var raw:hl.Bytes;
		try {
			raw = nativeRecv(handle).await();
		} catch (e:Dynamic) {
			throw new ChannelClosed();
		}
		final n = raw.getI32(0);
		return raw.toBytes(4 + n).sub(4, n);
	}

	/** A future of raw bytes: a little-endian u32 message length, then the message. */
	public function recvAsync():ash.Future<hl.Bytes>
		return nativeRecv(handle);

	/** Tell the agent to close its connection. Waiting `recv`s throw `ChannelClosed`. */
	public function close():Void
		nativeClose(handle);

	function get_isOpen():Bool
		return nativeState(handle) & 7 == 1;

	function get_isClosed():Bool
		return nativeState(handle) & 6 != 0;

	@:hlNative("ash_channel", "channel_open")
	static function nativeOpen(name:hl.Bytes, nameLength:Int, capacity:Int):Int
		return 0;

	@:hlNative("ash_channel", "channel_send")
	static function nativeSend(handle:Int, data:hl.Bytes, pos:Int, len:Int):Int
		return 0;

	@:hlNative("ash_channel", "channel_recv")
	static function nativeRecv(handle:Int):ash.Future<hl.Bytes>
		return null;

	@:hlNative("ash_channel", "channel_state")
	static function nativeState(handle:Int):Int
		return 0;

	@:hlNative("ash_channel", "channel_close")
	static function nativeClose(handle:Int):Void {}
}
