package wgpu;

/** Where work is submitted. Comes with the device. **/
abstract Queue(Int) from Int to Int {
	public inline function write(buffer : Buffer, offset : Int, data : haxe.io.Bytes) : Void {
		_Native.queue_write_buffer(this, buffer, offset, @:privateAccess data.b, data.length);
	}

	/** Rows need no alignment here, unlike a copy out of a texture. **/
	public inline function writeTexture(texture : Texture, data : haxe.io.Bytes, width : Int, height : Int, bytesPerRow : Int) : Void {
		_Native.queue_write_texture(this, texture, @:privateAccess data.b, width, height, bytesPerRow);
	}

	/** Finishes when everything submitted so far has run. **/
	public inline function workDone(device : Device) : Request {
		return new Request(_Native.queue_work_done(device, this));
	}
}
