package window;

/** What happened since the last poll. **/
abstract Events(Int) from Int to Int {
	public var closed(get, never) : Bool;
	public var resized(get, never) : Bool;

	inline function get_closed() : Bool {
		return this & 1 != 0;
	}

	inline function get_resized() : Bool {
		return this & 2 != 0;
	}
}
