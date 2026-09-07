package wgpu;

/**
	A render pass being described, before it has anywhere to draw.

	A pass can have several colour targets and may or may not have depth, so it
	is described and then opened. As with a pipeline, the type changes as it is
	filled in: `begin` only exists once there is at least one colour target.
**/
abstract RenderPassBuilder(Int) from Int to Int {
	/**
		Adds a colour target and the colour to clear it to.

		Their order is the order the fragment shader's `@location`s are
		numbered in.
	**/
	public inline function colour(target : TextureView, r : Float, g : Float, b : Float, a = 1.0) : RenderPassTargets {
		_Native.pass_colour(this, target, r, g, b, a);
		return cast this;
	}
}
