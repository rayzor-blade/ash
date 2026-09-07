package wgpu;

/**
	Anything a bind group can hold.

	A handle carries its own kind, so the library works out what each entry
	binds as; these casts are only so a mixed array still type-checks.
**/
abstract Binding(Int) from Int to Int {
	@:from static inline function ofBuffer(v : Buffer) : Binding {
		return cast(v : Int);
	}

	@:from static inline function ofView(v : TextureView) : Binding {
		return cast(v : Int);
	}

	@:from static inline function ofSampler(v : Sampler) : Binding {
		return cast(v : Int);
	}
}
