package wgpu;

/** What a sampler does outside 0..1. **/
enum abstract AddressMode(Int) from Int to Int {
	var ClampToEdge = 0;
	var Repeat = 1;
}
