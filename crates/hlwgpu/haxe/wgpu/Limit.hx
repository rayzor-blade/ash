package wgpu;

/**
	What `Adapter.limit()` can be asked for.
**/
enum abstract Limit(Int) to Int {
	var MaxTextureDimension1D = 0;
	var MaxTextureDimension2D = 1;
	var MaxTextureDimension3D = 2;
	var MaxBindGroups = 3;
	var MaxBufferSize = 4;
	var MaxComputeWorkgroupSizeX = 5;
	var MaxComputeInvocationsPerWorkgroup = 6;
}
