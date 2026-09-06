/**
	A spinning cube, which is the smallest Heaps program that exercises the
	parts base2d never reaches: vertex and index buffers, a camera and its
	matrices, depth testing, and an actual draw call.
**/
class Main extends hxd.App {
	var cube:h3d.scene.Mesh;

	override function init() {
		// Heaps looks up per-model smoothing settings through the resource
		// loader, and reaches it before anything asks for a resource, so the
		// loader has to exist even for a program with no assets.
		hxd.Res.initEmbed();

		var prim = new h3d.prim.Cube(1, 1, 1);
		// The cube's faces share corners, and a shared corner cannot have two
		// normals; unindexing gives each face its own so the lighting has
		// edges rather than a smooth ball.
		prim.unindex();
		prim.addNormals();
		prim.addUVs();

		cube = new h3d.scene.Mesh(prim, s3d);
		cube.material.color.setColor(0xFF4488FF);

		s3d.camera.pos.set(3, 3, 3);
		s3d.camera.target.set(0, 0, 0);
		new h3d.scene.fwd.DirLight(new h3d.Vector(-0.3, -0.5, -0.8), s3d);
	}

	override function update(dt:Float) {
		cube.rotate(0, 0, dt);
	}

	static function main() {
		new Main();
	}
}
