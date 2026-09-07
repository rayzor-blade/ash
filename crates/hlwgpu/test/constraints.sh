#!/bin/sh
# The builder's type states, checked by trying to break them.
#
# `PipelineBuilder` and its siblings exist so that a mis-ordered chain is a
# compile error rather than a primitive quietly attaching an attribute to no
# buffer. That only holds if it is tested, and the only way to test a thing
# that must NOT compile is to try to compile it.
#
#     crates/hlwgpu/test/constraints.sh
set -e
here=$(cd "$(dirname "$0")" && pwd)
haxe_path="$here/../haxe"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

fail=0

reject() {
	cat > "$work/Bad.hx" <<HX
class Bad {
	static function main() {
		var device : wgpu.Device = 0;
		var shader : wgpu.Shader = 0;
		$2
	}
}
HX
	if haxe -hl "$work/out.hl" -cp "$haxe_path" -cp "$work" -main Bad >/dev/null 2>&1; then
		echo "  NOT REJECTED: $1"
		fail=1
	else
		echo "  rejected: $1"
	fi
}

accept() {
	cp "$1" "$work/Good.hx"
	if haxe -hl "$work/out.hl" -cp "$haxe_path" -cp "$work" -main Good >/dev/null 2>&1; then
		echo "  accepted: a correct chain"
	else
		echo "  WRONGLY REJECTED: a correct chain"
		fail=1
	fi
}

reject "an attribute with no vertex buffer open" \
	'device.pipeline().shader(shader,"vs","fs").attribute(Float32x2, 0, 0);'
reject "a blend with no target open" \
	'device.pipeline().shader(shader,"vs","fs").blend(One, One);'
reject "a build with no target" \
	'device.pipeline().shader(shader,"vs","fs").build();'
reject "a vertex buffer before any shader" \
	'device.pipeline().vertexBuffer(16);'

cat > "$work/Good.src" <<'HX'
class Good {
	static function main() {
		var device : wgpu.Device = 0;
		var shader : wgpu.Shader = 0;
		device.pipeline().shader(shader, "vs", "fs")
			.vertexBuffer(16).attribute(Float32x2, 0, 0)
			.target(Rgba8Unorm).blend(One, One)
			.build();
	}
}
HX
accept "$work/Good.src"

[ $fail -eq 0 ] && echo "builder constraints hold" || { echo "builder constraints DO NOT hold"; exit 1; }
