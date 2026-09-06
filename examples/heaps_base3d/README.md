# Heaps base3d on ash

A spinning cube: the smallest Heaps program that exercises what base2d never
reaches. base2d sets a background colour and draws nothing, so it makes no
draw call at all.

Traced with `ASH_TRACE_NATIVES=1`, which names each native the first time it
is called:

| | base2d | base3d |
|---|---|---|
| `sdl` primitives called | 50 | **66** |
| of which GL | 39 | **55** |
| window and event | 11 | 11 |

The window half is identical. The cube adds the seventeen that make it a
renderer -- `gl_draw_elements`, `gl_vertex_attrib_pointer`,
`gl_enable_vertex_attrib_array`, `gl_get_attrib_location`,
`gl_get_uniform_location`, `gl_uniform1i`, `gl_uniform4fv`,
`gl_bind_framebuffer`, `gl_framebuffer_texture2d`, `gl_active_texture`, and
the depth, cull, blend and polygon state around them.

Against `sdl.hdll`'s 208 exports and the 163 the bytecode names, 66 is the
size of a port that runs this program.

`gl_has_extension` is the one primitive base2d calls and this does not, so a
library covering this program plus that one covers both.

## Two things that are not obvious

**`hxd.Res.initEmbed()`, not `initLocal()`.** With a local loader the cube
sets up its buffers and then never draws: `gl_buffer_sub_data` is called and
`gl_draw_elements` is not, and nothing reports an error. The window opens on a
cleared background and looks like a working program with nothing in it.

**The library versions.** See `compile.hxml`: haxelib's `heaps` and `hlsdl`
are both `dev:` checkouts belonging to the MBHaxe fixture, and building
against those produces a program that names `sdl@win_get_pixel_size` -- a
primitive the patch adds and the committed `sdl.hdll` does not export. ash
then refuses to start, naming it.

## Building and running

    mkdir -p res bin && cp ../heaps_base2d/bin/*.hdll bin/
    haxe compile.hxml
    cd bin && ash --mode interp game.hl

`res` has to exist even though it is empty: `initEmbed` embeds that directory
at compile time. `bin` is not committed -- the `.hdll` files are the base2d
example's.

The `.hdll` files are copies of the ones beside the base2d example; see that
README for their provenance.
