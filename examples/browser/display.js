// The compositor: an agent whose only job is to put the program's frames on
// the page.
//
// It exists because the agent running HashLink cannot do this. That one is
// inside a call into the program for as long as the program runs and never
// returns to its event loop, and a canvas reaches the page at the end of a
// task -- so a frame drawn there arrives when the program ends and not
// before. The whole run composited as one frame, after every thread had
// finished, which is not a slow renderer but no renderer at all.
//
// This agent's event loop is free, so its frames land. It never talks to the
// program and the program never waits for it: the framebuffer is in shared
// memory, the program keeps writing it at whatever rate it manages, and this
// reads it at display rate. Nothing is synchronised, so a frame can catch a
// band mid-update -- at these rates invisible, and the honest picture of
// several threads writing one buffer.
//
// The page keeps the <canvas> element and gives up the right to draw on it,
// which is what `transferControlToOffscreen` means. That is the point rather
// than a detail: the page's own thread stays out of the paint path entirely,
// so the frame counter it keeps is still measuring a thread with nothing to
// do.

let context = null;
let frame = null;
let image = null;
let painting = null;

// Sixty a second, by interval rather than by animation frame: a worker has no
// `requestAnimationFrame`. Each tick is a task, and a task that drew is what
// pushes a frame to the page.
const PERIOD = 1000 / 60;

self.onmessage = ({ data }) => {
  if (data.canvas) {
    context = data.canvas.getContext("2d");
    return;
  }
  if (!context) return;

  frame = data;
  const canvas = context.canvas;
  // The program decides how big a frame is. The page only declared a size for
  // the element's layout, and a mismatch would crop rather than scale.
  if (canvas.width !== frame.width) canvas.width = frame.width;
  if (canvas.height !== frame.height) canvas.height = frame.height;
  image = new ImageData(frame.width, frame.height);
  if (painting === null) painting = setInterval(paint, PERIOD);
};

function paint() {
  if (!frame || !image) return;
  try {
    // A fresh view every time. A `WebAssembly.Memory` that grows detaches the
    // buffer the last one was taken over, and the collector grows this one.
    const pixels = new Uint8Array(
      frame.memory.buffer,
      frame.address,
      frame.width * frame.height * 4,
    );
    // Copied rather than viewed: `ImageData` will not take a view backed by a
    // `SharedArrayBuffer`, and a threaded module's memory is one.
    image.data.set(pixels);
    context.putImageData(image, 0, 0);
  } catch (e) {
    // A detached buffer or a program that has exited. Stop rather than throw
    // once a second for the rest of the page's life.
    clearInterval(painting);
    painting = null;
  }
}
