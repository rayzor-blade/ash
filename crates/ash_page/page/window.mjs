// The window, from the page's side: what happens to the canvas goes into the
// program's event ring, and what the program asks for comes out of its
// command ring. The format is docs/wasm/window.md; the program's side is the
// `ash_window` crate. Started by the page when a program calls
// `ash_host_agent("window", block)`.

const MAGIC = 0x57485341;
const VERSION = 1;
const HEADER = 64;

// Header words, as u32 indices.
const W = {
  MAGIC: 0, VERSION: 1, EVENT_CAP: 2, COMMAND_CAP: 3, EVENT_HEAD: 4, EVENT_TAIL: 5,
  COMMAND_HEAD: 6, COMMAND_TAIL: 7, DROPPED: 8, ATTACHED: 9, FOCUSED: 10, WIDTH: 11,
  HEIGHT: 12, SCALE: 13, SCREEN_WIDTH: 14, SCREEN_HEIGHT: 15,
};

const E = {
  RESIZED: 1, FOCUSED: 2, OCCLUDED: 3, CLOSE: 4, POINTER_ENTERED: 5, POINTER_LEFT: 6,
  POINTER_MOVED: 7, POINTER_BUTTON: 8, POINTER_CANCELLED: 9, WHEEL: 10, KEY: 11,
  MODIFIERS: 12, THEME: 13, REDRAW: 14, SCALE_FACTOR: 15, IME: 16, FILE_HOVERED: 17,
  FILE_HOVER_CANCELLED: 18, FILE_DROPPED: 19, FILE_READ: 20, PINCH: 21,
  TOUCHPAD_PRESSURE: 22, MOUSE_MOTION: 23, SUSPENDED: 24, RESUMED: 25, MONITOR: 26,
  FULLSCREEN: 27, POINTER_LOCK: 28,
};
const C = {
  SET_TITLE: 1, SET_CURSOR: 2, REQUEST_REDRAW: 3, SET_SIZE: 4, FOCUS: 5, FULLSCREEN: 6,
  POINTER_LOCK: 7, SET_MIN_SIZE: 8, SET_MAX_SIZE: 9, SET_VISIBLE: 10, SET_CURSOR_IMAGE: 11,
  SET_IME_ALLOWED: 12, SET_IME_AREA: 13, READ_FILE: 14, REQUEST_MONITOR: 15,
};
const IME = { ENABLED: 0, PREEDIT: 1, COMMIT: 2, DISABLED: 3 };
const PHASE = { STARTED: 0, MOVED: 1, ENDED: 2, CANCELLED: 3 };

const encoder = new TextEncoder();
const decoder = new TextDecoder();

// A payload under construction. Fields are little-endian and 4-aligned.
class Payload {
  constructor() { this.parts = []; this.size = 0; }
  u32(v) { return this.push(4, (d, o) => d.setUint32(o, v >>> 0, true)); }
  i32(v) { return this.push(4, (d, o) => d.setInt32(o, v | 0, true)); }
  f32(v) { return this.push(4, (d, o) => d.setFloat32(o, v, true)); }
  f64(v) { return this.push(8, (d, o) => d.setFloat64(o, v, true)); }
  str(s) {
    const bytes = encoder.encode(s);
    this.u32(bytes.length);
    return this.push((bytes.length + 3) & ~3, (d, o) =>
      new Uint8Array(d.buffer, d.byteOffset + o, bytes.length).set(bytes));
  }
  push(size, write) { this.parts.push([this.size, write]); this.size += size; return this; }
}
const P = () => new Payload();

// UTF-16 offset in `text` to a UTF-8 byte offset, for the ime selection.
const utf8Offset = (text, index) => (index < 0 ? -1 : encoder.encode(text.slice(0, index)).length);

export function start({ memory, address, canvas }) {
  const words = () => new Uint32Array(memory.buffer, address, HEADER / 4);
  const header = words();
  if (header[W.MAGIC] !== MAGIC || header[W.VERSION] !== VERSION) {
    console.error("window.mjs: the block has no window header");
    return;
  }
  const eventCap = header[W.EVENT_CAP];
  const commandCap = header[W.COMMAND_CAP];
  const eventRing = address + HEADER;
  const commandRing = eventRing + eventCap;
  // A fresh view per use: a memory that grows replaces its buffer.
  const view = () => new DataView(memory.buffer);
  const state = (word, value) => Atomics.store(words(), word, value >>> 0);
  const f32bits = (v) => new Uint32Array(new Float32Array([v]).buffer)[0];

  function emit(kind, payload = P()) {
    const h = words();
    const length = 4 + payload.size;
    let head = Atomics.load(h, W.EVENT_HEAD);
    const tail = Atomics.load(h, W.EVENT_TAIL);
    let pos = head % eventCap;
    const toEnd = eventCap - pos;
    const need = length > toEnd ? toEnd + length : length;
    if (length > eventCap || length > 0xfffc || eventCap - ((head - tail) >>> 0) < need) {
      Atomics.add(h, W.DROPPED, 1);
      return;
    }
    const d = view();
    if (length > toEnd) {
      d.setUint32(eventRing + pos, toEnd << 16, true); // pad to the end
      head = (head + toEnd) >>> 0;
      pos = 0;
    }
    const at = eventRing + pos;
    d.setUint32(at, kind | (length << 16), true);
    for (const [offset, write] of payload.parts) write(d, at + 4 + offset);
    Atomics.store(h, W.EVENT_HEAD, (head + length) >>> 0);
    // Only an Int32Array can be notified; same memory, same word.
    Atomics.notify(new Int32Array(memory.buffer, address, HEADER / 4), W.EVENT_HEAD);
  }

  function* commands() {
    const h = words();
    const head = Atomics.load(h, W.COMMAND_HEAD);
    let tail = Atomics.load(h, W.COMMAND_TAIL);
    while (tail !== head) {
      const d = view();
      const pos = tail % commandCap;
      const word = d.getUint32(commandRing + pos, true);
      const kind = word & 0xffff;
      const length = word >>> 16;
      if (length < 4 || length > commandCap - pos) break; // corrupt: stop reading
      if (kind !== 0) yield { kind, d, at: commandRing + pos + 4 };
      tail = (tail + length) >>> 0;
      Atomics.store(h, W.COMMAND_TAIL, tail);
    }
  }
  const readStr = (d, at) => {
    const n = d.getUint32(at, true);
    return decoder.decode(new Uint8Array(d.buffer, d.byteOffset + at + 4, n).slice());
  };

  const ratio = () => window.devicePixelRatio || 1;
  const modifiers = (e) =>
    (e.shiftKey ? 1 : 0) | (e.ctrlKey ? 2 : 0) | (e.altKey ? 4 : 0) | (e.metaKey ? 8 : 0) |
    (e.getModifierState && e.getModifierState("CapsLock") ? 16 : 0);
  const pointerType = (e) => (e.pointerType === "pen" ? 1 : e.pointerType === "touch" ? 2 : 0);
  const at = (e) => [e.offsetX * ratio(), e.offsetY * ratio()];

  canvas.hidden = false;
  if (canvas.tabIndex < 0) canvas.tabIndex = 0;
  canvas.style.touchAction = "none"; // pointer events for touch, not scrolling
  canvas.style.outline = "none";

  // Size and scale, in physical pixels. The canvas may belong to an
  // OffscreenCanvas owner by now; its drawing size is theirs to set from this.
  let size = [0, 0];
  const resized = (width, height) => {
    width = Math.max(1, Math.round(width));
    height = Math.max(1, Math.round(height));
    if (width === size[0] && height === size[1]) return;
    size = [width, height];
    state(W.WIDTH, width);
    state(W.HEIGHT, height);
    emit(E.RESIZED, P().u32(width).u32(height).f64(ratio()));
  };
  new ResizeObserver((entries) => {
    for (const entry of entries) {
      const box = entry.devicePixelContentBoxSize?.[0];
      if (box) resized(box.inlineSize, box.blockSize);
      else resized(entry.contentRect.width * ratio(), entry.contentRect.height * ratio());
    }
  }).observe(canvas);
  const screenSize = () => {
    state(W.SCREEN_WIDTH, screen.width);
    state(W.SCREEN_HEIGHT, screen.height);
  };
  window.addEventListener("resize", screenSize);

  // A change of scale: the query for the current resolution stops matching.
  let resolution = null;
  const watchScale = () => {
    const now = ratio();
    state(W.SCALE, f32bits(now));
    resolution = window.matchMedia(`(resolution: ${now}dppx)`);
    resolution.addEventListener("change", () => {
      emit(E.SCALE_FACTOR, P().f64(ratio()));
      watchScale();
    }, { once: true });
  };

  // The canvas and its text-input textarea are one window: focus moving
  // between them is not a change.
  let hasFocus = null;
  const focused = (on) => {
    if (on === hasFocus) return;
    hasFocus = on;
    state(W.FOCUSED, on ? 1 : 0);
    emit(E.FOCUSED, P().u32(on ? 1 : 0));
  };
  const focusChange = (on) => (e) => {
    if (e.relatedTarget && (e.relatedTarget === canvas || e.relatedTarget === textarea)) return;
    focused(on);
  };
  canvas.addEventListener("focus", focusChange(true));
  canvas.addEventListener("blur", focusChange(false));
  document.addEventListener("visibilitychange", () =>
    emit(E.OCCLUDED, P().u32(document.visibilityState === "hidden" ? 1 : 0)));
  window.addEventListener("pagehide", (e) => emit(e.persisted ? E.SUSPENDED : E.CLOSE));
  window.addEventListener("pageshow", (e) => { if (e.persisted) emit(E.RESUMED); });
  const dark = window.matchMedia("(prefers-color-scheme: dark)");
  dark.addEventListener("change", () => emit(E.THEME, P().u32(dark.matches ? 1 : 0)));
  document.addEventListener("fullscreenchange", () =>
    emit(E.FULLSCREEN, P().u32(document.fullscreenElement === canvas ? 1 : 0)));
  document.addEventListener("pointerlockchange", () =>
    emit(E.POINTER_LOCK, P().u32(document.pointerLockElement === canvas ? 1 : 0)));

  // Requests a browser allows only inside a user's input.
  let gated = [];
  const runGated = () => {
    const now = gated;
    gated = [];
    for (const request of now) {
      try {
        const done = request();
        if (done && done.catch) done.catch(() => {});
      } catch {}
    }
  };

  // Pointers. A touch is a pointer of type touch; its force is the pressure.
  canvas.addEventListener("pointerenter", (e) =>
    emit(E.POINTER_ENTERED, P().i32(e.pointerId).u32(pointerType(e))));
  canvas.addEventListener("pointerleave", (e) =>
    emit(E.POINTER_LEFT, P().i32(e.pointerId).u32(pointerType(e))));
  let lastPressure = 0;
  const pressure = (e) => {
    if (pointerType(e) !== 0 || e.pressure === lastPressure) return;
    lastPressure = e.pressure;
    // A mouse reports 0.5 while a button is down; anything else is a
    // pressure-sensitive surface.
    if (e.pressure !== 0 && e.pressure !== 0.5) {
      emit(E.TOUCHPAD_PRESSURE, P().f32(e.pressure).u32(e.pressure > 0.5 ? 2 : 1));
    }
  };
  canvas.addEventListener("webkitmouseforcechanged", (e) =>
    emit(E.TOUCHPAD_PRESSURE, P().f32(e.webkitForce / 3).u32(e.webkitForce >= 2 ? 2 : 1)));
  canvas.addEventListener("pointermove", (e) => {
    const coalesced = e.getCoalescedEvents ? e.getCoalescedEvents() : [];
    for (const m of coalesced.length ? coalesced : [e]) {
      const [x, y] = at(m);
      const dx = m.movementX * ratio();
      const dy = m.movementY * ratio();
      emit(E.POINTER_MOVED, P().i32(m.pointerId).u32(pointerType(m)).f64(x).f64(y).f64(dx).f64(dy)
        .f32(m.pressure).u32(m.buttons));
      if (document.pointerLockElement === canvas) emit(E.MOUSE_MOTION, P().f64(dx).f64(dy));
    }
    pressure(e);
  });
  const button = (pressed) => (e) => {
    if (pressed) {
      if (document.activeElement !== imeTarget()) imeTarget().focus();
      runGated();
    }
    const [x, y] = at(e);
    emit(E.POINTER_BUTTON, P().i32(e.pointerId).u32(pointerType(e)).u32(pressed ? 1 : 0)
      .u32(e.button).f64(x).f64(y).f32(e.pressure));
    pressure(e);
  };
  canvas.addEventListener("pointerdown", button(true));
  canvas.addEventListener("pointerup", button(false));
  canvas.addEventListener("pointercancel", (e) =>
    emit(E.POINTER_CANCELLED, P().i32(e.pointerId).u32(pointerType(e))));
  canvas.addEventListener("contextmenu", (e) => e.preventDefault());

  // Wheel, and the trackpad pinch browsers report as a wheel with control.
  let pinching = null;
  canvas.addEventListener("wheel", (e) => {
    e.preventDefault();
    const [x, y] = at(e);
    if (e.ctrlKey && e.deltaMode === 0) {
      const phase = pinching ? PHASE.MOVED : PHASE.STARTED;
      clearTimeout(pinching);
      pinching = setTimeout(() => {
        pinching = null;
        emit(E.PINCH, P().u32(PHASE.ENDED).f64(0).f64(x).f64(y));
      }, 150);
      emit(E.PINCH, P().u32(phase).f64(-e.deltaY / 100).f64(x).f64(y));
      return;
    }
    const scale = e.deltaMode === 0 ? ratio() : 1;
    emit(E.WHEEL, P().u32(e.deltaMode).u32(modifiers(e)).f64(e.deltaX * scale)
      .f64(e.deltaY * scale).f64(x).f64(y));
  }, { passive: false });

  // Keys, and which side each modifier is held on.
  let lastModifiers = -1;
  let sides = 0;
  const SIDE = { Shift: 1, Control: 4, Alt: 16, Meta: 64 };
  const key = (pressed) => (e) => {
    if (pressed) runGated();
    const side = SIDE[e.key];
    if (side && (e.location === 1 || e.location === 2)) {
      const bit = e.location === 1 ? side : side * 2;
      sides = pressed ? sides | bit : sides & ~bit;
    }
    const mods = modifiers(e);
    const flags = (e.repeat ? 1 : 0) | (e.isTrusted ? 0 : 2) | (e.isComposing ? 4 : 0);
    emit(E.KEY, P().u32(pressed ? 1 : 0).u32(e.location).u32(flags).u32(mods).str(e.code).str(e.key));
    if (((mods << 8) | sides) !== lastModifiers) {
      lastModifiers = (mods << 8) | sides;
      emit(E.MODIFIERS, P().u32(mods).u32(sides));
    }
    // Keys the program handles should not also scroll or navigate the page.
    // While text input is allowed they keep their default action, which is
    // how typed text reaches the input method and comes back as a commit.
    if (!e.metaKey && !e.ctrlKey && !imeAllowed) e.preventDefault();
  };
  const listenKeys = (target) => {
    target.addEventListener("keydown", key(true));
    target.addEventListener("keyup", key(false));
  };
  listenKeys(canvas);

  // Text input through an input method. EditContext where the browser has
  // it; elsewhere a textarea kept out of sight, which takes focus instead of
  // the canvas while text input is allowed.
  let editContext = null;
  let textarea = null;
  let imeAllowed = false;
  let imeArea = [0, 0, 1, 1];
  const imeTarget = () => (textarea && !textarea.disabled ? textarea : canvas);
  const ime = (what, text = "", start = -1, end = -1) =>
    emit(E.IME, P().u32(what).i32(utf8Offset(text, start)).i32(utf8Offset(text, end)).str(text));
  const placeIme = () => {
    const rect = canvas.getBoundingClientRect();
    const [x, y, w, h] = imeArea.map((v) => v / ratio());
    const bounds = new DOMRect(rect.left + x, rect.top + y, w, h);
    if (editContext) {
      editContext.updateControlBounds(rect);
      editContext.updateSelectionBounds(bounds);
    }
    if (textarea) {
      Object.assign(textarea.style, {
        left: `${bounds.left + window.scrollX}px`,
        top: `${bounds.top + window.scrollY}px`,
        height: `${Math.max(1, bounds.height)}px`,
      });
    }
  };
  const allowIme = (allowed) => {
    imeAllowed = allowed;
    if (allowed) {
      if ("EditContext" in window) {
        if (!editContext) {
          editContext = new EditContext();
          let composing = false;
          editContext.addEventListener("compositionstart", () => { composing = true; });
          editContext.addEventListener("compositionend", () => {
            composing = false;
            ime(IME.COMMIT, editContext.text);
            editContext.updateText(0, editContext.text.length, "");
            editContext.updateSelection(0, 0);
          });
          editContext.addEventListener("textupdate", () => {
            const text = editContext.text;
            if (composing) {
              ime(IME.PREEDIT, text, editContext.selectionStart, editContext.selectionEnd);
            } else {
              ime(IME.COMMIT, text);
              editContext.updateText(0, text.length, "");
              editContext.updateSelection(0, 0);
            }
          });
        }
        canvas.editContext = editContext;
      } else {
        if (!textarea) {
          textarea = document.createElement("textarea");
          textarea.setAttribute("autocomplete", "off");
          textarea.setAttribute("autocapitalize", "off");
          textarea.setAttribute("spellcheck", "false");
          Object.assign(textarea.style, {
            position: "absolute", width: "1px", opacity: "0", padding: "0", border: "0",
            resize: "none", overflow: "hidden", pointerEvents: "none",
          });
          document.body.appendChild(textarea);
          listenKeys(textarea);
          textarea.addEventListener("focus", focusChange(true));
          textarea.addEventListener("blur", focusChange(false));
          textarea.addEventListener("compositionupdate", (e) => {
            const text = e.data || "";
            ime(IME.PREEDIT, text, text.length, text.length);
          });
          textarea.addEventListener("compositionend", (e) => {
            ime(IME.COMMIT, e.data || "");
            textarea.value = "";
          });
          textarea.addEventListener("input", (e) => {
            if (!e.isComposing && e.data) ime(IME.COMMIT, e.data);
            if (!e.isComposing) textarea.value = "";
          });
        }
        textarea.disabled = false;
        if (document.activeElement === canvas) textarea.focus();
      }
      placeIme();
      ime(IME.ENABLED);
    } else {
      if (canvas.editContext) canvas.editContext = null;
      if (textarea) {
        const had = document.activeElement === textarea;
        textarea.disabled = true;
        if (had) canvas.focus();
      }
      ime(IME.DISABLED);
    }
  };

  // Files dragged onto the canvas. Their contents are read on request, into
  // the program's memory.
  const files = [];
  canvas.addEventListener("dragenter", (e) => {
    e.preventDefault();
    const items = [...(e.dataTransfer?.items || [])].filter((i) => i.kind === "file");
    emit(E.FILE_HOVERED, P().u32(items.length).str(items.map((i) => i.type).join("\n")));
  });
  canvas.addEventListener("dragover", (e) => e.preventDefault());
  canvas.addEventListener("dragleave", (e) => {
    if (!canvas.contains(e.relatedTarget)) emit(E.FILE_HOVER_CANCELLED);
  });
  canvas.addEventListener("drop", (e) => {
    e.preventDefault();
    for (const file of e.dataTransfer?.files || []) {
      const index = files.push(file) - 1;
      emit(E.FILE_DROPPED, P().u32(index).f64(file.size).str(file.name).str(file.type));
    }
  });
  const readFile = (index, into, capacity) => {
    const file = files[index];
    if (!file) return emit(E.FILE_READ, P().u32(index).i32(-1));
    file.slice(0, capacity).arrayBuffer().then(
      (bytes) => {
        new Uint8Array(memory.buffer, into, bytes.byteLength).set(new Uint8Array(bytes));
        emit(E.FILE_READ, P().u32(index).i32(bytes.byteLength));
      },
      () => emit(E.FILE_READ, P().u32(index).i32(-1)),
    );
  };

  const cursorImage = (from, width, height, hotX, hotY) => {
    const pixels = new Uint8ClampedArray(width * height * 4);
    pixels.set(new Uint8Array(memory.buffer, from, pixels.length));
    const image = new OffscreenCanvas(width, height);
    image.getContext("2d").putImageData(new ImageData(pixels, width, height), 0, 0);
    image.convertToBlob().then((blob) => {
      canvas.style.cursor = `url(${URL.createObjectURL(blob)}) ${hotX} ${hotY}, auto`;
    });
  };

  const monitor = async () => {
    let label = "";
    let width = screen.width;
    let height = screen.height;
    try {
      if (window.getScreenDetails) {
        const current = (await window.getScreenDetails()).currentScreen;
        label = current.label || "";
        width = current.width;
        height = current.height;
      }
    } catch {}
    emit(E.MONITOR, P().u32(width).u32(height).f64(ratio()).str(label));
  };

  const cssSize = (w, h, first, second) => {
    canvas.style[first] = w > 0 ? `${w}px` : "";
    canvas.style[second] = h > 0 ? `${h}px` : "";
  };

  // Commands, once per frame.
  let redraw = false;
  const frame = () => {
    if (redraw) {
      redraw = false;
      emit(E.REDRAW);
    }
    for (const { kind, d, at: p } of commands()) {
      const u = (i) => d.getUint32(p + 4 * i, true);
      const f = (offset) => d.getFloat64(p + offset, true);
      switch (kind) {
        case C.SET_TITLE: document.title = readStr(d, p); break;
        case C.SET_CURSOR: canvas.style.cursor = readStr(d, p); break;
        case C.REQUEST_REDRAW: redraw = true; break;
        case C.SET_SIZE: cssSize(f(0), f(8), "width", "height"); break;
        case C.FOCUS: imeTarget().focus(); break;
        case C.FULLSCREEN:
          if (u(0)) gated.push(() => canvas.requestFullscreen());
          else if (document.fullscreenElement) document.exitFullscreen().catch(() => {});
          break;
        case C.POINTER_LOCK:
          if (u(0)) gated.push(() => canvas.requestPointerLock());
          else if (document.pointerLockElement) document.exitPointerLock();
          break;
        case C.SET_MIN_SIZE: cssSize(f(0), f(8), "minWidth", "minHeight"); break;
        case C.SET_MAX_SIZE: cssSize(f(0), f(8), "maxWidth", "maxHeight"); break;
        case C.SET_VISIBLE: canvas.hidden = !u(0); break;
        case C.SET_CURSOR_IMAGE: cursorImage(u(0), u(1), u(2), u(3), u(4)); break;
        case C.SET_IME_ALLOWED: allowIme(!!u(0)); break;
        case C.SET_IME_AREA: imeArea = [f(0), f(8), f(16), f(24)]; placeIme(); break;
        case C.READ_FILE: readFile(u(0), u(1), u(2)); break;
        case C.REQUEST_MONITOR: monitor(); break;
      }
    }
    requestAnimationFrame(frame);
  };
  requestAnimationFrame(frame);

  // The window as it is, then attached.
  screenSize();
  watchScale();
  const rect = canvas.getBoundingClientRect();
  resized(rect.width * ratio(), rect.height * ratio());
  focused(document.activeElement === canvas);
  emit(E.THEME, P().u32(dark.matches ? 1 : 0));
  Atomics.store(words(), W.ATTACHED, 1);
}
