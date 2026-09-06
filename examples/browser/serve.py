#!/usr/bin/env python3
"""Serve this directory with the two headers a shared memory needs.

`python3 -m http.server` is enough for the single-threaded demo and not for
the threaded one. A threads build imports a shared memory, a shared memory is
a `SharedArrayBuffer`, and a page only has `SharedArrayBuffer` when it is
cross-origin isolated -- which it is only when EVERY response carries:

    Cross-Origin-Opener-Policy: same-origin
    Cross-Origin-Embedder-Policy: require-corp

Without them `new WebAssembly.Memory({shared: true})` throws at the point the
module is instantiated, which reads as the module being broken rather than the
server being wrong. That is the whole reason this file exists.

    ./examples/browser/serve.py            # port 8731
    ./examples/browser/serve.py 9000
"""

import functools
import http.server
import pathlib
import sys


class Isolated(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        # A worker fetching its own module must not be served a stale one
        # while the demo is being rebuilt.
        self.send_header("Cache-Control", "no-store")
        super().end_headers()


def main() -> int:
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8731
    here = pathlib.Path(__file__).resolve().parent
    handler = functools.partial(Isolated, directory=str(here))
    with http.server.ThreadingHTTPServer(("127.0.0.1", port), handler) as httpd:
        print(f"serving {here} on http://127.0.0.1:{port} (cross-origin isolated)")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
