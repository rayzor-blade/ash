// The page's agent for the channel "ring_echo": an echo transport. The agent
// name is the file name; this is the whole of a page's side for a transport
// that has nothing to connect to.
import { serve } from "./channel-agent.mjs";

export const start = (ask) =>
  serve(ask, {
    open({ deliver }) {
      return {
        // Answer after a short delay so the program's recv has to wait for it.
        send: (frame) => setTimeout(() => deliver(frame), 5),
        close() {},
      };
    },
  });
