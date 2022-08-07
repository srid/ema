# Live Server

The "run" command of [[cli]] runs the live-server, as opposed to the "gen" command which generates the static site one off. The live-server is a simple HTTP server that serves the Ema site "on the fly" (without doing O(n) static-site generation). Furthermore, changes to your [[model]] (via [[dynamic]]) will automatically [[hot-reload]] the browser clients to display the new HTML as rendered using the new model value. 

It is not unusual to write Ema apps purely for the live server UX, and disable static site generation entirely.