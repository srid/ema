// Unlike setInnerHtml, this patches the Dom in place
function setHtml(elm, html) {
  window.dispatchEvent(new Event("EMABeforeMorphDOM"));
  Idiomorph.morph(elm, html);
  window.dispatchEvent(new Event("EMABeforeScriptReload"));
  // Re-add <script> tags, because just DOM diff applying is not enough.
  reloadScripts(elm);
  window.dispatchEvent(new Event("EMAHotReload"));
}

// FIXME: This doesn't reliably work across all JS.
// See also the HACK below in one of the invocations.
function reloadScripts(elm) {
  Array.from(elm.querySelectorAll("script")).forEach((oldScript) => {
    // Some scripts, like Tailwind, should not be reloaded (they are not idempotent)
    // Allow the user to skip such scripts with a data-* attribute
    if (oldScript.getAttribute("data-ema-skip") === "true") {
      return;
    }
    const newScript = document.createElement("script");
    Array.from(oldScript.attributes).forEach((attr) =>
      newScript.setAttribute(attr.name, attr.value),
    );
    newScript.appendChild(document.createTextNode(oldScript.innerHTML));
    oldScript.parentNode.replaceChild(newScript, oldScript);
  });
}

// Ema Status indicator
const messages = {
  connected: "Connected",
  reloading: "Reloading",
  connecting: "Connecting to the server",
  disconnected: "Disconnected - try reloading the window",
};
function setIndicators(connected, reloading, connecting, disconnected) {
  const is = { connected, reloading, connecting, disconnected };

  for (const i in is) {
    document.getElementById(`ema-${i}`).style.display = is[i]
      ? "block"
      : "none";
    if (is[i]) document.getElementById("ema-message").innerText = messages[i];
  }
  document.getElementById("ema-indicator").style.display = "block";
}
window.connected = () => setIndicators(true, false, false, false);
window.reloading = () => setIndicators(false, true, false, false);
window.connecting = () => setIndicators(false, false, true, false);
window.disconnected = () => setIndicators(false, false, false, true);
window.hideIndicator = () => {
  document.getElementById("ema-indicator").style.display = "none";
};

// Base URL path - for when the ema site isn't served at "/"
const baseHref = document.getElementsByTagName("base")[0]?.href;
const basePath = baseHref ? new URL(baseHref).pathname : "/";

// Use TLS for websocket iff the current page is also served with TLS
const wsProto = window.location.protocol === "https:" ? "wss://" : "ws://";
const wsUrl = wsProto + window.location.host + basePath;

// WebSocket logic: watching for server changes & route switching
function init(reconnecting) {
  // The route current DOM is displaying
  let routeVisible = document.location.pathname;

  const verb = reconnecting ? "Reopening" : "Opening";
  console.log(`ema: ${verb} conn ${wsUrl} ...`);
  window.connecting();
  let ws = new WebSocket(wsUrl);

  function sendObservePath(path) {
    const relPath = path.startsWith(basePath)
      ? path.slice(basePath.length)
      : path;
    console.debug(`ema: requesting ${relPath}`);
    ws.send(relPath);
  }

  // Call this, then the server will send update *once*. Call again for
  // continous monitoring.
  function watchCurrentRoute() {
    console.log(`ema: ⏿ Observing changes to ${document.location.pathname}`);
    sendObservePath(document.location.pathname);
  }

  function switchRoute(path, hash = "") {
    console.log(`ema: → Switching to ${path + hash}`);
    window.history.pushState({}, "", path + hash);
    sendObservePath(path);
  }

  function scrollToAnchor(hash) {
    console.log(`ema: Scroll to ${hash}`);
    var el = document.querySelector(hash);
    if (el !== null) {
      el.scrollIntoView({ behavior: "smooth" });
    }
  }

  function getAnchorIfOnPage(linkElement) {
    const url = new URL(linkElement.href); // Use URL API for parsing
    return url.host === window.location.host &&
      url.pathname === window.location.pathname &&
      url.hash
      ? url.hash.slice(1) // Return anchor name (slice off '#')
      : null; // Not an anchor on the current page
  }

  function handleRouteClicks(e) {
    const origin = e.target.closest("a");
    if (origin) {
      if (
        window.location.host === origin.host &&
        origin.getAttribute("target") != "_blank"
      ) {
        let anchor = getAnchorIfOnPage(origin);
        if (anchor !== null) {
          // Switching to local anchor
          window.history.pushState({}, "", origin.href);
          scrollToAnchor(window.location.hash);
          e.preventDefault();
        } else {
          // Switching to another route
          switchRoute(origin.pathname, origin.hash);
          e.preventDefault();
        }
      }
    }
  }
  // Intercept route click events, and ask server for its HTML whilst
  // managing history state.
  window.addEventListener(`click`, handleRouteClicks);

  ws.onopen = () => {
    console.log(`ema: ... connected!`);
    // window.connected();
    window.hideIndicator();
    if (!reconnecting) {
      // HACK: We have to reload <script>'s here on initial page load
      // here, so as to make Twind continue to function on the *next*
      // route change. This is not a problem with *subsequent* (ie. 2nd
      // or latter) route clicks, because those have already called
      // reloadScripts at least once.
      reloadScripts(document.documentElement);
    }
    watchCurrentRoute();
  };

  ws.onclose = () => {
    console.log("ema: reconnecting ..");
    window.removeEventListener(`click`, handleRouteClicks);
    window.reloading();
    // Reconnect after as small a time is possible, then retry again.
    // ghcid can take 1s or more to reboot. So ideally we need an
    // exponential retry logic.
    //
    // Note that a slow delay (200ms) may often cause websocket
    // connection error (ghcid hasn't rebooted yet), which cannot be
    // avoided as it is impossible to trap this error and handle it.
    // You'll see a big ugly error in the console.
    setTimeout(function () {
      init(true);
    }, 400);
  };

  ws.onmessage = (evt) => {
    if (evt.data.startsWith("REDIRECT ")) {
      console.log("ema: redirect");
      document.location.href = evt.data.slice("REDIRECT ".length);
    } else if (evt.data.startsWith("SWITCH ")) {
      console.log("ema: switch");
      switchRoute(evt.data.slice("SWITCH ".length));
    } else {
      console.log("ema: ✍ Patching DOM");
      setHtml(document.documentElement, evt.data);
      if (routeVisible != document.location.pathname) {
        // This is a new route switch; scroll up.
        window.scrollTo({ top: 0 });
        routeVisible = document.location.pathname;
      }
      if (window.location.hash) {
        scrollToAnchor(window.location.hash);
      }
    }
  };
  window.onbeforeunload = (evt) => {
    ws.close();
  };
  window.onpagehide = (evt) => {
    ws.close();
  };

  // When the user clicks the back button, resume watching the URL in
  // the addressback, which has the effect of loading it immediately.
  window.onpopstate = function (e) {
    watchCurrentRoute();
  };

  // API for user invocations
  window.ema = {
    switchRoute: switchRoute,
  };
}
