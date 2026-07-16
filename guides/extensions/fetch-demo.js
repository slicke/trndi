/* fetch() demo
(c) Trndi sample extension
@perms net
*/
// Shows the fetch() API: a browser-style HTTP client for extensions.
// Requests run on a background thread, so Trndi stays responsive even
// when the server is slow. Requires the `net` permission.
//
// Supported: GET/POST, request headers, string bodies, status/ok,
// case-insensitive response headers, text()/json().
// Not supported: other methods, streaming, AbortController, binary bodies.

async function demoGet() {
  // GET + JSON: look up the latest Trndi release on GitHub
  const res = await fetch("https://api.github.com/repos/slicke/trndi/releases/latest", {
    headers: { "Accept": "application/vnd.github+json" }
  });

  console.log("status: " + res.status + " ok: " + res.ok);
  // Header names are matched case-insensitively
  console.log("content-type: " + res.headers.get("Content-Type"));

  if (!res.ok) {
    // Non-2xx responses resolve, just like in the browser - check res.ok
    console.log("GitHub answered " + res.status + ": " + await res.text());
    return;
  }

  const release = await res.json();
  console.log("Latest Trndi release: " + release.tag_name);
}

async function demoPost() {
  // POST: send a JSON body. Stringify yourself and set the content type
  // (string bodies default to text/plain, per the fetch spec).
  const res = await fetch("https://httpbin.org/post", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ hello: "from Trndi" })
  });
  const echoed = await res.json();
  console.log("httpbin echoed back: " + echoed.data);
}

// fetch() rejects with a TypeError on transport failure (DNS, refused,
// timeout...) - wrap awaits in try/catch just like in a browser.
(async () => {
  try {
    await demoGet();
    await demoPost();
  } catch (e) {
    console.log("request failed: " + e);
  }
})();
