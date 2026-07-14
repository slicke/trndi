#!/usr/bin/env node
/*
 * Trndi — Medtronic CareLink (Care Partner) login helper
 *
 * Copyright (c) Björn Lindh — https://github.com/slicke/trndi
 * Distributed under the GNU General Public License, Version 3.
 *
 * Medtronic gates the CareLink login behind a CAPTCHA and only allows a
 * mobile-app custom-scheme redirect (com.medtronic.carepartner:/sso), so the
 * authorization code can never be seen by a plain browser or a headless
 * request. This helper opens a real browser window for you to sign in, catches
 * the redirect the moment it happens, exchanges the code for tokens, and prints
 * the resulting token data (the same JSON Trndi stores as its credential).
 *
 * It replaces the older Python + Selenium + OpenSSL helper: the current
 * (Auth0) login needs none of that — just Node and Puppeteer.
 *
 * Usage:
 *   npm install          # once, pulls in Puppeteer (bundles a browser)
 *   node carelink-login.mjs           # EU / rest-of-world (default)
 *   node carelink-login.mjs --us      # USA region
 *
 * The JSON blob is printed to stdout; all progress/log text goes to stderr, so
 * you can also capture just the token with:  node carelink-login.mjs > token.json
 *
 * Paste the printed JSON into Trndi's CareLink credential field. Treat it like
 * a password.
 */

import puppeteer from 'puppeteer';

// One discovery URL serves every region; the app uses this exact Android UA.
const DISCOVERY_URL = 'https://clcloud.minimed.eu/connect/carepartner/v13/discover/android/3.6';
const ANDROID_UA = 'Dalvik/2.1.0 (Linux; U; Android 10; Nexus 5X Build/QQ3A.200805.001)';
const LOGIN_TIMEOUT_MS = 5 * 60 * 1000; // 5 minutes to complete the browser login

const region = process.argv.includes('--us') ? 'US' : 'EU';

const log = (...a) => console.error('[carelink-login]', ...a);

async function getJson(url) {
  const r = await fetch(url, { headers: { 'User-Agent': ANDROID_UA, Accept: 'application/json' } });
  if (!r.ok) throw new Error(`GET ${url} -> HTTP ${r.status}`);
  return r.json();
}

/** Query parameters of any URL, including non-http custom schemes. */
function queryParams(url) {
  const i = url.indexOf('?');
  const q = i >= 0 ? url.slice(i + 1) : '';
  return new URLSearchParams(q.split('#')[0]);
}

/** Resolve the region's Auth0 login configuration from discovery + SSO config. */
async function resolveConfig() {
  const disc = await getJson(DISCOVERY_URL);
  const cp = (disc.CP || []).find((c) => String(c.region || '').toUpperCase() === region);
  if (!cp) throw new Error(`No CP entry for region ${region} in the discovery document`);

  const ssoKey = cp.UseSSOConfiguration;
  const ssoUrl = cp[ssoKey];
  if (!ssoUrl) throw new Error('Could not find the SSO configuration URL');

  const sso = await getJson(ssoUrl);
  const server = sso.server || {};
  let base = `https://${server.hostname}:${server.port || 443}`;
  if (server.prefix && server.prefix !== '/') base += '/' + server.prefix;

  const client = sso.client;
  const ep = sso.system_endpoints;
  if (!client || !ep) {
    throw new Error(
      'This region is not on the Auth0 login (no client/system_endpoints in the SSO config). ' +
      'Only the Auth0 flow is supported.'
    );
  }

  return {
    authorizeUrl: base + ep.authorization_endpoint_path,
    tokenUrl: base + ep.token_endpoint_path,
    clientId: client.client_id,
    redirectUri: client.redirect_uri,
    scope: client.scope,
    audience: client.audience,
  };
}

function buildAuthorizeUrl(cfg, state) {
  const p = new URLSearchParams({
    client_id: cfg.clientId,
    response_type: 'code',
    scope: cfg.scope,
    redirect_uri: cfg.redirectUri,
    audience: cfg.audience,
    state,
  });
  return `${cfg.authorizeUrl}?${p.toString()}`;
}

/**
 * Open a browser to the authorize URL and resolve with the authorization code.
 * The final step is a 302 to the custom-scheme redirect_uri; we catch it from
 * either the response headers or the (blocked) navigation request before the
 * browser gives up on the unknown scheme.
 */
async function captureCode(cfg, authorizeUrl) {
  const browser = await puppeteer.launch({
    headless: false,
    defaultViewport: null,
    args: ['--no-sandbox', '--disable-blink-features=AutomationControlled'],
  });

  try {
    const page = (await browser.pages())[0] || (await browser.newPage());

    return await new Promise((resolve, reject) => {
      let done = false;
      const timer = setTimeout(() => finish(new Error('Timed out waiting for login (5 minutes).')), LOGIN_TIMEOUT_MS);

      function finish(err, value) {
        if (done) return;
        done = true;
        clearTimeout(timer);
        page.off('response', onResponse);
        page.off('request', onRequest);
        if (err) reject(err); else resolve(value);
      }

      function tryUrl(u) {
        if (!u || !u.startsWith(cfg.redirectUri)) return false;
        const params = queryParams(u);
        const code = params.get('code');
        if (params.get('error')) {
          finish(new Error(`Login failed: ${params.get('error_description') || params.get('error')}`));
          return true;
        }
        if (code) {
          finish(null, { code, state: params.get('state') });
          return true;
        }
        return false;
      }

      const onResponse = (resp) => {
        const s = resp.status();
        if (s >= 300 && s < 400) tryUrl(resp.headers().location || '');
      };
      const onRequest = (req) => { tryUrl(req.url()); };

      page.on('response', onResponse);
      page.on('request', onRequest);

      log('opening browser — sign in with your Care Partner account and solve the CAPTCHA…');
      // Navigation to the custom scheme rejects; that's expected and handled above.
      page.goto(authorizeUrl, { waitUntil: 'domcontentloaded' }).catch(() => {});
    });
  } finally {
    await browser.close().catch(() => {});
  }
}

async function exchangeCode(cfg, code) {
  const body = new URLSearchParams({
    grant_type: 'authorization_code',
    client_id: cfg.clientId,
    code,
    redirect_uri: cfg.redirectUri,
  });
  const r = await fetch(cfg.tokenUrl, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded', 'User-Agent': ANDROID_UA },
    body: body.toString(),
  });
  const text = await r.text();
  if (!r.ok) throw new Error(`Token exchange failed: HTTP ${r.status} ${text}`);
  return JSON.parse(text);
}

function randomState() {
  return [...crypto.getRandomValues(new Uint8Array(16))].map((b) => b.toString(16).padStart(2, '0')).join('');
}

async function main() {
  log(`region: ${region}${region === 'EU' ? ' (use --us for the USA region)' : ''}`);

  const cfg = await resolveConfig();
  const state = randomState();
  const authorizeUrl = buildAuthorizeUrl(cfg, state);

  const { code, state: returnedState } = await captureCode(cfg, authorizeUrl);
  if (returnedState && returnedState !== state) log('warning: login state did not match (continuing anyway)');

  log('got the login code, exchanging it for tokens…');
  const tok = await exchangeCode(cfg, code);

  if (!tok.access_token || !tok.refresh_token) {
    throw new Error('The token response did not contain access_token/refresh_token');
  }

  // The same shape as logindata.json / what Trndi stores as its credential.
  const blob = {
    access_token: tok.access_token,
    refresh_token: tok.refresh_token,
    ...(tok.id_token ? { id_token: tok.id_token } : {}),
    ...(tok.scope ? { scope: tok.scope } : {}),
    client_id: cfg.clientId,
  };

  // Only the JSON goes to stdout, so it can be piped or copied cleanly.
  // Compact (single-line): the credential is stored in single-line settings
  // stores (ini on Linux) and pasted into single-line edit fields, so it must
  // never contain line breaks.
  console.log(JSON.stringify(blob));
  log('success — copy the JSON above into Trndi\'s CareLink credential field. Treat it like a password.');
}

main().catch((e) => {
  log('ERROR:', e.message || e);
  process.exit(1);
});
