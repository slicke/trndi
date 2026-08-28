/**
 * Type declarations for the Trndi extensions API (v1 `Trndi.*` methods,
 * the additive v2 facade, and the engine's global helpers).
 *
 * Using these types
 * -----------------
 * JavaScript with checking (recommended): copy this file into your extensions
 * folder next to your `.js` files and add a `jsconfig.json` beside them:
 *
 *     { "compilerOptions": { "checkJs": true, "target": "ES2022", "lib": ["ES2023"] } }
 *
 * Your editor (VS Code etc.) will then type-check and autocomplete the Trndi
 * API inside plain `.js` extensions.
 *
 * TypeScript: author a `.ts` file next to this one and compile it to the
 * extensions folder — Trndi itself only loads `.js`:
 *
 *     tsc --target ES2022 --lib ES2023 --strict myextension.ts
 *
 * Rules for the emitted file:
 * - Keep the manifest block comment (the one with `@name` and `@perms`) as
 *   the very first thing in the .ts file; tsc keeps leading comments by
 *   default (do not enable `removeComments`), so it stays first in the
 *   emitted .js.
 * - Extensions are evaluated as plain global scripts: do not use
 *   `import`/`export` (they would make tsc emit module code that the engine
 *   does not resolve for bare names).
 * - Use `lib: ["ES2023"]` without "DOM". The globals below (console, timers,
 *   fetch-style networking, URL, crypto, ...) are Trndi's own subset and
 *   would conflict with the DOM lib's declarations.
 *
 * Callbacks: the classic way to receive events is declaring a global such as
 * `function levelCallback(level, previous) {...}`. Those bare declarations
 * cannot be type-checked against this file; prefer `Trndi.on("level", ...)`,
 * which is fully typed via {@link TrndiEventMap}.
 *
 * Permissions: `data`, `ui` and `timers` are always granted. `net`, `exec`
 * and `settings` must be declared in the extension's `@perms` manifest line
 * and approved by the user — members gated on them are typed as optional or
 * documented with the required permission. See guides/Extensions.md.
 */

/** Blood glucose reading as returned by `Trndi.getCurrentReading`/`getReadings`. */
interface TrndiReading {
  /** Value in the user's display unit (mg/dL or mmol/L, see `Trndi.getUnit()`). */
  value_system: number;
  /** Value in mg/dL (integer). */
  value_mgdl: number;
  /** Value in mmol/L. */
  value_mmol: number;
  /** Change since the previous reading in mg/dL (integer). */
  delta_mgdl: number;
  /** Change since the previous reading in mmol/L. */
  delta_mmol: number;
  /** Trend arrow: ↑, ↗, →, ↘, ↓ (or similar glyphs). */
  direction: string;
  /** Delphi TDateTime float, kept for compatibility — prefer `timestamp_ms`. */
  timestamp: number;
  /** Unix epoch in milliseconds; `new Date(r.timestamp_ms)` works directly. */
  timestamp_ms: number;
  /** Age of the reading in seconds. */
  age_seconds: number;
}

/** CGM thresholds from `Trndi.getLimits()`. */
interface TrndiLimits {
  low_mgdl: number;
  high_mgdl: number;
  low_mmol: number;
  high_mmol: number;
  /** Target-range lower bound (0 when the backend has no range support). */
  low_range_mgdl: number;
  /** Target-range upper bound (500 when the backend has no range support). */
  high_range_mgdl: number;
  low_range_mmol: number;
  high_range_mmol: number;
}

/** Result of `Trndi.getStatistics()`; values are in the user's display unit. */
interface TrndiStatistics {
  mean: number;
  median: number;
  stdDev: number;
  /** Coefficient of variation in percent; below 36 is considered stable. */
  cv: number;
  /** Percentage of analyzed readings inside the target range. */
  timeInRange: number;
  timeAbove: number;
  timeBelow: number;
  readingCount: number;
}

/**
 * One prediction from `Trndi.predictReadings`:
 * `[value in display unit, mg/dL, mmol/L, TDateTime, Unix epoch ms]`.
 */
type TrndiPrediction = [
  valueSystem: number,
  valueMgdl: number,
  valueMmol: number,
  timestamp: number,
  timestampMs: number
];

/** The `[display unit, mg/dL, mmol/L]` array appended to some callbacks. */
type TrndiBGTuple = [valueSystem: number, valueMgdl: number, valueMmol: number];

/** Glucose classification reported to the `level` event / `levelCallback`. */
type TrndiLevel = "high" | "low" | "normal" | "range-high" | "range-low" | "stale";

/** Permission group names used in `@perms` and `Trndi.permissions`. */
type TrndiPermission = "data" | "ui" | "timers" | "net" | "exec" | "settings";

/**
 * Event names accepted by `Trndi.on`/`Trndi.off` and the signature each
 * listener is called with (identical to the equivalent callback global).
 */
interface TrndiEventMap {
  /**
   * A finalized reading update (global: `updateCallback`). Called with the
   * update time as a locale-formatted string and the current reading as a
   * `[system, mgdl, mmol]` array.
   */
  reading: (time: string, reading: TrndiBGTuple) => void;
  /**
   * A reading was fetched (global: `fetchCallback`). Values arrive
   * pre-formatted as display strings.
   */
  fetch: (
    mgdl: string,
    mmol: string,
    deltaMgdl: string,
    deltaMmol: string,
    hasData: boolean
  ) => void;
  /**
   * Glucose classification after every update (global: `levelCallback`).
   * Fires on every update, not only on changes; `previous` is "" on the
   * first report after startup.
   */
  level: (level: TrndiLevel, previous: TrndiLevel | "") => void;
  /**
   * The periodic clock flash (global: `clockView`). Return a non-empty
   * string to show it instead of the clock; return nothing (or "") to keep
   * the normal clock. Called with the time as a locale-formatted string and
   * the current reading as a `[system, mgdl, mmol]` array.
   */
  clock: (time: string, reading: TrndiBGTuple) => string | void;
  /**
   * A trend dot was clicked (global: `dotClicked`). `open` is true when the
   * dot now shows its value; `time` is encoded as hour*100 + minute.
   */
  dot: (open: boolean, mgdl: number, mmol: number, time: number) => void;
  /**
   * A UX element was clicked (global: `uxClick`). `element` is "tir",
   * "no-reading" or "range"; extra values depend on the element. Return
   * false to suppress Trndi's own dialog.
   */
  uxclick: (element: string, ...values: unknown[]) => boolean | void;
  /**
   * The extension is about to be unloaded (global: `unloadCallback`) —
   * reload or app exit. Keep it fast and synchronous; timers and promises
   * will not fire afterwards.
   */
  unload: () => void;
}

/** Metadata about the running extension, from the v2 facade. */
interface TrndiApiInfo {
  version: "2.0";
  /** Stable identifier derived from the extension's file name. */
  extensionId: string;
  /** Granted permission groups. */
  permissions: TrndiPermission[];
  capabilities: { storage: boolean; network: boolean; exec: boolean };
}

/** Case-insensitive response-header lookup for `Trndi.net.fetch`. */
interface TrndiFetchHeaders {
  get(name: string): string | null;
  has(name: string): boolean;
  keys(): string[];
}

/** Response object resolved by `Trndi.net.fetch`. Bodies are buffered. */
interface TrndiFetchResponse {
  status: number;
  /** True for 2xx. Non-2xx responses resolve — check this, like browser fetch. */
  ok: boolean;
  /** Final URL after redirects. */
  url: string;
  redirected: boolean;
  headers: TrndiFetchHeaders;
  text(): Promise<string>;
  json(): Promise<any>;
}

/** Options for `Trndi.net.fetch`. Only GET and POST are supported. */
interface TrndiFetchInit {
  method?: "GET" | "POST" | "get" | "post";
  headers?: Record<string, string>;
  /** Request body; stringify JSON yourself. */
  body?: string;
  /**
   * Milliseconds before the promise rejects with a TypeError containing
   * "timeout". Non-standard; off by default (transport limits still apply).
   */
  timeout?: number;
}

declare const Trndi: {
  // ---- v2 facade -------------------------------------------------------

  api: TrndiApiInfo;
  permissions: {
    has(permission: TrndiPermission | string): boolean;
    /** Throws unless the permission is granted; returns true otherwise. */
    require(permission: TrndiPermission | string): true;
  };
  data: {
    /** Current reading, or false when none is available. */
    current(): TrndiReading | false;
    /** Cached history, newest first, optionally capped and/or time-filtered. */
    readings(options?: { limit?: number; minutes?: number }): TrndiReading[];
    limits(): TrndiLimits;
    statistics(options?: { minutes?: number }): TrndiStatistics;
    predict(options?: { count?: number }): {
      values: TrndiPrediction[];
      /** 0..1 fit confidence of this prediction run. */
      confidence: number;
    };
  };
  /** Event names mapped to their callback-global names. */
  events: Record<keyof TrndiEventMap, string>;
  /**
   * Register a listener; returns it (handy for `off`). Several listeners can
   * coexist with a classic callback global declared in the same script.
   */
  on<E extends keyof TrndiEventMap>(event: E, listener: TrndiEventMap[E]): TrndiEventMap[E];
  off<E extends keyof TrndiEventMap>(event: E, listener: TrndiEventMap[E]): void;
  /** Networking; present only with the `net` permission. */
  net?: {
    /** Threaded fetch-style HTTP. Rejects with TypeError on transport failure/timeout. */
    fetch(url: string, init?: TrndiFetchInit): Promise<TrndiFetchResponse>;
  };
  /** Private key/value storage; present only with the `settings` permission. */
  storage?: {
    /** Stored string, or false when the key is missing. */
    get(key: string): string | false;
    set(key: string, value: string | number | boolean): void;
    remove(key: string): void;
    /** Parsed value, or `dflt` (default null) on missing/unparsable keys. */
    getJSON(key: string, dflt?: any): any;
    setJSON(key: string, value: any): void;
  };

  // ---- Dialogs and logging (ui) ---------------------------------------

  /** Modal alert dialog. */
  alert(message: string): void;
  /** Modal yes/no question. */
  confirm(message: string): boolean;
  /** Modal input field; returns the entered string. */
  prompt(caption: string, description: string, defaultValue: string): string;
  /** Modal list picker; returns the zero-based index of the chosen option. */
  select(caption: string, description: string, ...options: string[]): number;
  /** Same as `console.log`. */
  log(message: string): void;
  /** HTML content box: window title, title, description, HTML body, window scale. */
  htmlMsg(windowTitle: string, title: string, description: string, html: string, scale: number): void;
  /** HTML dialog: window title, HTML body, window scale. */
  htmlDlg(windowTitle: string, html: string, scale: number): void;
  /** HTML yes/no dialog; true when the user answers yes. */
  htmlYesNo(windowTitle: string, html: string, scale: number): boolean;
  /** System notification: `attention(message)` or `attention(title, message)`. */
  attention(message: string): void;
  attention(title: string, message: string): void;
  /**
   * Non-modal, stay-on-top notification window. Repeated calls append to the
   * same window; pass an id as first argument to group into separate windows
   * (ids are private to the calling extension).
   */
  notify(title: string, message: string): void;
  notify(id: string, title: string, message: string): void;

  // ---- Appearance and audio (ui) --------------------------------------

  /**
   * Set a property on a named UI control. Supported properties: "color",
   * "font-color" (HTML color strings) and "font" (font name).
   */
  uxProp(component: string, property: "color" | "font-color" | "font", value: string): void;
  /** Windows reading-badge size: 0.1–1.0 of the app icon, optional font size. */
  setBadgeSize(scale: number, fontSize?: number): boolean;
  /** Trend-dot scale factor (integer; 2 = 2x). */
  setDotSize(scale: number): boolean;
  /** Vertical trend-dot position multiplier; negative = up, positive = down. */
  setDotAdjust(multiplier: number): boolean;
  /**
   * Reading colors as HTML color strings. 3 args set backgrounds (ok, high,
   * low); 6 add text colors (ok, high, low); 10 add the custom range levels
   * (background high, background low, text high, text low). Other argument
   * counts are rejected (returns false).
   */
  setLevelColor(okBg: string, highBg: string, lowBg: string): boolean;
  setLevelColor(
    okBg: string, highBg: string, lowBg: string,
    okText: string, highText: string, lowText: string
  ): boolean;
  setLevelColor(
    okBg: string, highBg: string, lowBg: string,
    okText: string, highText: string, lowText: string,
    rangeHighBg: string, rangeLowBg: string, rangeHighText: string, rangeLowText: string
  ): boolean;
  /** Play an audio file from an absolute path. */
  playSound(path: string): void;
  /** Read a text aloud (TTS). */
  sayText(text: string): void;
  /**
   * Show the clock (when enabled) every `intervalMs` for `durationMs`
   * milliseconds. The two values must differ.
   */
  setClockInterval(intervalMs: number, durationMs: number): void;

  // ---- Readings and metadata (data) -----------------------------------

  /** App language code, such as "en" or "sv". */
  getLocale(): string;
  /** The user's display unit. */
  getUnit(): "mg/dL" | "mmol/L";
  /** Username in multi-user mode ("" for the default user), false otherwise. */
  getCurrentUser(): string | false;
  /** Nickname in multi-user mode, false otherwise. */
  getCurrentNickname(): string | false;
  /** Trndi build number, e.g. "200". */
  getBuild(): string;
  /** Name of the active backend, e.g. "NightScout v3". */
  getCurrentAPI(): string;
  /** Current value in mmol/L (true) or mg/dL (false); false when unavailable. */
  getReading(mmol: boolean): number | false;
  /** Current reading with metadata, or false when unavailable. */
  getCurrentReading(): TrndiReading | false;
  /** Cached history, newest first. Omitted or <= 0 count returns everything. */
  getReadings(count?: number): TrndiReading[];
  getLimits(): TrndiLimits;
  /** Statistics over the last `minutes` (default 1440 = 24 h). */
  getStatistics(minutes?: number): TrndiStatistics;
  /** Scheduled basal rate in U/hr, or false when the backend has none. */
  getBasalRate(): number | false;
  /**
   * Linear-trend predictions (default 3, max 20). Empty array when fewer
   * than 3 recent readings exist.
   */
  predictReadings(count?: number): TrndiPrediction[];
  /** 0..1 fit confidence of the most recent `predictReadings` run. */
  predictionConfidence(): number;

  // ---- Settings (requires the `settings` permission) ------------------

  /** Settings value as a string, or false when the key does not exist. */
  getSetting?(key: string): string | false;
  /**
   * Write a settings value. Keys outside `extval.*` prompt the user for
   * approval; prefer `Trndi.storage` for private extension state.
   */
  setSetting?(key: string, value: string): void;
  /** Max minutes and max reading count to fetch from the backend. */
  setTimeAndRange?(minutes: number, count: number): void;
  /** Minutes before a reading counts as old (clamped to >= 6). Use with care. */
  setOverrideThresholdMinutes?(minutes: number): void;
};

// ---- Globals provided by the extension engine -------------------------

/** Modal alert dialog (same as `Trndi.alert`). */
declare function alert(message: string): void;

/**
 * Run a local program; requires the `exec` permission (otherwise the global
 * does not exist at runtime). Resolves true on success.
 */
declare function runCMD(command: string, argString?: string, delimiter?: string): Promise<boolean>;

/**
 * Set the high/low thresholds and optionally the custom range bounds, in the
 * user's display unit. Requires the `settings` permission (otherwise the
 * global does not exist at runtime).
 */
declare function setLimits(low: number, high: number, lowRange?: number, highRange?: number): Promise<void>;

declare function setTimeout(callback: (...args: any[]) => void, delay?: number, ...args: any[]): number;
declare function setInterval(callback: (...args: any[]) => void, delay?: number, ...args: any[]): number;
declare function clearTimeout(timerId: number): void;
declare function clearInterval(timerId: number): void;
declare function queueMicrotask(callback: () => void): void;

declare var console: {
  /** Shows a popup per call; non-string arguments are JSON-stringified. */
  log(...data: any[]): void;
  /** Buffer a message (shared across extensions) without showing a popup. */
  push(...data: any[]): void;
  /** Show and clear the buffered messages; returns the number of lines shown. */
  pop(): number;
  /** @deprecated Alias of `console.pop()`. */
  logs(): number;
  /** Buffered level logging: like `push` with an `[error]` prefix. */
  error(...data: any[]): void;
  warn(...data: any[]): void;
  info(...data: any[]): void;
  debug(...data: any[]): void;
};

/** Base64-encode a Latin1 string (throws TypeError above U+00FF). */
declare function btoa(data: string): string;
/** Base64-decode; throws TypeError on invalid input. */
declare function atob(data: string): string;

/** Deep clone (objects, arrays, Date, RegExp, Map, Set, typed arrays, cycles). */
declare function structuredClone<T>(value: T): T;

declare var performance: {
  /** Monotonic milliseconds (µs precision); never jumps with clock changes. */
  now(): number;
  timeOrigin: number;
};

declare var crypto: {
  /** Fill an integer typed array with OS randomness; returns the same array. */
  getRandomValues<T extends Int8Array | Uint8Array | Uint8ClampedArray | Int16Array | Uint16Array | Int32Array | Uint32Array | BigInt64Array | BigUint64Array>(array: T): T;
  /** RFC 4122 version-4 UUID string. */
  randomUUID(): string;
};

declare class URLSearchParams {
  constructor(
    init?: string | Record<string, string> | [string, string][] | URLSearchParams
  );
  get(name: string): string | null;
  getAll(name: string): string[];
  set(name: string, value: string): void;
  append(name: string, value: string): void;
  delete(name: string): void;
  has(name: string): boolean;
  sort(): void;
  forEach(callback: (value: string, name: string, parent: URLSearchParams) => void): void;
  keys(): IterableIterator<string>;
  values(): IterableIterator<string>;
  entries(): IterableIterator<[string, string]>;
  [Symbol.iterator](): IterableIterator<[string, string]>;
  toString(): string;
}

/** Hierarchical URLs only (no mailto:/data:); invalid URLs throw TypeError. */
declare class URL {
  constructor(url: string, base?: string);
  protocol: string;
  username: string;
  password: string;
  hostname: string;
  port: string;
  host: string;
  readonly origin: string;
  pathname: string;
  search: string;
  readonly searchParams: URLSearchParams;
  hash: string;
  href: string;
  toString(): string;
}

declare class TextEncoder {
  /** UTF-8 encode a string to bytes. */
  encode(input?: string): Uint8Array;
  readonly encoding: "utf-8";
}

declare class TextDecoder {
  /** UTF-8 only; other labels throw RangeError. */
  constructor(label?: string, options?: { fatal?: boolean });
  decode(input?: Uint8Array | ArrayBuffer | number[]): string;
  readonly encoding: "utf-8";
  readonly fatal: boolean;
}
