import Gio from 'gi://Gio';
import GLib from 'gi://GLib';
import St from 'gi://St';
import Clutter from 'gi://Clutter';
import Pango from 'gi://Pango';

import * as Main from 'resource:///org/gnome/shell/ui/main.js';
import * as PanelMenu from 'resource:///org/gnome/shell/ui/panelMenu.js';
import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';

export default class TrndiCurrentExtension extends Extension {
  constructor(metadata) {
    super(metadata);
    this._button = null;
    this._label = null;
    this._timeoutId = null;
    this._lastDebugKey = null;
  }

  _staleAfterSeconds() {
    // Default hide threshold for the cache file mtime.
    // If the cache provides an explicit threshold, we use that instead.
    return 11 * 60;
  }

  _cachePath() {
    const cacheDir = GLib.get_user_cache_dir();
    return GLib.build_filenamev([cacheDir, 'trndi', 'current.txt']);
  }

  _readCurrentState() {
    const path = this._cachePath();
    try {
      const file = Gio.File.new_for_path(path);
      if (!file.query_exists(null))
        return null;

      // We'll decide whether to hide based on cache file mtime, using the
      // freshness threshold (line 3) when available.
      const info = file.query_info('time::modified', Gio.FileQueryInfoFlags.NONE, null);
      const mtime = info.get_attribute_uint64('time::modified');
      const now = Math.floor(Date.now() / 1000);
      const mtimeAge = (mtime > 0) ? (now - mtime) : 0;

      const stream = file.read(null);
      const dis = new Gio.DataInputStream({ base_stream: stream });
      const [line1] = dis.read_line_utf8(null);
      const [line2] = dis.read_line_utf8(null);
      const [line3] = dis.read_line_utf8(null);
      dis.close(null);

      if (!line1)
        return null;

      // Compact ranges for panel fit: "70 - 180" -> "70-180".
      const trimmed = line1.trim().replace(/\s*-\s*/g, '-');
      const value = trimmed.length > 0 ? trimmed : null;
      if (!value)
        return null;

      // Newer Trndi writes:
      // line1: value
      // line2: reading epoch seconds
      // line3: freshness threshold minutes
      let isStale = false;
      let epoch = line2 ? parseInt(String(line2).trim(), 10) : NaN;
      const freshMin = line3 ? parseInt(String(line3).trim(), 10) : NaN;
      if (!Number.isNaN(epoch) && epoch > 0 && !Number.isNaN(freshMin) && freshMin > 0) {
        const now = Math.floor(Date.now() / 1000);

        // Accept both seconds and milliseconds epoch.
        if (epoch > 1000000000000)
          epoch = Math.floor(epoch / 1000);

        // If epoch is in the future (timezone bug in old writers), try to
        // correct by subtracting a whole-hour offset.
        if (epoch > (now + 60)) {
          const delta = epoch - now;
          const hours = Math.round(delta / 3600);
          if (hours !== 0)
            epoch = epoch - (hours * 3600);
        }

        isStale = (now - epoch) > (freshMin * 60);
      }

      // Hide when the cache file itself is old (e.g. Trndi not running).
      // Use the freshness threshold when provided; otherwise fall back to 11 min.
      const hideAfterSeconds = (!Number.isNaN(freshMin) && freshMin > 0) ? (freshMin * 60) : this._staleAfterSeconds();
      if (mtimeAge > 0 && mtimeAge > hideAfterSeconds)
        return null;

      return { value, isStale, epoch, freshMin };
    } catch (_) {
      return null;
    }
  }

  _tick() {
    const state = this._readCurrentState();
    if (!state) {
      if (this._button) {
        this._button.destroy();
        this._button = null;
        this._label = null;
      }
      return GLib.SOURCE_CONTINUE;
    }

    if (!this._button) {
      this._button = new PanelMenu.Button(0.0, 'Trndi Current', false);
      this._label = new St.Label({
        text: '',
        y_align: Clutter.ActorAlign.CENTER,
        style_class: 'panel-label'
      });
      // Never show ellipsis (we rely on shorter formatting + panel width).
      try {
        this._label.clutter_text.ellipsize = Pango.EllipsizeMode.NONE;
      } catch (_) {
      }
      this._button.add_child(this._label);
      Main.panel.addToStatusArea('trndiCurrent', this._button, 0, 'right');
    }

    // GNOME Shell panel rendering for strike-through is inconsistent across
    // versions/themes. Use a clear, robust stale indicator instead.
    if (state.isStale) {
      this._label.set_text('--');
    } else {
      this._label.set_text(state.value);
    }

    // Low-noise debug: log only when value/stale changes.
    try {
      const dbg = `${state.value}|${state.isStale ? 'stale' : 'fresh'}|${state.epoch ?? ''}|${state.freshMin ?? ''}`;
      if (dbg !== this._lastDebugKey) {
        this._lastDebugKey = dbg;
        log(`[TrndiCurrent] ${dbg}`);
      }
    } catch (_) {
    }
    return GLib.SOURCE_CONTINUE;
  }

  enable() {
    if (this._button)
      return;

    try {
      log(`[TrndiCurrent] enable (${this.metadata?.uuid ?? 'unknown'})`);
    } catch (_) {
    }

    // Poll every 5 seconds; Trndi writes the file when readings update.
    this._timeoutId = GLib.timeout_add_seconds(GLib.PRIORITY_DEFAULT, 5, this._tick.bind(this));
    this._tick();
  }

  disable() {
    try {
      log('[TrndiCurrent] disable');
    } catch (_) {
    }
    if (this._timeoutId) {
      GLib.source_remove(this._timeoutId);
      this._timeoutId = null;
    }

    if (this._button) {
      this._button.destroy();
      this._button = null;
      this._label = null;
    }
  }
}
