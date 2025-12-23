import Gio from 'gi://Gio';
import GLib from 'gi://GLib';
import St from 'gi://St';
import Clutter from 'gi://Clutter';

import * as Main from 'resource:///org/gnome/shell/ui/main.js';
import * as PanelMenu from 'resource:///org/gnome/shell/ui/panelMenu.js';
import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';

export default class TrndiCurrentExtension extends Extension {
  constructor(metadata) {
    super(metadata);
    this._button = null;
    this._label = null;
    this._timeoutId = null;
  }

  _cachePath() {
    const cacheDir = GLib.get_user_cache_dir();
    return GLib.build_filenamev([cacheDir, 'trndi', 'current.txt']);
  }

  _readCurrentValue() {
    const path = this._cachePath();
    try {
      const file = Gio.File.new_for_path(path);
      if (!file.query_exists(null))
        return null;

      // If the file hasn't been updated recently, treat it as "not running".
      // This avoids showing a stale value if Trndi was killed/crashed.
      const info = file.query_info('time::modified', Gio.FileQueryInfoFlags.NONE, null);
      const mtime = info.get_attribute_uint64('time::modified');
      const now = Math.floor(Date.now() / 1000);
      if (mtime > 0 && (now - mtime) > 120)
        return null;

      const stream = file.read(null);
      const dis = new Gio.DataInputStream({ base_stream: stream });
      const [line] = dis.read_line_utf8(null);
      dis.close(null);

      if (!line)
        return null;
      const trimmed = line.trim();
      return trimmed.length > 0 ? trimmed : null;
    } catch (_) {
      return null;
    }
  }

  _tick() {
    const v = this._readCurrentValue();
    if (!v) {
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
      this._button.add_child(this._label);
      Main.panel.addToStatusArea('trndiCurrent', this._button, 0, 'right');
    }

    this._label.set_text(v);
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
