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
      const [, bytes] = file.load_contents(null);
      const text = new TextDecoder('utf-8').decode(bytes);
      const trimmed = text.trim();
      return trimmed.length > 0 ? trimmed : null;
    } catch (_) {
      return null;
    }
  }

  _tick() {
    if (!this._label)
      return GLib.SOURCE_REMOVE;

    const v = this._readCurrentValue();
    this._label.text = v ?? '--';
    return GLib.SOURCE_CONTINUE;
  }

  enable() {
    if (this._button)
      return;

    this._button = new PanelMenu.Button(0.0, 'Trndi Current', false);
    this._label = new St.Label({
      text: '--',
      y_align: Clutter.ActorAlign.CENTER,
      style_class: 'panel-label'
    });

    // Clutter is available globally in GNOME Shell; avoid an extra import.
    this._button.add_child(this._label);
    Main.panel.addToStatusArea(this.uuid, this._button, 0, 'right');

    // Poll every 5 seconds; Trndi writes the file when readings update.
    this._timeoutId = GLib.timeout_add_seconds(GLib.PRIORITY_DEFAULT, 5, this._tick.bind(this));
    this._tick();
  }

  disable() {
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
