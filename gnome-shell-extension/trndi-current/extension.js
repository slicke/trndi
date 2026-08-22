// SPDX-License-Identifier: GPL-3.0-only
//
// Part of Trndi - https://github.com/slicke/trndi
// Copyright (c) Björn Lindh
//
// This program is distributed under the terms of the GNU General Public
// License, Version 3, as published by the Free Software Foundation. See
// LICENSE.md in the Trndi repository, or <https://www.gnu.org/licenses/gpl-3.0>.
//
// MEDICAL DISCLAIMER: Trndi is NOT a medical device. Readings shown may be
// delayed, inaccurate, or unavailable — never make medical decisions based on
// them. See DISCLAIMER.md.
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
    this._settings = null;
    this._settingsChangedIds = [];
  }

  // Settings mirror the KDE plasmoid's config. A manual copy of the extension
  // may lack a compiled schema, so fall back to the historical defaults
  // instead of failing to load.
  _settingBool(key, fallback) {
    try {
      return this._settings ? this._settings.get_boolean(key) : fallback;
    } catch (_) {
      return fallback;
    }
  }

  _settingInt(key, fallback) {
    try {
      return this._settings ? this._settings.get_int(key) : fallback;
    } catch (_) {
      return fallback;
    }
  }

  _staleAfterSeconds() {
    // Hide threshold for the cache file mtime; when the file is older than
    // this, Trndi is likely not running.
    return this._settingInt('hide-after-minutes', 11) * 60;
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
      const [line4] = dis.read_line_utf8(null);
      dis.close(null);

      if (!line1)
        return null;

      // Compact ranges for panel fit: "70 - 180" -> "70-180".
      const trimmed = line1.trim().replace(/\s*-\s*/g, '-');
      const value = trimmed.length > 0 ? trimmed : null;
      if (!value)
        return null;

      // Hide when the cache file itself is old (e.g. Trndi not running).
      // Important: this is NOT the same as reading freshness — that threshold
      // comes from the cache file (line 3); this one is the extension's own
      // hide-after-minutes setting.
      const hideAfterSeconds = this._staleAfterSeconds();
      if (mtimeAge > 0 && mtimeAge > hideAfterSeconds) {
        log(`[TrndiCurrent] File too old: mtime=${mtime}, age=${mtimeAge}s, threshold=${hideAfterSeconds}s`);
        return null;
      }

      // Newer Trndi writes:
      // line1: value
      // line2: reading epoch seconds
      // line3: freshness threshold minutes
      // line4: trend arrow, empty when the user turned the badge trend off
      const arrow = line4 ? String(line4).trim() : '';
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

      // Age of the reading in whole minutes, from the reading epoch when it
      // is usable and the file mtime otherwise (same fallback as the KDE
      // plasmoid); null when neither is known.
      let ageMin = null;
      const ageBasis = (!Number.isNaN(epoch) && epoch > 0) ? epoch : mtime;
      if (ageBasis > 0)
        ageMin = Math.max(0, Math.floor((now - ageBasis) / 60));

      log(`[TrndiCurrent] Read: value=${value}, arrow=${arrow}, isStale=${isStale}, epoch=${epoch}, freshMin=${freshMin}, mtimeAge=${mtimeAge}`);
      return { value, arrow, isStale, epoch, freshMin, ageMin };
    } catch (e) {
      log(`[TrndiCurrent] Error reading file: ${e}`);
      return null;
    }
  }

  _tick() {
    const state = this._readCurrentState();
    if (!state) {
      log('[TrndiCurrent] No state, hiding');
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
    let text;
    if (state.isStale)
      text = '--';
    else if (state.arrow && this._settingBool('show-trend-arrow', true))
      text = `${state.value} ${state.arrow}`;
    else
      text = state.value;

    // The plasmoid shows "X min ago" as its own row; a panel label is a
    // single line, so append a compact form instead.
    if (this._settingBool('show-age', false) && state.ageMin !== null)
      text += ` · ${state.ageMin}m`;

    this._label.set_text(text);

    // Low-noise debug: log only when value/stale changes.
    try {
      const dbg = `${state.value}|${state.arrow ?? ''}|${state.isStale ? 'stale' : 'fresh'}|${state.epoch ?? ''}|${state.freshMin ?? ''}`;
      if (dbg !== this._lastDebugKey) {
        this._lastDebugKey = dbg;
        log(`[TrndiCurrent] ${dbg}`);
      }
    } catch (_) {
    }
    return GLib.SOURCE_CONTINUE;
  }

  _armTimer() {
    if (this._timeoutId) {
      GLib.source_remove(this._timeoutId);
      this._timeoutId = null;
    }
    const interval = Math.max(2, this._settingInt('update-interval-seconds', 5));
    this._timeoutId = GLib.timeout_add_seconds(GLib.PRIORITY_DEFAULT, interval, this._tick.bind(this));
  }

  enable() {
    if (this._button)
      return;

    try {
      log(`[TrndiCurrent] enable (${this.metadata?.uuid ?? 'unknown'})`);
    } catch (_) {
    }

    // getSettings() throws when the schema is missing (a hand-copied install
    // without glib-compile-schemas); run with the defaults in that case.
    try {
      this._settings = this.getSettings();
      for (const key of ['show-age', 'show-trend-arrow', 'hide-after-minutes']) {
        this._settingsChangedIds.push(
          this._settings.connect(`changed::${key}`, () => this._tick()));
      }
      this._settingsChangedIds.push(
        this._settings.connect('changed::update-interval-seconds', () => {
          this._armTimer();
          this._tick();
        }));
    } catch (e) {
      log(`[TrndiCurrent] Settings unavailable, using defaults: ${e}`);
      this._settings = null;
    }

    // Poll the cache file; Trndi writes it when readings update.
    this._armTimer();
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

    if (this._settings) {
      for (const id of this._settingsChangedIds)
        this._settings.disconnect(id);
      this._settings = null;
    }
    this._settingsChangedIds = [];

    if (this._button) {
      this._button.destroy();
      this._button = null;
      this._label = null;
    }
  }
}
