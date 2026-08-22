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
//
// Preferences for the "Trndi Current" indicator, mirroring the KDE plasmoid's
// config page (kde-plasmoid/trndi-current/contents/ui/configGeneral.qml).
// Adw.SwitchRow/SpinRow need libadwaita 1.4, which every supported Shell
// version (45+) ships.
import Adw from 'gi://Adw';
import Gio from 'gi://Gio';
import Gtk from 'gi://Gtk';

import {ExtensionPreferences} from 'resource:///org/gnome/Shell/Extensions/js/extensions/prefs.js';

export default class TrndiCurrentPrefs extends ExtensionPreferences {
  fillPreferencesWindow(window) {
    const settings = this.getSettings();

    const page = new Adw.PreferencesPage();
    const group = new Adw.PreferencesGroup({title: 'Indicator'});

    const arrowRow = new Adw.SwitchRow({
      title: 'Show trend arrow',
      subtitle: 'Only shown while Trndi’s own “trend arrow on the badge” setting is on',
    });
    settings.bind('show-trend-arrow', arrowRow, 'active', Gio.SettingsBindFlags.DEFAULT);
    group.add(arrowRow);

    const ageRow = new Adw.SwitchRow({
      title: 'Show reading age',
      subtitle: 'Append how many minutes ago the reading arrived',
    });
    settings.bind('show-age', ageRow, 'active', Gio.SettingsBindFlags.DEFAULT);
    group.add(ageRow);

    const intervalRow = new Adw.SpinRow({
      title: 'Check for new readings every',
      subtitle: 'Seconds between reads of Trndi’s cache file',
      adjustment: new Gtk.Adjustment({lower: 2, upper: 120, step_increment: 1}),
    });
    settings.bind('update-interval-seconds', intervalRow, 'value', Gio.SettingsBindFlags.DEFAULT);
    group.add(intervalRow);

    const hideRow = new Adw.SpinRow({
      title: 'Hide when Trndi stops updating for',
      subtitle: 'Minutes without cache updates before the indicator hides',
      adjustment: new Gtk.Adjustment({lower: 1, upper: 120, step_increment: 1}),
    });
    settings.bind('hide-after-minutes', hideRow, 'value', Gio.SettingsBindFlags.DEFAULT);
    group.add(hideRow);

    page.add(group);
    window.add(page);
  }
}
