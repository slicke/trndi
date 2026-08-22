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
import QtQuick
import QtQuick.Controls as QQC2
import org.kde.kcmutils as KCM
import org.kde.kirigami as Kirigami

KCM.SimpleKCM {
    property alias cfg_ShowAgeRow: showAgeRow.checked
    property alias cfg_ShowTrendArrow: showTrendArrow.checked
    property alias cfg_UpdateIntervalSeconds: updateInterval.value
    property alias cfg_HideAfterMinutes: hideAfter.value

    Kirigami.FormLayout {
        QQC2.CheckBox {
            id: showAgeRow
            text: i18n("Show “X ago” row")
        }

        QQC2.CheckBox {
            id: showTrendArrow
            // Trndi only publishes an arrow while its own "trend arrow on the
            // badge" setting is on; this hides it for this widget alone.
            text: i18n("Show trend arrow")
        }

        QQC2.SpinBox {
            id: updateInterval
            Kirigami.FormData.label: i18n("Check for new readings every:")
            from: 2
            to: 120
            textFromValue: function (value) {
                return i18np("%1 second", "%1 seconds", value)
            }
            valueFromText: function (text) {
                return parseInt(text, 10) || 5
            }
        }

        QQC2.SpinBox {
            id: hideAfter
            // Matches the reading-hiding done in main.qml's readCmd(): when the
            // cache file itself is this old, Trndi is likely not running.
            Kirigami.FormData.label: i18n("Hide when Trndi stops updating for:")
            from: 1
            to: 120
            textFromValue: function (value) {
                return i18np("%1 minute", "%1 minutes", value)
            }
            valueFromText: function (text) {
                return parseInt(text, 10) || 11
            }
        }
    }
}
