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
import QtQuick 2.15
import QtQuick.Layouts 1.15
import org.kde.plasma.components 3.0 as PlasmaComponents3
import org.kde.kcmutils as KCM

KCM.SimpleKCM {
    property alias cfg_ShowAgeRow: showAgeRow.checked

    ColumnLayout {
        spacing: 6

        PlasmaComponents3.CheckBox {
            id: showAgeRow
            text: "Show “X ago” row"
        }
    }
}
