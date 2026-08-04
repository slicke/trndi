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

    Kirigami.FormLayout {
        QQC2.CheckBox {
            id: showAgeRow
            text: "Show “X ago” row"
        }
    }
}
