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
import org.kde.plasma.core 2.0 as PlasmaCore

Item {
    // This file is loaded by main.qml as a fallback for Plasma 5.

    Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: exec.run(readCmd())
    }

    Component.onCompleted: exec.run(readCmd())

    PlasmaCore.DataSource {
        id: exec
        engine: "executable"
        connectedSources: []

        onNewData: function (sourceName, data) {
            var out = "";
            if (data && data["stdout"] !== undefined) {
                out = String(data["stdout"]);
            }
            out = out.trim();
            parent.readingText = out;
            disconnectSource(sourceName);
        }

        function run(cmd) {
            connectSource(cmd);
        }
    }

    function readCmd() {
        return "bash -lc '" +
               "f=\"${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt\"; " +
               "if [ -f \"$f\" ]; then " +
               "now=$(date +%s); m=$(stat -c %Y \"$f\" 2>/dev/null || echo 0); " +
               "if [ $((now-m)) -le 600 ]; then head -n1 \"$f\"; fi; " +
               "fi'";
    }
}
