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
