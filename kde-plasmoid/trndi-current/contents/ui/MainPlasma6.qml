import QtQuick 2.15
import org.kde.plasma.plasma5support 2.0 as Plasma5Support

Item {
    // This file is loaded by main.qml. It may fail to load on Plasma 5.

    Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: exec.run(readCmd())
    }

    Component.onCompleted: exec.run(readCmd())

    Plasma5Support.DataSource {
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
        // Read the same cache file as the GNOME extension.
        // Hide value if missing or stale (>120s).
        return "bash -lc '" +
               "f=\"${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt\"; " +
               "if [ -f \"$f\" ]; then " +
               "now=$(date +%s); m=$(stat -c %Y \"$f\" 2>/dev/null || echo 0); " +
               "if [ $((now-m)) -le 120 ]; then head -n1 \"$f\"; fi; " +
               "fi'";
    }
}
