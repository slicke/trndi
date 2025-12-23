import QtCore
import QtQuick 2.15
import org.kde.plasma.plasmoid 2.0
import org.kde.plasma.components 3.0 as PlasmaComponents3
import org.kde.plasma.plasma5support 2.0 as Plasma5Support

PlasmoidItem {
    id: root

    property string readingText: ""
    property string lastErrorText: ""
    property string lastGoodText: ""
    property double lastGoodEpochMs: 0
    // If reads temporarily fail (file:// hiccup), keep last known value briefly.
    property int keepLastGoodForMs: 30000

    preferredRepresentation: compactRepresentation

    compactRepresentation: FittedLabel {
        text: root.readingText.length > 0 ? root.readingText
                                          : (root.lastErrorText.length > 0 ? "!" : "")
        visible: text.length > 0
        elide: Text.ElideRight
        minPointSize: 7
        maxPointSize: 48
    }

    fullRepresentation: FittedLabel {
        text: root.readingText.length > 0 ? root.readingText : root.lastErrorText
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
        elide: Text.ElideRight
        minPointSize: 10
        maxPointSize: 72
    }

    toolTipMainText: "Trndi"
    toolTipSubText: root.readingText.length > 0 ? root.readingText
                                                : (root.lastErrorText.length > 0 ? root.lastErrorText : "No current value")

    Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: exec.run(root.readCmd())
    }

    Component.onCompleted: exec.run(root.readCmd())


    Plasma5Support.DataSource {
        id: exec
        engine: "executable"
        connectedSources: []

        onNewData: function (sourceName, data) {
            var now = Date.now();
            var out = "";
            var err = "";

            if (data) {
                if (data["stdout"] !== undefined) out = String(data["stdout"]);
                if (data["stderr"] !== undefined) err = String(data["stderr"]);
            }

            out = out.trim();
            err = err.trim();

            if (out.length > 0) {
                root.readingText = out;
                root.lastGoodText = out;
                root.lastGoodEpochMs = now;
                root.lastErrorText = "";
            } else if (err.length > 0) {
                root.lastErrorText = err;
                if (root.lastGoodText.length > 0 && (now - root.lastGoodEpochMs) <= root.keepLastGoodForMs)
                    root.readingText = root.lastGoodText;
                else
                    root.readingText = "";
            } else {
                // No output means "no value" (file missing/stale) => clear.
                root.readingText = "";
                root.lastGoodText = "";
                root.lastGoodEpochMs = 0;
                root.lastErrorText = "";
            }

            disconnectSource(sourceName);
        }

        function run(cmd) {
            connectSource(cmd);
        }
    }

    function readCmd() {
        // Read the same cache file as GNOME extension.
        // Hide value if missing or stale (>120s).
        return "bash -lc '" +
               "f=\"${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt\"; " +
               "if [ -f \"$f\" ]; then " +
               "now=$(date +%s); m=$(stat -c %Y \"$f\" 2>/dev/null || echo 0); " +
               "if [ $((now-m)) -le 120 ]; then head -n1 \"$f\"; fi; " +
               "fi'";
    }
}
