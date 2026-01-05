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
    property bool readingStale: false
    // If reads temporarily fail (file:// hiccup), keep last known value briefly.
    property int keepLastGoodForMs: 30000

    preferredRepresentation: compactRepresentation

    compactRepresentation: FittedLabel {
        text: root.readingText.length > 0 ? root.readingText
                                          : (root.lastErrorText.length > 0 ? "!" : "")
        visible: text.length > 0
        elide: Text.ElideNone
        minPointSize: 5
        maxPointSize: 48
        strikeout: root.readingStale
    }

    fullRepresentation: FittedLabel {
        text: root.readingText.length > 0 ? root.readingText : root.lastErrorText
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
        elide: Text.ElideNone
        minPointSize: 8
        maxPointSize: 72
        strikeout: root.readingStale
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

            // Format: "<value>\t<epochSeconds>\t<freshMinutes>" (epoch/fresh may be empty).
            var value = out;
            var epoch = 0;
            var freshMin = 0;
            var parts = null;
            if (out.indexOf("\t") !== -1) {
                parts = out.split("\t");
            } else if (out.indexOf("\\t") !== -1) {
                // Fallback if the command produced literal "\t".
                parts = out.split("\\t");
            }

            if (parts !== null) {
                value = (parts.length > 0 ? String(parts[0]) : "");
                epoch = (parts.length > 1 ? parseInt(parts[1], 10) : 0);
                freshMin = (parts.length > 2 ? parseInt(parts[2], 10) : 0);
            }

            value = value.trim();
            if (isNaN(epoch)) epoch = 0;
            if (isNaN(freshMin)) freshMin = 0;

            // Accept seconds or milliseconds epoch; correct future timestamps
            // from older writers by subtracting a rounded hour offset.
            var nowS = Math.floor(now / 1000);
            if (epoch > 1000000000000)
                epoch = Math.floor(epoch / 1000);
            if (epoch > (nowS + 60)) {
                var delta = epoch - nowS;
                var hours = Math.round(delta / 3600);
                if (hours !== 0)
                    epoch = epoch - (hours * 3600);
            }

            root.readingStale = (epoch > 0 && freshMin > 0) ?
                ((nowS - epoch) > (freshMin * 60)) : false;

            // Compact range formatting to improve fit in panel (e.g. "70 - 180" -> "70-180").
            if (value.indexOf("-") !== -1) {
                value = value.replace(/\s*-\s*/g, "-");
            }

            if (value.length > 0) {
                root.readingText = value;
                root.lastGoodText = value;
                root.lastGoodEpochMs = now;
                root.lastErrorText = "";
            } else if (err.length > 0) {
                root.readingStale = false;
                root.lastErrorText = err;
                if (root.lastGoodText.length > 0 && (now - root.lastGoodEpochMs) <= root.keepLastGoodForMs)
                    root.readingText = root.lastGoodText;
                else
                    root.readingText = "";
            } else {
                // No output means "no value" (file missing) => clear.
                root.readingStale = false;
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
        // Trndi (newer versions) write:
        //   line1: value
        //   line2: reading epoch seconds
        //   line3: freshness threshold minutes
        // Output a single line with tab-separated fields for easy parsing.
        return "bash -lc '" +
               "f=\"${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt\"; " +
               "if [ -f \"$f\" ]; then " +
             "now=$(date +%s); mt=$(stat -c %Y \"$f\" 2>/dev/null || echo 0); " +
             "v=$(head -n1 \"$f\" 2>/dev/null); " +
             "t=$(sed -n 2p \"$f\" 2>/dev/null); " +
             "m=$(sed -n 3p \"$f\" 2>/dev/null); " +
             // Hide if cache file is old (Trndi likely not running). Fixed 11 minutes.
             "age=$((now-mt)); thr=$((11*60)); " +
             "if [ $mt -gt 0 ] && [ $age -gt $thr ]; then exit 0; fi; " +
             "printf \"%s\\t%s\\t%s\\n\" \"$v\" \"$t\" \"$m\"; " +
               "fi'";
    }
}
