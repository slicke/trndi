import QtQuick 2.15
import org.kde.plasma.components 3.0 as PlasmaComponents3

Item {
    id: root

    property alias text: label.text
    property alias color: label.color

    // Allow callers to strike through the label (used for stale readings).
    property bool strikeout: false

    property int minPointSize: 7
    property int maxPointSize: 48

    property int horizontalAlignment: Text.AlignHCenter
    property int verticalAlignment: Text.AlignVCenter
    property int elide: Text.ElideNone

    function recompute() {
        // Avoid thrashing during layout; keep a sane default.
        if (!label.text || root.width <= 2 || root.height <= 2) {
            label.font.pointSize = Math.max(root.minPointSize, 10)
            return
        }

        // Binary search best point size.
        var lo = root.minPointSize
        var hi = root.maxPointSize
        var best = lo

        metrics.text = label.text
        metrics.font = label.font

        // Padding guard (Labels often get tight in panel)
        var availW = Math.max(1, root.width - 2)
        var availH = Math.max(1, root.height - 2)

        while (lo <= hi) {
            var mid = Math.floor((lo + hi) / 2)
            metrics.font.pointSize = mid

            // TextMetrics reports single-line height/width for current font.
            if (metrics.width <= availW && metrics.height <= availH) {
                best = mid
                lo = mid + 1
            } else {
                hi = mid - 1
            }
        }

        label.font.pointSize = best
    }

    PlasmaComponents3.Label {
        id: label
        anchors.fill: parent
        horizontalAlignment: root.horizontalAlignment
        verticalAlignment: root.verticalAlignment
        elide: root.elide
        wrapMode: Text.NoWrap

        font.strikeout: root.strikeout

        onTextChanged: root.recompute()
    }

    TextMetrics {
        id: metrics
        text: ""
        font: label.font
    }

    onWidthChanged: recompute()
    onHeightChanged: recompute()
    Component.onCompleted: recompute()
}
