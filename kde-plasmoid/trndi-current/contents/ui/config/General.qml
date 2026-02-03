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
