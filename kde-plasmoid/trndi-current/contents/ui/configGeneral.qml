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
