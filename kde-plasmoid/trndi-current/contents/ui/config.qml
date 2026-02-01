import QtQuick 2.15
import org.kde.kcmutils as KCM

KCM.ConfigModel {
    KCM.ConfigCategory {
        name: "General"
        icon: "preferences-system"
        source: "ui/config/General.qml"
    }
}
