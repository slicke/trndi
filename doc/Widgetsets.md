Trndi compiles natively to each platform, with a native widgetset.

# Windows
Trndi compiles as a native Windows application, with native Windows libraries. It also uses Windows-native system calls and features - such as coloring the title bar etc. Any Windows calls can be performed as long as they are gated from Linux and macOS.

# Linux
Trndi preffers Qt6 and is designed to work with it. As such it runs natively on Linux, with a Qt6 configuration. It's also using gated platform-native code such as font handling and window manager compability features etc.

# macOS
Trndi runs natively on macOS with it's native widgetset (cocoa). It uses gated native code for the Dock and title bar. At the time of writing, testing is limited on macOS as I lack a Mac to test on at this time.