# Widgetsets (beginner-friendly)

Trndi can be built for Windows, Linux, and macOS.

When you build it, you must pick a **widgetset**. Think of a widgetset as the “UI toolkit” Trndi uses on your computer (what it uses to draw windows, buttons, fonts, etc.).

If you’re not sure which one to pick, use the recommendations below.

More detailed build notes:
- `guides/BUILDING.md`
- `README.md` (Linux Qt6 notes)

# Windows
Use the **native Windows** widgetset.

- Recommended: the Windows default/native widgetset.
- Result: a normal Windows app.

# Linux
Use **Qt6**.

- Recommended: **Qt6** (this is what Trndi is mainly developed and tested with on Linux).
- If Trndi won’t start after you build it, you may be missing Qt6-related packages (see `guides/BUILDING.md` and the Linux section in `README.md`).
- Other Linux widgetsets (like GTK) can sometimes work, but some features may be limited.

# macOS
You can use **Cocoa** (native macOS) or **Qt6**.

- Cocoa is the “native macOS look” option.
- There has been a known Lazarus/Cocoa drawing issue in the past for a specific custom-drawn control in Trndi, and the code includes a workaround on macOS.
- If you still see drawing/glitch issues on macOS, try building with **Qt6** instead.
- Note: macOS builds are less tested in this repo than Windows/Linux, so double-check the result on your macOS version.

## Command-line (optional)
If you build from the terminal, the widgetset is chosen with `--widgetset=...`.

- Linux/macOS Qt6: `--widgetset=qt6`