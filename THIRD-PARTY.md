# Third-party components

Trndi as a whole is distributed under the GNU General Public License, version 3
([LICENSE.md](LICENSE.md)). It incorporates the components below, each under its
own license. Every license listed here is compatible with GPLv3, and the
combined work is conveyed under GPLv3.

| Component | Where | License | Notes |
|---|---|---|---|
| quickjs-ng | `externals/quickjs/prebuilt/**` (binaries), bound by `units/trndi/ext/trndi.ext.quickjs.pp` | MIT — [`externals/quickjs/LICENSE.quickjs-ng`](externals/quickjs/LICENSE.quickjs-ng) | Unmodified upstream v0.15.1. Only present in `Extensions` build modes. Rebuild with `externals/quickjs/build.sh`. |
| Pixie | `externals/pixie/common/**`, `externals/pixie/htmlview/**`, bound by `units/slicke/slicke.ux.alert.pp` | MIT — [`externals/pixie/LICENSE`](externals/pixie/LICENSE) | © 2026 SoftPerfect Pty Ltd. Vendored from upstream commit `29c05c26` (see `externals/pixie/README.md`); only `common/` and `htmlview/` are included. Present in every build mode — renders the HTML message/log dialogs. |
| CMC DirectX 12 headers | `units/misc/winutils/directx/DX12.*.pas` | GPL-2.0-or-later | © 2015 CMC Development Team. Pascal translations of Microsoft Windows SDK headers (© Microsoft Corporation). Used into GPLv3 under the "or later" option. |
| nsutils Cocoa helpers | `units/misc/nsutils/nsutils.cocoahelpers.pp` and siblings | Modified LGPL (FPC RTL variant) | © 2012 Phil Hess, extended by Björn Lindh. |
| Lazarus LCL | linked at build time | Modified LGPL | Not vendored; supplied by the Lazarus installation. |
| slicke toolkit | `units/slicke/**`, `tests/mock/slicke.*` | Apache-2.0 | © Björn Lindh. Deliberately permissive so it can be reused outside Trndi; Apache-2.0 is one-way compatible into GPLv3. |

## Notices in shipped builds

Bundled MIT components carry their notices **inside the program**, not as files
beside it. "Included in all copies" is satisfied by text compiled into the
binary, and one in-app copy cannot drift out of sync across six packaging
layouts the way six copied files can:

- Every component above is credited by name under **Settings → Libraries**.
- The Pixie and quickjs-ng MIT texts are reproduced in full under
  **Settings → License**, below the "Additional component licenses" rule. The
  quickjs-ng half is compiled in only for `Extensions` builds, which are the
  only ones that ship it; the Pixie half is unconditional.

Trndi's own GPLv3 is the exception: section 4 wants the full license text, which
is far too long to embed, so `LICENSE.md` still ships as a file — together with
`DISCLAIMER.md` and this document — in every package layout: Windows installer,
macOS app bundle (`Trndi.app/Contents/Resources/`), AppImage, deb/rpm, and the
portable ZIPs.
