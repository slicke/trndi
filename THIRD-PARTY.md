# Third-party components

Trndi as a whole is distributed under the GNU General Public License, version 3
([LICENSE.md](LICENSE.md)). It incorporates the components below, each under its
own license. Every license listed here is compatible with GPLv3, and the
combined work is conveyed under GPLv3.

| Component | Where | License | Notes |
|---|---|---|---|
| quickjs-ng | `externals/quickjs/prebuilt/**` (binaries), bound by `units/trndi/ext/trndi.ext.quickjs.pp` | MIT — [`externals/quickjs/LICENSE.quickjs-ng`](externals/quickjs/LICENSE.quickjs-ng) | Unmodified upstream v0.15.1. Only present in `Extensions` build modes. Rebuild with `externals/quickjs/build.sh`. |
| libPasCURL | `units/misc/libpascurl.pas` | GPL-3.0-only | © 2019-2020 Ivan Semenkov. This is why Trndi is GPLv3-**only** rather than "or later". |
| CMC DirectX 12 headers | `units/misc/winutils/directx/DX12.*.pas` | GPL-2.0-or-later | © 2015 CMC Development Team. Pascal translations of Microsoft Windows SDK headers (© Microsoft Corporation). Used into GPLv3 under the "or later" option. |
| nsutils Cocoa helpers | `units/misc/nsutils/nsutils.cocoahelpers.pp` and siblings | Modified LGPL (FPC RTL variant) | © 2012 Phil Hess, extended by Björn Lindh. |
| Lazarus LCL | linked at build time | Modified LGPL | Not vendored; supplied by the Lazarus installation. |
| slicke toolkit | `units/slicke/**`, `tests/mock/slicke.*` | Apache-2.0 | © Björn Lindh. Deliberately permissive so it can be reused outside Trndi; Apache-2.0 is one-way compatible into GPLv3. |

## Notices in shipped builds

- The quickjs-ng MIT notice is reproduced in full in Trndi's own License dialog
  (Settings → License) in `Extensions` builds, and as a file alongside the
  prebuilt binaries in the source tree.
- `LICENSE.md`, `DISCLAIMER.md`, and this file are installed with every binary
  package (Windows installer, macOS app bundle, AppImage, deb/rpm).
