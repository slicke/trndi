# Pixie (vendored)

HTML/CSS rendering engine for the slicke.ux.alert dialogs, replacing
TurboPower iPro (TIpHtmlPanel).

- Upstream: https://gitlab.com/retrofoxed/pixie
- Vendored commit: 29c05c26d54cca5f4bff7660452b92e1d344d081
- License: MIT (see LICENSE)

Only the `common/` and `htmlview/` source trees are vendored — the
markdown, svgview, tagbar and paintbox components are not used. The
units are compiled straight into Trndi via the project's unit search
path; no Lazarus package installation is required.

To update: clone upstream, replace `common/` and `htmlview/` with
`source/common` and `source/htmlview`, and record the new commit here.
