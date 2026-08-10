# Trndi Translations

Trndi can be translated to any language.
All translation files are stored in `lang/`.

## File Naming

Use this naming format:

- `Trndi.<language-code>.po`

Examples:

- `Trndi.de.po`
- `Trndi.sv.po`
- `Trndi.nb.po`

Use an [ISO 639-1 language code](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes) when possible.

## Add Or Update A Translation

You can use either:

- [Poedit](https://poedit.net/download) (desktop)
- [POEditor](https://poeditor.com) (online)

1. Open or upload `lang/Trndi.pot`.
2. Create a new translation, or open an existing `lang/Trndi.<language-code>.po` and update it.
3. Save/export as `Trndi.<language-code>.po`.
4. Submit a pull request with the `.po` file.

Submit the `.po` — that is the source translation file. The `.mo` files in `lang/` are
committed binaries that nothing in the build regenerates (`make` only copies `lang/` to
the output directory), and Trndi loads `.po` in preference to `.mo` anyway, so a new or
updated `.po` takes effect on its own.

---

# For developers: adding new strings

Every user-facing string has to reach `lang/Trndi.pot` before it can be translated.
Two kinds of string end up there, and they are refreshed by different tools:

| Source | Generated into | Refreshed by |
| --- | --- | --- |
| `resourcestring` in code | `lib/<target>/<unit>.rsj` | every `make` / `lazbuild` |
| A property set in an `.lfm` | `units/forms/<form>.lrj` | saving the form in the Lazarus IDE |

## Component captions: design the form in the IDE

Set captions where they belong — on the component, in the form designer. Saving the
form writes both the `.lfm` and the `.lrj`, so the caption reaches the catalogs with
no extra work. This is how nearly all of Trndi's UI text is declared, and there is
nothing wrong with it.

Two cases escape that, and only those two:

- **`TStrings` properties are never extracted at all.** The `Items` of a
  `TRadioGroup`, `TComboBox` or `TListBox` produce no `.po` entry whether you use the
  designer or not. They have to be filled from code to be translatable.
- **An `.lfm` edited outside the IDE leaves a stale `.lrj`.** `lazbuild` never
  regenerates it — not even when the file is missing — so a caption added by hand (or
  by a script or coding agent) stays untranslated until someone opens the form in the
  designer and saves it. Doing that is the fix; it costs one save.

`TfConf.ApplyCaptionsFromResources` in `units/forms/uconf.pp` is the escape hatch for
both: it fills `rbTrendWindow.Items` and reassigns three captions that were added
without the designer. It holds 3 of the settings dialog's 261 strings — treat it as a
patch for those cases, not as the pattern to follow for new controls.

Text that a form never displays statically — dialog messages, errors, anything built
with `Format` — is a `resourcestring` for the ordinary reason that it lives in code.

## Refreshing the catalogs

`lazbuild` does **not** update `lang/Trndi.pot` or the `.po` files — only a build from
inside the IDE does. To merge new strings from the command line, use the
`updatepofiles` tool that ships with Lazarus (`tools/updatepofiles` in the Lazarus
directory).

> **Pass every `.rsj` *and* every `.lrj`, always.**
> `updatepofiles` rebuilds the `.pot` as exactly the union of the resource files you
> hand it, and prunes everything else from the `.pot` **and from every `.po`**.
> Feeding it only `lib/*.rsj` silently deletes all 216 `tfconf.*` form captions and
> their translations, in all six languages. Given the complete set it is purely
> additive — verified, zero lines removed.

```powershell
# Windows, after a build has produced the .rsj files
$res = (Get-ChildItem lib\x86_64-win64\*.rsj).FullName +
       (Get-ChildItem units\forms\*.lrj).FullName
& C:\lazarus\tools\updatepofiles.exe @res "$PWD\lang\Trndi.pot"
```

Then check `git diff lang/` before committing, every time. Two more things to know:

- Including the `.lrj` files — which you must — also sweeps in every design-time
  caption: the ~37 `?` help buttons and placeholders such as `lArrow` or `"75"`,
  several hundred lines of untranslatable noise. Setting `Localized=False` on those
  captions in the IDE is the real fix.
- It writes CRLF line endings.

Because of all that, for one or two new strings it is usually less work — and safer —
to add the entries by hand. Keep them sorted alphabetically by
`unit.lowercaseidentifier`, and add a `#, object-pascal-format` line for any string
containing `%s` / `%d`.

## Check your work

```bash
make lang-check          # Linux/BSD/Haiku (gmake on macOS)
.\make.ps1 lang-check    # Windows
```

It lists every resource string in the current checkout that never reached the `.pot`,
then reports translated/fuzzy/untranslated counts per catalog. It is read-only — it
never writes to `lang/`.

Design-time placeholders (the `?` help buttons, captions still equal to their component
name, numeric mock-ups) are counted but not listed; pass `LANG_ALL=1` / `-all` to see
them. The `.po` half needs `msgfmt` from gettext, which Windows does not ship — without
it the target still does the `.pot` audit and says it skipped the rest.

To validate a single catalog by hand:

```bash
msgfmt --check-format --check-header -o /dev/null lang/Trndi.sv.po
```

Jämtlandic (`Trndi.jm.po`) is a Swedish dialect, not Jamaican — see the existing
entries for its spelling conventions.

## Why there is no `make` target that generates the catalogs

`lang-check` only reads. Nothing in the build writes `lang/`, and that is deliberate:
`updatepofiles` prunes to the union of the resource files it is handed, and the set a
checkout has depends on which platform and which build mode last compiled. A Linux
build produces no `trndi.native.win.rsj`; a stale one here also lacked
`trndi.api.librelinkup.rsj`, whose 19 strings a `make po` would therefore have deleted
from all six languages. Keeping generation manual keeps that decision visible.


