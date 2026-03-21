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

The `.mo` files are generated during build, so contributors should normally submit the `.po` source translation file.


