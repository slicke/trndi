#!/usr/bin/env python3
"""Apply translation dictionaries to PO files."""
import re, sys, shutil
sys.stdout.reconfigure(encoding="utf-8")

def apply_to_po(filename, translations, html_map=None):
    with open(filename, "r", encoding="utf-8") as f:
        content = f.read()

    # Normalize line endings for processing
    content = content.replace("\r\n", "\n")
    blocks = re.split(r"\n\n+", content)
    updated = 0

    for i, block in enumerate(blocks):
        ctx_m = re.search(r'msgctxt\s+"(.*)"', block)
        id_m  = re.search(r'msgid\s+"(.*)"', block)
        str_m = re.search(r'msgstr\s+"(.*)"', block)

        if not (id_m and str_m):
            continue

        ctx    = ctx_m.group(1) if ctx_m else ""
        msgid  = id_m.group(1)
        msgstr = str_m.group(1)

        if not msgid or msgstr:
            continue  # skip header and already-translated entries

        key = (ctx, msgid)

        # Check HTML map first (keyed by a short name, matched by looking at msgid content)
        translation = None
        if html_map:
            if "NightScout v2 Setup" in msgid or "NightScout v2-" in msgid or \
               ("NightScout" in msgid and "v2" in msgid and "ol" in msgid):
                translation = html_map.get("nightscout_v2")
            elif "NightScout v3 Setup" in msgid or "NightScout v3-" in msgid or \
                 ("NightScout" in msgid and "v3" in msgid and "ALPHA" in msgid):
                translation = html_map.get("nightscout_v3")
            elif "xDrip setup" in msgid or "xDrip-" in msgid:
                translation = html_map.get("xdrip")

        if translation is None:
            translation = translations.get(key)
        # Fallback: try with escaped quotes normalized to plain quotes
        if translation is None:
            key2 = (ctx, msgid.replace('\\"', '"'))
            translation = translations.get(key2)

        if translation is not None:
            # Escape any literal quotes in translation that aren't already escaped
            # The translation value should already be in PO-escaped form
            new_str_line = f'msgstr "{translation}"'
            block = re.sub(r'msgstr\s+".*"', new_str_line, block)
            blocks[i] = block
            updated += 1

    result = "\n\n".join(blocks)
    # Restore Windows line endings
    result = result.replace("\n", "\r\n")

    with open(filename, "w", encoding="utf-8", newline="") as f:
        f.write(result)

    return updated


def main():
    import os

    # --- JM ---
    from translations_jm import translations_jm
    n = apply_to_po("Trndi.jm.po", translations_jm)
    print(f"JM: applied {n} translations")

    # --- DA ---
    try:
        from translations_da import translations_da, HTML_DA
        n = apply_to_po("Trndi.da.po", translations_da, HTML_DA)
        print(f"DA: applied {n} translations")
    except ImportError:
        print("DA: skipped (no translations_da.py)")

    # --- DE ---
    try:
        from translations_de import translations_de, HTML_DE
        n = apply_to_po("Trndi.de.po", translations_de, HTML_DE)
        print(f"DE: applied {n} translations")
    except ImportError:
        print("DE: skipped (no translations_de.py)")

    # --- NB ---
    try:
        from translations_nb import translations_nb, HTML_NB
        n = apply_to_po("Trndi.nb.po", translations_nb, HTML_NB)
        print(f"NB: applied {n} translations")
    except ImportError:
        print("NB: skipped (no translations_nb.py)")


if __name__ == "__main__":
    main()
