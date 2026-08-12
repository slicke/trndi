<#
make.ps1 — Windows helper to run `lazbuild` and provide common shortcuts

Usage:
  ./make.ps1 [release|debug|noext|noext-debug|ide-libs|list-modules|test|assets|ptop|clean[-n|--dry-run]|help] or ./make.ps1 [lazbuild-args...]

Behavior:
 - Sets `LAZBUILD` to `C:\lazarus\lazbuild.exe` if present and `LAZBUILD` is not already set
 - Ensures `OS=Windows_NT` environment variable is set for compatibility with the Makefile
 - Provides shortcuts (release, debug, noext, noext-debug, list-modules) that invoke `lazbuild` or enumerate units
 - Build targets stage a runnable layout in `build/` (override with the `OUTDIR` environment variable), like the Makefile's OUTDIR
 - `list-modules` uses native PowerShell (no Perl dependency on Windows)
 - Unknown arguments are forwarded directly to `lazbuild`
#>

param(
    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$MakeArgs
)

# This script focuses on calling lazbuild directly; it no longer searches for or invokes make.exe.
# Unknown arguments are forwarded to lazbuild below.

# If LAZBUILD not set, prefer standard Lazarus install location
if (-not $env:LAZBUILD) {
    $stdLaz = "C:\lazarus\lazbuild.exe"
    if (Test-Path $stdLaz) { $env:LAZBUILD = $stdLaz }
}

# Ensure OS is set so the Makefile can detect Windows
$env:OS = "Windows_NT"

# Lazbuild shortcuts: run lazbuild directly for common targets on Windows (convenience)
$firstArg = if ($MakeArgs.Length -ge 1) { $MakeArgs[0].ToLower() } else { "" }
$extraArgs = if ($MakeArgs.Length -gt 1) { $MakeArgs[1..($MakeArgs.Length - 1)] } else { @() }

function Find-Lazbuild {
    if ($env:LAZBUILD -and (Test-Path $env:LAZBUILD)) { return $env:LAZBUILD }
    $cmd = Get-Command lazbuild -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Path }
    $std = "C:\lazarus\lazbuild.exe"
    if (Test-Path $std) { return $std }
    return $null
}
$laz = Find-Lazbuild

# 'ptop' runs a Perl script. Unlike 'list-modules' there is no PowerShell twin --
# duplicating the generator would defeat the point of a single formatting source
# of truth -- so find the perl that Git for Windows already ships.
function Find-Perl {
    $cmd = Get-Command perl -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Path }
    $git = Get-Command git -ErrorAction SilentlyContinue
    if ($git) {
        $cand = Join-Path (Split-Path -Parent (Split-Path -Parent $git.Source)) 'usr\bin\perl.exe'
        if (Test-Path $cand) { return $cand }
    }
    foreach ($p in @('C:\Program Files\Git\usr\bin\perl.exe', 'C:\Strawberry\perl\bin\perl.exe')) {
        if (Test-Path $p) { return $p }
    }
    return $null
}

# The extension engine links quickjs-ng and its ABI shim as shared libraries.
# Windows resolves them from the executable's own directory, so place them next
# to Trndi.exe after an extensions-enabled build. See externals/quickjs/README.md.
function Copy-QuickJSLibs {
    param([string]$Destination = $PSScriptRoot)
    $src = Join-Path $PSScriptRoot 'externals\quickjs\prebuilt\x86_64-win64'
    if (-not (Test-Path $src)) {
        Write-Warning "QuickJS libraries not found at $src - extensions will fail to start. Rebuild them with externals/quickjs/build.sh."
        return
    }
    foreach ($dll in (Get-ChildItem (Join-Path $src '*.dll'))) {
        Copy-Item $dll.FullName $Destination -Force
        Write-Host "  copied $($dll.Name) -> $Destination" -ForegroundColor DarkGray
    }
}

# lazbuild writes Trndi.exe into the project directory (the .lpi target filename
# is relative). Stage a runnable layout in build/ from there -- binary,
# translations, and for extensions builds the QuickJS libraries -- so a Windows
# build produces the same thing to package as the Makefile's OUTDIR does. The
# copies in the project root stay: the Lazarus IDE builds and runs Trndi there
# (see the 'ide-libs' target).
function Publish-Build {
    param([switch]$WithQuickJS)

    $outDir = if ($env:OUTDIR) { $env:OUTDIR } else { Join-Path $PSScriptRoot 'build' }
    $exe = Join-Path $PSScriptRoot 'Trndi.exe'
    if (-not (Test-Path $exe)) {
        Write-Warning "Trndi.exe not found in the project directory - nothing to stage in $outDir."
        return
    }

    if (-not (Test-Path $outDir)) { New-Item -ItemType Directory -Path $outDir | Out-Null }
    Copy-Item $exe $outDir -Force
    Write-Host "  copied Trndi.exe -> $outDir" -ForegroundColor DarkGray

    $lang = Join-Path $PSScriptRoot 'lang'
    if (Test-Path $lang) {
        $outLang = Join-Path $outDir 'lang'
        if (-not (Test-Path $outLang)) { New-Item -ItemType Directory -Path $outLang | Out-Null }
        Copy-Item (Join-Path $lang '*') $outLang -Recurse -Force
        Write-Host "  copied translations -> $outLang" -ForegroundColor DarkGray
    }

    if ($WithQuickJS) { Copy-QuickJSLibs $outDir }
}

switch ($firstArg) {
    "" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Copy-QuickJSLibs; Publish-Build -WithQuickJS }
        exit $LASTEXITCODE
    }
    "release" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Copy-QuickJSLibs; Publish-Build -WithQuickJS }
        exit $LASTEXITCODE
    }
    "debug" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Debug)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Copy-QuickJSLibs; Publish-Build -WithQuickJS }
        exit $LASTEXITCODE
    }
    "ide-libs" {
        # The Lazarus IDE builds and runs Trndi.exe in the project root, so the
        # QuickJS libraries have to sit there for an Extensions build started
        # with F9 to load the engine. The build targets above already do this on
        # Windows; the target exists so the same command works on every platform
        # (see the Makefile's ide-libs).
        Write-Host "Copying QuickJS libraries to the project root (for Lazarus IDE runs)" -ForegroundColor Cyan
        Copy-QuickJSLibs
        exit 0
    }
    "noext" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'No Ext (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        # No QuickJS staging: a No Ext build compiles without TrndiExt and never
        # loads the engine.
        if ($LASTEXITCODE -eq 0) { Publish-Build }
        exit $LASTEXITCODE
    }
    "noext-debug" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'No Ext (Debug)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Publish-Build }
        exit $LASTEXITCODE
    }
    "test" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }

        Write-Host "Building console tests (tests/TrndiTestConsole.lpi)" -ForegroundColor Cyan
        & $laz -B 'tests/TrndiTestConsole.lpi' @extraArgs
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

        # ext_js_tests links the QuickJS engine and its ABI shim, which Windows
        # resolves from the test binary's own directory.
        Copy-QuickJSLibs (Join-Path $PSScriptRoot 'tests')

        # The test runner starts an in-process Pascal test server via
        # tests/testserver/test_server_helper.pp. Set TRNDI_TEST_SERVER_URL to
        # reuse an already-running server, or TRNDI_NO_TESTSERVER=1 to skip
        # integration tests entirely.
        Write-Host "Running console tests (embedded Pascal test server)" -ForegroundColor Cyan
        & 'tests/TrndiTestConsole.exe' @extraArgs
        exit $LASTEXITCODE
    }
    "clean" {
        Write-Host "Cleaning common products..." -ForegroundColor Cyan

        # Accept optional dry-run flag: -n or --dry-run
        $opts = $extraArgs
        $dryRun = $false
        if ($opts -and $opts.Count -gt 0) {
            if ($opts -contains '-n' -or $opts -contains '--dry-run' -or $opts -contains '/n') { $dryRun = $true }
        }

        $root = (Get-Location).ProviderPath
        $maxDepth = 3

        # Find build artifacts (depth-limited) similar to the Makefile's `find -maxdepth 3 ...`
        $matches = Get-ChildItem -Path . -Recurse -Force -ErrorAction SilentlyContinue |
            Where-Object {
                try {
                    $rel = $_.FullName.Substring($root.Length + 1)
                } catch { return $false }
                $depth = ($rel -split '[\\/]').Length - 1
                if ($depth -gt $maxDepth) { return $false }

                if (-not $_.PSIsContainer) {
                    return ($_.Name -match '\.(o|ppu|compiled|a|so|dll|exe)$') -or ($_.Name -match '\.noext-.*') -or ($_.Name -match 'noext-.*\.(lpi|res|ico|png)$')
                }
                else {
                    return ($_.Name -match '\.app$')
                }
            }

        $matchCount = ($matches | Measure-Object).Count

        if ($matchCount -gt 0) {
            if ($dryRun) {
                Write-Host "DRY RUN: items that would be removed:" -ForegroundColor Yellow
                foreach ($m in $matches | Sort-Object FullName) { Write-Host "  $($m.FullName)" }
                Write-Host "Would remove $matchCount items." -ForegroundColor Yellow
            }
            else {
                foreach ($m in $matches) {
                    try { Remove-Item -LiteralPath $m.FullName -Force -Recurse -ErrorAction SilentlyContinue } catch { }
                }
                Write-Host "Removed $matchCount matching items." -ForegroundColor Green
            }
        }
        else {
            Write-Host "(no matching build artifacts found)" -ForegroundColor Yellow
        }

        Write-Host "(Note: Lazarus project files and sources are not removed; temporary noext project files are cleaned.)" -ForegroundColor Cyan
        exit 0
    }
    "list-modules" {
        Write-Host "Modules (units) found under units/ (grouped by dot-separated names):" -ForegroundColor Cyan

        # Find Pascal unit files under units/ and extract `unit <name>` declarations.
        # Use a robust enumeration (Get-ChildItem -Include can be flaky when -Path has no wildcard).
        $files = Get-ChildItem -Path 'units' -Recurse -File -ErrorAction SilentlyContinue | Where-Object { $_.Extension -match '^\.(pp|pas)$' }
        if (-not $files -or $files.Count -eq 0) { Write-Host "  (no modules found)"; exit 0 }

        $pairs = New-Object System.Collections.Generic.List[System.String]
        foreach ($f in $files) {
            try {
                # Read file lines and look for the first `unit <name>` declaration
                foreach ($line in (Get-Content -Path $f.FullName -ErrorAction SilentlyContinue)) {
                    if ($line -match '^[\s]*unit[\s]+([A-Za-z0-9_.]+)') {
                        $unit = $Matches[1]
                        # keep path relative to repo root when possible
                        $rel = $f.FullName
                        try { $cwd = (Get-Location).ProviderPath; if ($rel.StartsWith($cwd)) { $rel = $rel.Substring($cwd.Length+1) } } catch { }
                        $pairs.Add("$unit`t$rel")
                        break
                    }
                }
            } catch { }
        }

        if ($pairs.Count -eq 0) { Write-Host "  (no modules found)"; exit 0 }

        $uniq = $pairs | Sort-Object -Unique

        # Normalize entries so files physically under `units/forms/` are shown under a
        # top-level `forms*` node even when the `unit` declaration has no namespace.
        $prefixed = $uniq | ForEach-Object {
            $pair = $_ -split "`t", 2
            $unit = $pair[0]
            $path = if ($pair.Length -gt 1) { $pair[1] } else { '' }
            if ($path -and $path -match '^[Uu]nits[\\/](?:forms)(?:[\\/]|$)') {
                if ($unit -notmatch '^forms(\*|\.)') { $unit = "forms*.$unit" }
            }

            # Files named buildinfo.pp / buildinfo.pas are shown under `ci*`
            if ($path -and $path -match '[\\/](?:buildinfo)\.(?:pp|pas)$') {
                if ($unit -notmatch '^ci(\*|\.)') { $unit = "ci*.$unit" }
            }

            "$unit`t$path"
        }

        # Use the native PowerShell tree printer on Windows so we don't depend on
        # an external `perl` binary. The result matches the Linux `perl` printer's
        # hierarchical format (top-level nodes, indented `- child` entries).
        $root = @{}
        foreach ($entry in $prefixed) {
            $unit = $entry.Split("`t")[0]
            $parts = $unit -split '\.'
            $h = $root
            foreach ($part in $parts) {
                if (-not $h.ContainsKey($part)) { $h[$part] = @{} }
                $h = $h[$part]
            }
        }
        function Print-Node([hashtable]$h, [int]$level) {
            foreach ($k in ($h.Keys | Sort-Object)) {
                if ($level -eq 0) { Write-Host $k }
                else { Write-Host ("  " * $level) -NoNewline; Write-Host "- $k" }
                Print-Node $h[$k] ($level + 1)
            }
        }
        Print-Node $root 0
        exit 0
    }
    "assets" {
        # Regenerate compiled-in resource bundles (.lrs) from their source files.
        # Currently: the CareLink login helper embedded in trndi.api.carelink.
        $lazres = $null
        if ($laz) {
            $cand = Join-Path (Split-Path -Parent $laz) 'tools\lazres.exe'
            if (Test-Path $cand) { $lazres = $cand }
        }
        if (-not $lazres) {
            $cmd = Get-Command lazres -ErrorAction SilentlyContinue
            if ($cmd) { $lazres = $cmd.Path }
        }
        if (-not $lazres) { Write-Error "lazres not found (looked next to lazbuild and on PATH)."; exit 1 }

        $out = 'assets\carelink_assets.lrs'
        if (-not (Test-Path 'assets')) { New-Item -ItemType Directory -Path 'assets' | Out-Null }
        Write-Host "Regenerating $out via $lazres" -ForegroundColor Cyan
        & $lazres $out `
            'tools\carelink-login\carelink-login.mjs' `
            'tools\carelink-login\package.json' `
            'tools\carelink-login\package-lock.json'
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        # lazres.exe writes CRLF; the committed .lrs is pinned to LF
        # (.gitattributes), so normalize to keep the working tree clean and match
        # what Linux/macOS lazres and the CI staleness check produce.
        $text = [IO.File]::ReadAllText($out) -replace "`r`n", "`n"
        [IO.File]::WriteAllText($out, $text)
        Write-Host "Normalized $out to LF" -ForegroundColor Cyan
        exit 0
    }
    "ptop" {
        # Regenerate ptop.cfg from JCFSettings.xml, so the JEDI Code Formatter
        # profile stays the only place formatting is described. ptop maps just a
        # subset of it; the generated header lists what was and was not carried
        # over. Mirrors 'make ptop' on Linux/macOS/BSD.
        $perl = Find-Perl
        if (-not $perl) { Write-Error "perl not found (looked on PATH, next to git.exe, and in the usual install locations)."; exit 1 }

        $out = 'ptop.cfg'
        Write-Host "Regenerating $out from JCFSettings.xml via $perl" -ForegroundColor Cyan
        & $perl 'scripts\jcf-to-ptop.pl' -o $out
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

        # Indent width and line length are ptop command-line options, not config
        # file keys, so they have to travel with every invocation.
        $flags = (& $perl 'scripts\jcf-to-ptop.pl' --args).Trim()
        Write-Host "Wrote $out. Run ptop as: ptop $flags -c $out <in> <out>"
        Write-Host "VS Code (alefragnani.pascal-formatter): set pascal.formatter.engineParameters to this file," -ForegroundColor DarkGray
        Write-Host "and pascal.format.indent / pascal.format.wrapLineLength to match the flags above." -ForegroundColor DarkGray
        exit 0
    }
    "lang-check" {
        # Audit lang/: which resource strings never reached the .pot, and how
        # complete each catalog is. Read-only. Mirrors 'make lang-check'.
        #
        # The exit status reports the .po validation alone: a msgfmt failure
        # fails the target, a missing-from-.pot listing does not. That list is
        # advisory by design -- which .rsj/.lrj files a checkout has depends on
        # the platform and build mode that last compiled (a No Ext build emits
        # no TrndiExt strings, a Linux build no trndi.native.win ones), so it is
        # routinely non-empty for reasons no target can fix. Keep this in step
        # with the Makefile recipe, which exits the same way.
        #
        # There is deliberately no target that *writes* lang/. updatepofiles
        # rebuilds the .pot as the union of the resource files handed to it and
        # prunes everything else, from the .pot and from every .po -- and the
        # .rsj set present depends on which platform and build mode last ran.
        # See doc/LANGUAGES.md for the manual recipe.
        $pot = 'lang\Trndi.pot'
        if (-not (Test-Path $pot)) { Write-Error "$pot not found."; exit 1 }

        $res = @(Get-ChildItem -Recurse -Filter *.rsj lib -ErrorAction SilentlyContinue) +
               @(Get-ChildItem units\forms -Filter *.lrj -ErrorAction SilentlyContinue)
        if (-not $res) {
            Write-Host "No .rsj/.lrj found - build first (.\make.ps1), or the audit has nothing to compare."
            exit 0
        }

        # .rsj/.lrj are one JSON object per line; name is already the
        # 'unit.lowercaseident' key the .pot references with '#: '.
        #
        # Matched, not ConvertFrom-Json'd, because the value has to stay in its
        # on-disk escaped form: FPC escapes UTF-8 *per byte*, so a decode turns
        # the several \u escapes of one character into that many Latin-1 ones,
        # and the placeholder rule below tests for raw \uXXXX runs.
        # (?:[^"\\]|\\.)* is a JSON string body -- an escaped quote inside a
        # value (the Nightscout setup HTML has several) no longer cuts the
        # match short.
        $rx = '"name":"((?:[^"\\]|\\.)*)","sourcebytes":\[[^\]]*\],"value":"((?:[^"\\]|\\.)*)"'
        $pairs = @{}
        foreach ($f in $res) {
            foreach ($m in [regex]::Matches((Get-Content -Raw $f.FullName), $rx)) {
                $pairs[$m.Groups[1].Value] = $m.Groups[2].Value
            }
        }
        $inPot = [System.Collections.Generic.HashSet[string]]::new()
        foreach ($m in [regex]::Matches((Get-Content -Raw $pot), '(?m)^#: (.+)$')) {
            [void]$inPot.Add($m.Groups[1].Value.Trim())
        }
        Write-Host ("Resource strings in this checkout: {0}   entries in {1}: {2}" -f $pairs.Count, $pot, $inPot.Count)
        Write-Host ""

        # $extraArgs, not $args: the param() block above takes every argument
        # into $MakeArgs, which leaves $args permanently empty.
        $all = $extraArgs -contains '-all' -or $env:LANG_ALL
        $skipped = 0; $shown = 0
        foreach ($k in ($pairs.Keys | Where-Object { -not $inPot.Contains($_) } | Sort-Object)) {
            $v = $pairs[$k]
            $comp = ($k -split '\.')[-2]
            # Designer defaults that were never edited: '?' help buttons, the app
            # name, numeric mock-ups, pure punctuation/escapes, and captions still
            # equal to the component's own name.
            $ph = ($v -eq '?') -or ($v -eq 'Trndi') -or ($v -match '^[0-9]+%?$') -or
                  ($v -match '^[\p{P}\p{S}]+$') -or ($v -match '^(\\u[0-9A-Fa-f]{4})+$') -or
                  ($comp -and ($v.ToLowerInvariant() -eq $comp))
            if ($ph -and -not $all) { $skipped++; continue }
            if ($shown -eq 0) { Write-Host "Missing from $pot - untranslatable until added:" -ForegroundColor Yellow }
            $shown++
            Write-Host ("  {0,-44} = ""{1}""" -f $k, $v)
        }
        if ($shown -eq 0) { Write-Host "All resource strings built here are present in $pot." -ForegroundColor Green }
        if ($skipped) {
            Write-Host ""
            Write-Host "($skipped design-time placeholders skipped: ""?"" help buttons, unedited" -ForegroundColor DarkGray
            Write-Host " component names, numeric mock-ups. Pass -all to list them. The real fix is" -ForegroundColor DarkGray
            Write-Host " Localized=False on those properties in the Lazarus object inspector.)" -ForegroundColor DarkGray
        }

        # gettext is not standard on Windows; the .po half is best-effort.
        Write-Host ""
        $msgfmt = Get-Command msgfmt -ErrorAction SilentlyContinue
        if (-not $msgfmt) {
            Write-Host "msgfmt not found - skipping .po validation (install gettext, or run 'make lang-check' under WSL)." -ForegroundColor DarkGray
            exit 0
        }
        $rc = 0
        $null_out = Join-Path $env:TEMP 'trndi-msgfmt.mo'
        foreach ($po in Get-ChildItem lang -Filter 'Trndi.*.po') {
            Write-Host ("{0,-22} " -f $po.Name) -NoNewline
            & $msgfmt.Source --check-format --check-header --statistics -o $null_out $po.FullName
            if ($LASTEXITCODE -ne 0) { $rc = 1 }
        }
        Remove-Item $null_out -ErrorAction SilentlyContinue
        exit $rc
    }
    "help" {
        Write-Host "Trndi make.ps1" -ForegroundColor Cyan
        Write-Host "  ./make.ps1 [target] (no target -> release)" -ForegroundColor Cyan
        Write-Host "Targets:" -ForegroundColor Cyan
        Write-Host "  release          Build release ('Extensions (Release)' mode; default)"
        Write-Host "  debug            Build debug ('Extensions (Debug)' mode)"
        Write-Host "  noext            Build without extensions ('No Ext (Release)' mode; no QuickJS dependency)"
        Write-Host "  noext-debug      Build without extensions, debug ('No Ext (Debug)' mode)"
        Write-Host "  test             Build tests/TrndiTestConsole.lpi and run it (spawns an in-process test server;"
        Write-Host "                   set TRNDI_NO_TESTSERVER=1 to skip integration tests)"
        Write-Host "  ide-libs         Copy the QuickJS engine + ABI shim to the project root, for Extensions builds run from the Lazarus IDE (F9)"
        Write-Host "                   (the build targets above already do this on Windows)"
        Write-Host "  list-modules     Show Pascal 'unit' modules found under units/ as a tree"
        Write-Host "  assets           Regenerate compiled-in resource bundles (.lrs), e.g. the CareLink login helper (needs lazres)"
        Write-Host "  ptop             Regenerate ptop.cfg from JCFSettings.xml (formatter config for ptop; needs perl)"
        Write-Host "  lang-check       Audit lang/: resource strings missing from Trndi.pot, plus per-catalog stats"
        Write-Host "                   (read-only; -all lists design-time placeholders; .po validation needs gettext)"
        Write-Host "  clean            Remove build artifacts (*.o, *.ppu, executables, ...); use -n or --dry-run to preview"
        Write-Host "  help             Show this help"
        Write-Host "Notes:" -ForegroundColor Cyan
        Write-Host "  Extra arguments after a target are forwarded to lazbuild (or the test runner for 'test')."
        Write-Host "  Unknown targets are forwarded to lazbuild as-is."
        Write-Host "  Set LAZBUILD to override the lazbuild location (default: C:\lazarus\lazbuild.exe or PATH)."
        Write-Host "  Builds land in the project directory and are staged into build\ (binary + lang\, plus the"
        Write-Host "  QuickJS libraries for extensions modes). Set OUTDIR to stage somewhere else."
        exit 0
    }
    default { }
}

# Unknown/other args: forward all arguments to lazbuild
if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
if ($env:LAZBUILD) { Write-Host "Using LAZBUILD: $env:LAZBUILD" -ForegroundColor Cyan }
Write-Host "Forwarding to lazbuild: $laz $MakeArgs" -ForegroundColor Cyan
& $laz @MakeArgs
$exitCode = $LASTEXITCODE
exit $exitCode
