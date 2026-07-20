<#
make.ps1 — Windows helper to run `lazbuild` and provide common shortcuts

Usage:
  ./make.ps1 [release|debug|noext|noext-debug|list-modules|test|assets|fetch-mormot2|install-mormot2|check-mormot2|clean[-n|--dry-run]|help] or ./make.ps1 [lazbuild-args...]

Behavior:
 - Sets `LAZBUILD` to `C:\lazarus\lazbuild.exe` if present and `LAZBUILD` is not already set
 - Ensures `OS=Windows_NT` environment variable is set for compatibility with the Makefile
 - Provides shortcuts (release, debug, noext, noext-debug, list-modules) that invoke `lazbuild` or enumerate units
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

switch ($firstArg) {
    "" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        exit $LASTEXITCODE
    }
    "release" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        exit $LASTEXITCODE
    }
    "debug" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Debug)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        exit $LASTEXITCODE
    }
    "noext" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'No Ext (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        exit $LASTEXITCODE
    }
    "noext-debug" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'No Ext (Debug)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        exit $LASTEXITCODE
    }
    "test" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }

        Write-Host "Building console tests (tests/TrndiTestConsole.lpi)" -ForegroundColor Cyan
        & $laz -B 'tests/TrndiTestConsole.lpi' @extraArgs
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

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
    "fetch-mormot2" {
        # Mirrors the Makefile's fetch-mormot2: clone the pinned mORMot2 commit
        # into externals\mORMot2 and extract the QuickJS static archive into .\static.
        $repo      = 'https://github.com/synopse/mORMot2.git'
        $commit    = 'b72f260b880557d2f9ebc15905d820e7a3a9bf01'
        $staticUrl = 'https://github.com/synopse/mORMot2/releases/download/2.4-stable/mormot2static.7z'
        if (Test-Path 'externals\mORMot2') { Write-Error "externals\mORMot2 already exists; remove it to re-clone"; exit 1 }
        New-Item -ItemType Directory -Force -Path 'externals' | Out-Null
        Write-Host "Fetching mORMot2 into externals\mORMot2 (commit $commit)" -ForegroundColor Cyan
        git -c init.defaultBranch=main init externals/mORMot2
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        git -C externals/mORMot2 remote add origin $repo
        git -C externals/mORMot2 fetch --depth 1 origin $commit
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        git -C externals/mORMot2 checkout FETCH_HEAD
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        $archive = Join-Path $env:TEMP 'mormot2static.7z'
        try {
            Write-Host "Downloading $staticUrl" -ForegroundColor Cyan
            Invoke-WebRequest -Uri $staticUrl -OutFile $archive
        } catch {
            Write-Host "Could not download mormot2static.7z automatically; download $staticUrl and extract into .\static" -ForegroundColor Yellow
            $archive = $null
        }
        if ($archive -and (Test-Path $archive)) {
            New-Item -ItemType Directory -Force -Path 'static' | Out-Null
            $sevenZip = Get-Command 7z -ErrorAction SilentlyContinue
            if ($sevenZip) { & $sevenZip.Source x $archive '-ostatic' '-y' }
            else { tar -xf $archive -C static }  # Windows tar (bsdtar) can read 7z archives
            if ($LASTEXITCODE -ne 0) { Write-Host "Extraction failed; extract the archive into .\static manually" -ForegroundColor Yellow }
            else { Write-Host "Extracted static\" -ForegroundColor Green; Remove-Item $archive -Force -ErrorAction SilentlyContinue }
        }
        Write-Host "Done. Run './make.ps1 install-mormot2' to compile the package so lazbuild can find it." -ForegroundColor Cyan
        exit 0
    }
    "install-mormot2" {
        # Compile mORMot2's Lazarus package (same as CI). lazbuild registers a
        # compiled .lpk in the user's package links, which is what lets a later
        # 'lazbuild Trndi.lpi' resolve the bare "mormot2" dependency.
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $lpk = 'externals\mORMot2\packages\lazarus\mormot2.lpk'
        if (-not (Test-Path $lpk)) { Write-Error "$lpk not found; run './make.ps1 fetch-mormot2' first"; exit 1 }
        Write-Host "Compiling mORMot2 package (registers it with Lazarus/lazbuild)" -ForegroundColor Cyan
        & $laz $lpk
        exit $LASTEXITCODE
    }
    "check-mormot2" {
        Write-Host "Checking mORMot2 presence and QuickJS static artifacts..." -ForegroundColor Cyan
        $lazCfg = Join-Path $env:LOCALAPPDATA 'lazarus'
        $pkgXml = Join-Path $lazCfg 'packagefiles.xml'
        $opmDir = Join-Path $lazCfg 'onlinepackagemanager\packages\mORMot2'
        $staticDirs = @((Join-Path $opmDir 'static'), 'externals\mORMot2\static', 'static')
        # Package links registered with Lazarus (what lazbuild consults to resolve "mormot2")
        $regFound = $false
        if (Test-Path $pkgXml) {
            $lpks = Select-String -Path $pkgXml -Pattern '<Filename Value="([^"]*mormot2\.lpk)"' |
                ForEach-Object { $_.Matches[0].Groups[1].Value }
            foreach ($lpk in $lpks) {
                if (Test-Path $lpk) {
                    $regFound = $true
                    $root = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $lpk))
                    $staticDirs += (Join-Path $root 'static')
                }
            }
        }
        if ($regFound) { Write-Host "mormot2.lpk is registered in $pkgXml" }
        if (-not $regFound -and -not (Test-Path $opmDir) -and -not (Test-Path 'externals\mORMot2') -and -not (Test-Path 'static')) {
            Write-Error "mORMot2 not found (no mormot2.lpk registered in $pkgXml, no externals\mORMot2 or .\static). Run './make.ps1 fetch-mormot2' followed by './make.ps1 install-mormot2'."
            exit 1
        }
        $found = $false
        foreach ($d in $staticDirs) {
            if ((Test-Path $d) -and (Get-ChildItem -Path $d -Recurse -File -Include '*quickjs*.a','*quickjs*.o','*quickjs*.obj','*quickjs*.so' -ErrorAction SilentlyContinue | Select-Object -First 1)) {
                $found = $true; break
            }
        }
        if (-not $found) {
            Write-Error "QuickJS static artifacts were not found. Run './make.ps1 fetch-mormot2' to fetch them into .\static."
            exit 1
        }
        Write-Host "mORMot2 and QuickJS static artifacts found." -ForegroundColor Green
        exit 0
    }
    "help" {
        Write-Host "Trndi make.ps1" -ForegroundColor Cyan
        Write-Host "  ./make.ps1 [target] (no target -> release)" -ForegroundColor Cyan
        Write-Host "Targets:" -ForegroundColor Cyan
        Write-Host "  release          Build release ('Extensions (Release)' mode; default)"
        Write-Host "  debug            Build debug ('Extensions (Debug)' mode)"
        Write-Host "  noext            Build without extensions ('No Ext (Release)' mode; no mORMot2/QuickJS dependency)"
        Write-Host "  noext-debug      Build without extensions, debug ('No Ext (Debug)' mode)"
        Write-Host "  test             Build tests/TrndiTestConsole.lpi and run it (spawns an in-process test server;"
        Write-Host "                   set TRNDI_NO_TESTSERVER=1 to skip integration tests)"
        Write-Host "  list-modules     Show Pascal 'unit' modules found under units/ as a tree"
        Write-Host "  assets           Regenerate compiled-in resource bundles (.lrs), e.g. the CareLink login helper (needs lazres)"
        Write-Host "  fetch-mormot2    Clone the pinned mORMot2 commit into externals\mORMot2 and extract QuickJS statics into .\static"
        Write-Host "  install-mormot2  Compile the fetched mormot2.lpk so lazbuild can resolve the mormot2 dependency"
        Write-Host "  check-mormot2    Verify mORMot2 (registered .lpk, OPM, externals or .\static) and QuickJS statics"
        Write-Host "  clean            Remove build artifacts (*.o, *.ppu, executables, ...); use -n or --dry-run to preview"
        Write-Host "  help             Show this help"
        Write-Host "Notes:" -ForegroundColor Cyan
        Write-Host "  Extra arguments after a target are forwarded to lazbuild (or the test runner for 'test')."
        Write-Host "  Unknown targets are forwarded to lazbuild as-is."
        Write-Host "  Set LAZBUILD to override the lazbuild location (default: C:\lazarus\lazbuild.exe or PATH)."
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
