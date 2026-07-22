<#
make.ps1 — Windows helper to run `lazbuild` and provide common shortcuts

Usage:
  ./make.ps1 [release|debug|noext|noext-debug|list-modules|test|assets|clean[-n|--dry-run]|help] or ./make.ps1 [lazbuild-args...]

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

# The extension engine links quickjs-ng and its ABI shim as shared libraries.
# Windows resolves them from the executable's own directory, so place them next
# to Trndi.exe after an extensions-enabled build. See externals/quickjs/README.md.
function Copy-QuickJSLibs {
    $src = Join-Path $PSScriptRoot 'externals\quickjs\prebuilt\x86_64-win64'
    if (-not (Test-Path $src)) {
        Write-Warning "QuickJS libraries not found at $src - extensions will fail to start. Rebuild them with externals/quickjs/build.sh."
        return
    }
    foreach ($dll in (Get-ChildItem (Join-Path $src '*.dll'))) {
        Copy-Item $dll.FullName $PSScriptRoot -Force
        Write-Host "  copied $($dll.Name)" -ForegroundColor DarkGray
    }
}

switch ($firstArg) {
    "" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Copy-QuickJSLibs }
        exit $LASTEXITCODE
    }
    "release" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Release)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Copy-QuickJSLibs }
        exit $LASTEXITCODE
    }
    "debug" {
        if (-not $laz) { Write-Error "lazbuild not found. Install Lazarus or set LAZBUILD."; exit 1 }
        $mode = 'Extensions (Debug)'
        Write-Host "Running: $laz --build-mode=`"$mode`" Trndi.lpi" -ForegroundColor Cyan
        & $laz "--build-mode=$mode" 'Trndi.lpi' @extraArgs
        if ($LASTEXITCODE -eq 0) { Copy-QuickJSLibs }
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
        Write-Host "  list-modules     Show Pascal 'unit' modules found under units/ as a tree"
        Write-Host "  assets           Regenerate compiled-in resource bundles (.lrs), e.g. the CareLink login helper (needs lazres)"
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
