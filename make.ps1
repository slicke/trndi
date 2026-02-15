<#
make.ps1 — Windows helper to run `lazbuild` and provide common shortcuts

Usage:
  ./make.ps1 [release|debug|noext|noext-debug|list-modules] or ./make.ps1 [lazbuild-args...]

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
            Write-Host "Running tests (PHP detection enabled; set TRNDI_NO_PHP=1 to force skip)" -ForegroundColor Cyan
        # Auto-clear TRNDI_NO_PHP if a PHP executable is available (convenience)
        if ($env:TRNDI_NO_PHP -eq '1') {
            if ($env:TRNDI_PHP_EXECUTABLE -or (Test-Path 'C:\php\php.exe')) {
                Write-Host "TRNDI_NO_PHP is set but PHP executable found; clearing TRNDI_NO_PHP" -ForegroundColor Yellow
                Remove-Item Env:\TRNDI_NO_PHP -ErrorAction SilentlyContinue
            }
            else {
                try {
                    & php -v > $null 2>&1
                    if ($LASTEXITCODE -eq 0) {
                        Write-Host "TRNDI_NO_PHP is set but 'php' exists on PATH; clearing TRNDI_NO_PHP" -ForegroundColor Yellow
                        Remove-Item Env:\TRNDI_NO_PHP -ErrorAction SilentlyContinue
                    }
                } catch { }
            }
        }

        # Auto-detect PHP for convenience: prefer explicit env override, then C:\php\php.exe, then 'php' on PATH
        if (-not $env:TRNDI_TEST_SERVER_URL -and -not $env:TRNDI_NO_PHP) {
            if (-not $env:TRNDI_PHP_EXECUTABLE) {
                if (Test-Path 'C:\php\php.exe') {
                    $env:TRNDI_PHP_EXECUTABLE = 'C:\php\php.exe'
                }
                else {
                    try {
                        & php -v > $null 2>&1
                        if ($LASTEXITCODE -eq 0) { $env:TRNDI_PHP_EXECUTABLE = 'php' }
                    } catch { }
                }
            }

            if ($env:TRNDI_PHP_EXECUTABLE) {
                Write-Host "Detected PHP: $env:TRNDI_PHP_EXECUTABLE" -ForegroundColor Green
            }
            else {
                Write-Host "No PHP detected; PHP-dependent tests will be skipped unless TRNDI_TEST_SERVER_URL is set." -ForegroundColor Yellow
            }
        }

        & 'tests/TrndiTestConsole.exe' @extraArgs
        exit $LASTEXITCODE
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
    "help" {
        Write-Host "Usage: ./make.ps1 [release|debug|noext|noext-debug|list-modules|test] (no args -> release)" -ForegroundColor Cyan
        Write-Host "Other arguments are forwarded to lazbuild when using these shortcuts." -ForegroundColor Cyan
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
