<#
make.ps1 — Windows helper to run `lazbuild` and provide common shortcuts

Usage:
  ./make.ps1 [release|debug|noext|noext-debug] or ./make.ps1 [lazbuild-args...]

Behavior:
 - Sets `LAZBUILD` to `C:\lazarus\lazbuild.exe` if present and `LAZBUILD` is not already set
 - Ensures `OS=Windows_NT` environment variable is set for compatibility with the Makefile
 - Provides shortcuts (release, debug, noext, noext-debug) that invoke `lazbuild` with appropriate flags
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
    "help" {
        Write-Host "Usage: ./make.ps1 [release|debug|noext|noext-debug] (no args -> release)" -ForegroundColor Cyan
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
