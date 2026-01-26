$ErrorActionPreference = 'Stop'

$packageName = 'trndi'
$toolsDir = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"

# Create a shim for the executable
$exePath = Join-Path $toolsDir 'Trndi.exe'

# Install the shim (Chocolatey will automatically create this)
# No additional installation steps needed for portable app
Write-Host "Trndi has been installed to: $toolsDir"
Write-Host "You can now run 'Trndi' from any command prompt."
