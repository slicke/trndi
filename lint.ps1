# Ställ in sökvägarna
$ptopPath = "C:\lazarus\fpc\3.2.2\bin\x86_64-win64\ptop.exe"
$configPath = ".\style.ptop.cfg"
$currentDir = Get-Location

# Hämta alla .pp-filer i den aktuella katalogen
Get-ChildItem -Path $currentDir -Filter *.pp | ForEach-Object {
    $file = $_.FullName
    Write-Host "Formatting $file..."
    & $ptopPath -c $configPath $file $file
}

Write-Host "Done!"

