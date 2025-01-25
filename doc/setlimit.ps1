# Set path to extensions folder
$extensionsPath = Join-Path $env:LOCALAPPDATA "Trndi\extensions"

# Create directory if it doesn't exist
if (-not (Test-Path $extensionsPath)) {
    New-Item -ItemType Directory -Path $extensionsPath | Out-Null
}

# Function to convert input value
function Convert-Value($value) {
    if ([double]::TryParse($value, [ref]$null)) {
        if ($value -match '^\d+$') {
            # It's mg/dL
            return [int]$value
        } else {
            # It's mmol/L
            return [math]::Round([double]$value * 18)
        }
    } else {
        Write-Host "Invalid input. Please enter a numeric value."
        exit
    }
}

# Ask for Low value
$lowValueInput = Read-Host "Enter Low value (mg/dL or mmol/L)"
$lowValue = Convert-Value $lowValueInput

# Ask for High value
$highValueInput = Read-Host "Enter High value (mg/dL or mmol/L)"
$highValue = Convert-Value $highValueInput

# Create file content
$content = "setLimits($lowValue, $highValue);"

# Write to file
$filePath = Join-Path $extensionsPath "limitoverride.js"
$content | Out-File -FilePath $filePath -Encoding UTF8

Write-Host "File has been created/updated: $filePath"
