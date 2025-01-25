#!/bin/bash

# Set path to extensions folder
extensions_path="$HOME/.config/Trndi/extensions"

# Create directory if it doesn't exist
mkdir -p "$extensions_path"

# Function to convert input value
convert_value() {
    local value=$1
    if [[ $value =~ ^[0-9]+$ ]]; then
        # It's mg/dL
        echo "$value"
    elif [[ $value =~ ^[0-9]+\.[0-9]+$ ]]; then
        # It's mmol/L
        echo "$(echo "scale=0; $value * 18 / 1" | bc)"
    else
        echo "Invalid input. Please enter a numeric value."
        exit 1
    fi
}

# Ask for Low value
read -p "Enter Low value (mg/dL or mmol/L): " low_value_input
low_value=$(convert_value "$low_value_input")

# Ask for High value
read -p "Enter High value (mg/dL or mmol/L): " high_value_input
high_value=$(convert_value "$high_value_input")

# Create content and write to file
echo "setLimits($low_value, $high_value);" > "$extensions_path/limitoverride.js"

echo "File has been created/updated: $extensions_path/limitoverride.js"
