# Trndi Dictionary

This document defines special terms, abbreviations, and concepts used throughout the Trndi application and codebase.

## Table of Contents
- [Core Concepts](#core-concepts)
- [Blood Glucose (BG) Terms](#blood-glucose-bg-terms)
- [Data Sources & APIs](#data-sources--apis)
- [User Interface Elements](#user-interface-elements)
- [Technical Terms](#technical-terms)
- [Configuration & Settings](#configuration--settings)

---

## Core Concepts

### **CGM** (Continuous Glucose Monitoring)
A system that continuously monitors blood glucose levels, typically using a sensor worn on the body. Trndi displays data from CGM systems but is **NOT** a medical device itself.

### **BGReading**
A single blood glucose reading with associated metadata including:
- Current glucose value
- Delta (change from previous reading)
- Timestamp
- Trend direction
- Device/sensor information
- Signal quality (RSSI, noise)

### **BGResults**
An array of multiple `BGReading` objects, typically representing a timeline of glucose measurements.

---

## Blood Glucose (BG) Terms

### **BG Units**

#### **mmol/L** (Millimoles per Liter)
Standard unit for blood glucose measurement used internationally, particularly in Europe, Canada, and Australia.
- Normal range typically: 4.0-7.8 mmol/L
- Conversion: `mg/dL × 0.0555 = mmol/L`

#### **mg/dL** (Milligrams per Deciliter)
Unit for blood glucose measurement primarily used in the United States.
- Normal range typically: 70-140 mg/dL
- Conversion: `mmol/L × 18.0182 = mg/dL`

### **BGTrend** (Trend Direction)
The direction and rate of glucose change, represented by arrows:

| Trend | Arrow (UTF) | ASCII | Description | Rate of Change |
|-------|-------------|-------|-------------|----------------|
| **TdDoubleUp** | ↑↑ | ^^ | Rising Fast | ≥+3 mg/dL/min |
| **TdSingleUp** | ↑ | ^ | Rising | ≥+2 mg/dL/min |
| **TdFortyFiveUp** | ↗ | /´ | Rising Slightly | ≥+1 mg/dL/min |
| **TdFlat** | → | -> | Steady | -1 to +1 mg/dL/min |
| **TdFortyFiveDown** | ↘ | \_ | Falling Slightly | ≤-1 mg/dL/min |
| **TdSingleDown** | ↓ | \/ | Falling | ≤-2 mg/dL/min |
| **TdDoubleDown** | ↓↓ | \\/\/ | Falling Fast | ≤-3 mg/dL/min |
| **TdNotComputable** | ? | ? | Unknown | N/A |
| **TdPlaceholder** | X | X | Not Found | N/A |

### **BGValLevel** (Glucose Level Classification)
Classifies a reading relative to configured thresholds:
- **BGHIGH** - Above high threshold (urgent)
- **BGRangeHI** - In range but approaching high threshold (caution)
- **BGRange** - Normal range (optimal)
- **BGRangeLO** - In range but approaching low threshold (caution)
- **BGLOW** - Below low threshold (urgent)

### **Delta**
The change in glucose value between consecutive readings, typically measured over 5 minutes. Can be positive (rising) or negative (falling).

### **BG_NO_VAL**
Magic constant value (-904) used to indicate an unset or missing glucose reading.

---

## Data Sources & APIs

### **NightScout**
Open-source cloud-based CGM data management system. Users host their own server or use a hosted service. Trndi connects via REST API using URL and optional API secret.

### **Dexcom** / **Dexcom Share**
Commercial CGM system by Dexcom. Trndi can connect directly to Dexcom Share servers using Dexcom account credentials (username/password).

### **xDrip** / **xDrip+**
Open-source Android app for receiving CGM data. Trndi can connect to xDrip via local WiFi using the xDrip Web Service feature.

### **API Backend**
Abstract interface (`TrndiAPI`) that standardizes access to different data sources. Each source (NightScout, Dexcom, xDrip) implements this interface.

### **Debug APIs**
Testing/development data sources that generate fake glucose data:
- **Debug_FirstMissing** - Tests missing data handling
- **Debug_Edge** - Tests edge cases and boundary conditions
- **Debug_Custom** - Custom test scenarios

---

## User Interface Elements

### **Trend Dots**
Visual indicators showing recent glucose readings as dots arranged horizontally. Each dot represents a reading at a specific time, with the rightmost being most recent.

### **Dot Modes**
- **DOT_GRAPH** (•) - Standard dot display
- **DOT_FRESH** - Indicator for the most recent reading
- **Expanded** - Dot showing actual numeric value instead of just a dot

### **Arrow Display**
The main trend arrow showing current glucose direction. Can be displayed as:
- Simplified (↗, →, ↘) - Three basic directions
- Full set (↑↑, ↑, ↗, →, ↘, ↓, ↓↓) - All seven trend levels

### **Predictions Display**

#### **Full Mode**
Shows three predictions with times, arrows, and values:
```
⏱5' ↗ 145.2 | ⏱10' → 147.8 | ⏱15' ↘ 146.1
```

#### **Short Mode**
Simplified display with two options:
- **Arrow only** - Single large arrow: `↗`
- **Time and value** - Clock, arrow, and predicted value: `⏱10' ↗ 145.2`

### **Privacy Mode**
Hides actual glucose numbers, showing only dots and trend indicators. Useful when sharing screen or in public settings.

### **Floating Window**
Secondary window that can be positioned anywhere on screen, showing a simplified view of current glucose.

### **Off-Range Indicator**
Visual sidebar that appears when glucose is outside configured thresholds, color-coded for high (red) or low (yellow/orange).

---

## Technical Terms

### **TIR** (Time In Range)
Percentage of time glucose readings are within configured target range. Calculated over a specified time window (e.g., 24 hours).

### **RSSI** (Received Signal Strength Indicator)
Measure of CGM sensor signal strength, typically -1 if not available. Higher values indicate better signal.

### **Noise**
Signal quality metric from CGM sensor. Higher noise values may indicate less reliable readings.

### **Extensions** / **JavaScript Extensions**
Custom JavaScript code that can extend Trndi's functionality. Runs in an embedded JavaScript engine (QuickJS) with access to specific Trndi APIs.

### **WebAPI**
HTTP REST API exposed by Trndi for third-party applications to access current glucose data. See [WebAPI documentation](doc/WebAPI.md).

### **Razer Chroma**
RGB lighting integration that changes color of Razer peripherals (keyboards, mice) based on glucose levels.

### **Media Controller** / **System Media Controller**
Integration with Spotify/Deezer to play specific songs or pause playback when glucose hits certain levels.

### **ScaleLbl**
Internal function that automatically scales label font size to fit available space using binary search algorithm.

---

## Configuration & Settings

### **Override** / **Custom Thresholds**
User-defined high/low thresholds that override the default values from the CGM backend.

### **Range Settings**
Inner thresholds defining a preferred/optimal glucose range within the broader safe range:
- **cgmHi** / **cgmLo** - Outer safe boundaries (typically from API)
- **cgmRangeHi** / **cgmRangeLo** - Inner optimal boundaries (user-defined)

### **Predictions**
Experimental feature that estimates future glucose values based on current trend and historical data:
- **predictions.enable** - Toggle predictions on/off
- **predictions.short** - Use simplified short mode display
- **predictions.short.fullarrows** - Use full arrow set (↑↑, ↑, etc.)
- **predictions.short.showvalue** - Show time and value with arrow
- **predictions.short.size** - Arrow size in short mode (1=small, 2=medium, 3=big)

### **Multi-User** / **Parallel Users**
Feature allowing multiple Trndi instances to run simultaneously, each monitoring a different person's glucose data.

### **Fresh Threshold**
Time limit (in minutes) after which a reading is considered "stale" and gets visually marked (e.g., strikethrough text). Default is typically 10-15 minutes.

### **Timezone Offset** (tz)
Time difference in seconds between local time and UTC, used to correctly interpret timestamps from APIs.

### **Settings Storage**
Platform-specific location where Trndi saves configuration:
- **Windows**: Registry or AppData
- **Linux**: `~/.config/trndi/` or `~/.local/share/trndi/`
- **macOS**: `~/Library/Preferences/`

### **Native Module** (tNative)
Platform-specific code layer that handles OS-specific functionality like file I/O, settings storage, notifications, and system integration.

---

## Display States

### **Dot Scale** (dotscale)
Multiplier for trend dot size, adjustable via settings.

### **Touch Mode** / **Semi-Touch Mode**
Optimizations for touchscreen devices, adjusting UI element sizes and interaction methods.

### **Badge** / **Tray Icon**
Icon in system tray/notification area showing current glucose value with color coding.

### **Color Modes**
Visual states based on glucose level:
- **Normal** - Within optimal range (typically green)
- **High** - Above high threshold (typically red)
- **Low** - Below low threshold (typically orange/yellow)
- **Range High/Low** - In safe range but near boundaries (typically transitional colors)

---

## File Locations

### **Cache Directory**
Location where Trndi stores temporary data:
- Linux: `${XDG_CACHE_HOME:-$HOME/.cache}/trndi/`
- Contains files like `current.txt` with latest reading

### **Extensions Directory**
Where JavaScript extension files (.js) are stored and loaded from.

### **Log Files**
Debug/diagnostic logs stored per-platform.

---

## Abbreviations

- **API** - Application Programming Interface
- **BG** - Blood Glucose
- **CGM** - Continuous Glucose Monitoring
- **FPC** - Free Pascal Compiler
- **JS** - JavaScript
- **JSON** - JavaScript Object Notation
- **LCL** - Lazarus Component Library
- **NS** - NightScout
- **REST** - Representational State Transfer
- **RSSI** - Received Signal Strength Indicator
- **SGV** - Sensor Glucose Value (NightScout term)
- **TDateTime** - Delphi/Pascal date/time type
- **TIR** - Time In Range
- **UI** - User Interface
- **UTC** - Coordinated Universal Time
- **UTF** - Unicode Transformation Format

---

## See Also

- [API Documentation](guides/API.md)
- [Extensions Documentation](guides/Extensions.md)
- [Predictions Guide](guides/Predictions.md)
- [WebAPI Documentation](doc/WebAPI.md)
- [Multi-user Guide](guides/Multiuser.md)
