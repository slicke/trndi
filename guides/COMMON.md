# Common settings
Trndi features lots of settings, this guide provides an overview of the most common ones that users might change

## Quick Start
This guide, unless explicitly stating otherwise, talks about the __Settings dialog__. It is accessed by _right-clicking_ the main window, and choosing __Settings__.

## General tab
### Trend window
This controls how many "dots" you see on the main window, the history of your glucose levels.

## Display tab
### Rotate trend arrow by rate of change
Typically the trend used by Dexcom/NightScout/CareLink/etc is flat, 90 degree up/down or straight up/down. This makes the arrow turn more dynamically, with a slight angle on smaller changes.

### Highlight the newest dot while the reading is fresh
While the latest reading is recent, it will be drawn with an outline.

### Use range color for window background
This makes the title bar of the app colored by the window color. Eg there entire window, including the title, has the same color.

## Time-in-range
### Color the graph's background...
This adds a color on tuop of the graph, that shows your range (what's high and low), or your custom range (what's the most desirable values).

### Override Custom Range
This allows you to set which range/values you think are the most desireable. This is a smaller range than your high/low levels.

## Customization
### Override high / Low limits
These are the values you consider high/low

## Predictions
These settings control the experimental _prediction engine_ that helps predict future glucose values.

### Show glucose predictions
Enables the engine. Predictions will show in the lower-right side of the main window.

#### Show only trend arrows / 1 reading
This shows just one reading instead of multiple in the lower right.

#### Use full arrow set
All arrows will be used, otherwise high/low will be the only ones shown (in addition to flat)

#### Show predictions as dots on the trend
This places dots in the main trend (X instead of O) in the future to allow you to see the predictions in the graph.

## System
### Start with system
Starts Trndi when your PC/Mac starts.

### Enable Web API
This is an advanced feature for developers, see [WebAPI](/doc/WebAPI.md). It allows Trndi to provide data to other systems.

## Extensions
Extensions are programs developers can write to extend the features of Trndi. They work on Windows and Linux (amd64) and are written in the _JavaScript_ language. 

Developers can share extensions, which you can install by placing the extension in Trndi's configruation folder. (See _Show Folder_.)

# Special settings
## Multi user
This allows you to start Trndi with more than one account.
Each account has it's own backend and can be assigned a name and color.
> Don't forget to click both Save in the "User" box and when saving changes if you modify a profile!