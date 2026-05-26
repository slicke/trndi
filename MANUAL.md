# Trndi User Guide - Understanding Your Display

This guide explains the terms and features you'll see when using Trndi in everyday language.

## Table of Contents
- [The Basics](#the-basics)
- [Understanding Your Readings](#understanding-your-readings)
- [Arrows and What They Mean](#arrows-and-what-they-mean)
- [Display Features](#display-features)
- [Alerts and Notifications](#alerts-and-notifications)
- [Settings You Should Know](#settings-you-should-know)
- [Troubleshooting Common Questions](#troubleshooting-common-questions)

---

## The Basics

### What is Trndi?
Trndi is a desktop application that shows your blood sugar (glucose) readings on your computer screen. It connects to your existing glucose monitoring system and displays the information in an easy-to-read format.

**Important**: Trndi is NOT a medical device. Always verify important information with your official CGM device before making medical decisions.

### CGM (Continuous Glucose Monitor)
This is the sensor and system you wear that measures your blood sugar continuously. Examples include Dexcom, FreeStyle Libre, or Medtronic systems. Trndi doesn't replace these - it just shows their data on your computer.

### Where does Trndi get my data?
Trndi can connect to:
- **NightScout** - A website where your CGM data is uploaded (you or your caregiver sets this up)
- **Dexcom Share** - Dexcom's official data sharing service
- **xDrip** - An Android app that can share data over your local WiFi
- ***Tandem Source*** - Tandem Diabetes management app (_NOTE: Source has a lag! This is not Trndi's fault._)

---

## Understanding Your Readings

### Blood Sugar Units

#### mmol/L (millimoles per liter)
Used in most countries outside the USA. Typical numbers:
- **Low**: Below 4.0 mmol/L
- **Normal**: 4.0 - 7.8 mmol/L
- **High**: Above 10.0 mmol/L

#### mg/dL (milligrams per deciliter)
Used primarily in the United States. Typical numbers:
- **Low**: Below 70 mg/dL
- **Normal**: 70 - 140 mg/dL
- **High**: Above 180 mg/dL

**Tip**: You can switch between these units in Trndi's settings (right-click menu → Settings → Unit).

### The Number on Your Screen
This is your current blood sugar reading, shown in your preferred unit (mmol/L or mg/dL). The color tells you at a glance if it's in range:
- 🟢 **Green**: Normal range - you're good!
- 🔴 **Red**: High - above your target
- 🟡 **Orange/Yellow**: Low - below your target

### "Ago" Time
Shows how old the reading is (e.g., "3 min ago"). If this number gets large (like "15 min ago"), it might mean:
- Your sensor lost connection
- Your phone/uploader is offline
- Trndi can't reach the data source

**When to worry**: If it says more than 15-20 minutes ago, check your CGM system.

---

## Arrows and What They Mean

### Basic Arrows (Simplified Mode)
When predictions are enabled in short mode, you'll see three basic arrows:

| Arrow | What It Means | What To Do |
|-------|---------------|------------|
| ↗ | **Rising** - Your sugar is going up | Watch it, might need to take action soon |
| → | **Steady** - Staying about the same | Keep doing what you're doing |
| ↘ | **Falling** - Your sugar is going down | Watch it, might need carbs soon |

### Detailed Arrows (Full Set)
If you enable "full arrow set" in settings, you get more precise information:

| Arrow | What It Means | Speed |
|-------|---------------|-------|
| ↑↑ | **Rising Very Fast** | Going up quickly (≥3 mg/dL per minute) |
| ↑ | **Rising Fast** | Going up steadily (≥2 mg/dL per minute) |
| ↗ | **Rising Slowly** | Going up gently (≥1 mg/dL per minute) |
| → | **Steady** | Not changing much (±1 mg/dL per minute) |
| ↘ | **Falling Slowly** | Going down gently (≤-1 mg/dL per minute) |
| ↓ | **Falling Fast** | Going down steadily (≤-2 mg/dL per minute) |
| ↓↓ | **Falling Very Fast** | Going down quickly (≤-3 mg/dL per minute) |

**Why this matters**: Double arrows (↑↑ or ↓↓) mean rapid changes - you should pay extra attention!

---

## Display Features

### The Dots (Trend Graph)
Those small dots you see across the screen show your recent readings over time:
- **Rightmost dot** = Most recent reading (that's now!)
- **Dots to the left** = Your readings going back in time
- **Dot color** = Same as your current reading (green/red/yellow)

**How to use them**: Click on any dot to see the actual number for that reading.

### Predictions (Experimental)
When enabled, Trndi can show where your blood sugar might be heading in the next 5-15 minutes.

#### What you'll see:
- **Full mode**: `⏱5' ↗ 145.2 | ⏱10' → 147.8 | ⏱15' ↘ 146.1`
  - Shows predictions at 5, 10, and 15 minutes with values
  
- **Short mode**: Just an arrow or `⏱10' ↗ 145.2`
  - Shows just the 10-minute prediction

**Important**: Predictions are estimates based on your current trend. They don't know about food you just ate, insulin you just took, or exercise. Use them as a guide, not a guarantee!

### Privacy Mode
Hides your actual numbers, showing only dots and arrows. Useful when:
- Screen sharing in a video call
- Someone's watching your screen
- You want less detail at a glance

Enable it by right-clicking → Privacy Mode.

### Time In Range (TIR)
Shows what percentage of time your blood sugar has been in your target range (e.g., "85% TIR" means you were in range 85% of the time).
- **Good**: Above 70% in range
- **Needs work**: Below 70% in range

---

## Alerts and Notifications

### Visual Alerts
- **Red sidebar**: Your blood sugar is too high
- **Orange sidebar**: Your blood sugar is too low
- **Flashing**: Can be enabled for urgent situations

### Sound and System Notifications
Can be configured in settings to:
- Play a sound when you go high or low
- Show desktop notifications
- Pause your music (Spotify/Deezer) to get your attention

---

## Settings You Should Know

### High and Low Thresholds
These are YOUR personal limits. Everyone's different!
- **Low threshold**: Below this, Trndi shows warnings (e.g., 4.0 mmol/L or 70 mg/dL)
- **High threshold**: Above this, Trndi shows warnings (e.g., 10.0 mmol/L or 180 mg/dL)

**Where to set them**: Right-click → Settings → Customization tab

### Custom Range (Optional)
Want tighter control? Set a "preferred range" within your safe range:
- Example: Safe range 4-10 mmol/L, preferred range 5-8 mmol/L
- Shows yellow when you're safe but outside preferred range

### Multi-User Mode
If you need to monitor multiple people (like family members), you can:
1. Set up multiple accounts in Settings → Accounts
2. Start Trndi multiple times - each window picks a different person
3. Each person can have different settings and data sources

---

## Troubleshooting Common Questions

### Why is my reading crossed out?
The reading is more than 15 minutes old. Trndi marks old data to show it might not be current.
- **Fix**: Check that your CGM, phone, or uploader is working

### Why does it say "No data" or "?"
Trndi can't get readings from your data source.
- **Check**: Your internet connection
- **Check**: Your NightScout URL is correct
- **Check**: Your Dexcom/xDrip credentials are correct

### The colors don't match my preferences
You might need to set your own thresholds:
- Right-click → Settings → Customization
- Enable "Override official high/low" 
- Set your preferred limits

### Can I make the window bigger/smaller?
Yes! Just resize the window like any other app. Trndi will remember your size if you enable "Remember window size" in Settings.

### Can I move it to a specific screen corner?
Right-click → Position, then choose:
- Bottom Left
- Bottom Right  
- Top Right
- Center
- Custom (you pick)

### What's the difference between the main window and floating window?
- **Main window**: Full featured with all options
- **Floating window**: Smaller, simplified view you can position anywhere
- Both show the same data from your selected account

### The app disappeared from my screen!
Right-click the tray icon (near your clock) and select "Show Trndi" or try clicking Reset Position in settings.

### How do I know if Trndi is working correctly?
Look for:
- ✅ Current reading matches your CGM device
- ✅ "Ago" time is recent (under 5-10 minutes)
- ✅ Trend arrow makes sense based on recent changes
- ✅ Colors reflect your actual blood sugar level

---

## Quick Tips

### Getting Started
1. Right-click the window for the menu
2. Go to Settings to enter your data source (NightScout URL or Dexcom login)
3. Set your high/low thresholds if needed
4. That's it - readings should start appearing!

### Making It Yours
- **Change colors**: Settings → Colors tab
- **Change fonts**: Settings → Display tab
- **Add JavaScript extensions**: See the Extensions guide for advanced features
- **Keyboard shortcuts**: Settings → General tab

### Best Practices
- ✅ Keep Trndi running in the background
- ✅ Verify important readings with your official CGM
- ✅ Set alerts for high/low if you want them
- ✅ Update Trndi occasionally (check for new versions)
- ⚠️ Never rely solely on Trndi for medical decisions

---

## Need More Help?

- 💬 Join the [Discord community](https://discord.gg/QXACfpcW)
- 📖 Check the [full documentation](README.md)
- 🐛 Report issues on [GitHub](https://github.com/slicke/trndi/issues)
- 🌍 Help translate Trndi into your language

---

**Remember**: Trndi is a tool to help you see your glucose data more easily. It works alongside your medical devices, not instead of them. Always consult with your healthcare provider about your diabetes management.
