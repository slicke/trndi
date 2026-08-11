# Set up Tandem — Beta

> ⚠️ **Beta.** This backend may not work as intended; check the t:connect app before acting on a reading. As always: Trndi is not a medical device.

> Depending on your setup, some accounts only provide readings every 60 minutes ca. This is a Tandem Source limitation, not a Trndi bug. When it happens, Trndi keeps showing the last reading it received, and may report it as outdated — that is the data being old, not the connection being broken.

## Preparations
### Setup Tandem Source
Register on the website and install the Android/iPhone app. Then connect it to your pump.

## Setup Trndi
Use the setup wizard; if already setup:
Open Trndi and open the settings (if they aren't alredy showing), by right-clicking and choosing settings (long-press for touch screens).

### Configuring the settings dialog
1. Use the drop-down and choose Tandem t:connect (with your region)
2. Enter your __Tandem Source e-mail__
3. Enter your __Tandem Source password__

#### Display units
4. (If using the wizard, click Next first). Choose __mmol/L__ in the bottom, if you live in a country (such as Sweden) where it's used, or __mg/dL__ in countries such as the USA where that is used

Now close the window, then close Trndi (if it doesn't close itself) and start it again.

# What Trndi reads from Tandem Source

The pump-logs payload the readings come from carries a good deal more than glucose, and Trndi now reads some of it:

- **Sensor glucose values** from the CGM the pump is paired with (Dexcom G6/G7, FreeStyle Libre 2/3)
- **Insulin doses on the history graph**, if you turn them on (Settings → Display → *Show insulin doses on the history graph*). Doses appear as stems along the bottom of the graph, labelled in units.

  A bolus is reported as what the pump actually delivered, not what was asked for, so an interrupted bolus shows the smaller real figure. Stem heights are relative to the largest dose on screen rather than a fixed scale — read the labels, not the heights. The graph only shows doses for the period the last fetch covered, so an empty stretch means "nothing was reported", never "no insulin was given".

  Note that the second checkbox, for the pump's own automatic doses, does nothing on Tandem yet — see below.

- **Carbohydrates on the history graph** (Settings → Display → *Show carbohydrates on the history graph*), as discs in their own lane above the bottom axis, labelled in grams. These are the carbs you entered into the bolus calculator; Tandem has no separate meal entry, so unlike CareLink there is no double-counting to reconcile. Carbs entered for a bolus that was never delivered are not shown.
- **Basal rate** (Menu → Basal rate), in U/hr. This is the rate Control-IQ last commanded, which on a looping pump is generally not your programmed profile rate.

The `ibc` property on the pump's status and battery events drives the pump-battery notifications (20/15/10/5/2 percent) described in [Notifications.md](Notifications.md). It is a true fine-grained percentage — a live fetch carries values like 80, 45 and 35 — with 255 meaning the pump could not read it.

Read from the payload but not shown in the UI yet: reservoir level and whether delivery is suspended.

Not available from this backend: sensor life (so the sensor-expiry notifications never fire here — the four CGM event codes carry no session age, and no sensor-start event has been seen in a fetch window), transmitter battery, and a basal *profile* (the graph's basal overlay needs a repeating daily schedule, and a single fetch window is not one).

## For testers

Two things are unresolved, and a log from a debug build would settle both. Run a debug build, open the 24-hour history graph, then look in `trndi.log`:

1. **Which field marks a Control-IQ automatic correction.** The `Tandem.ExtractTreatments: bolus fields:` line lists `src=` (`bolusSource`), `type=` (`bolusType`) and `st=` (`completionStatus`) for every bolus. If you can say which of those boluses you gave yourself and which Control-IQ gave you, the mapping falls out. Until then every Tandem bolus is shown as manual — deliberately, since filing a real bolus as automatic would hide it behind a setting that is off by default.
2. **Whether the reservoir figure is right.** The same log block reports `reservoir=`. Compare it against what the pump display says at the time; the field sits next to one that is scaled by 100, so the scale is worth confirming before it is put on screen.

The `Tandem.EventCensus:` lines in the same log list every event code the payload contained with one sample of each. That is what the field names above were derived from, and a payload from a different pump or CGM would tell us whether they hold generally.

Redact before sharing: the samples contain your glucose, insulin and carbohydrate figures, plus account and device ids. Drop them in a GitHub issue or on [Discord](https://discord.gg/QXACfpcW).

# Personal Settings
## Setting limits
Personal limits and goals, such as "values over 10.5 are high", are not provided by the Tandem servers. 

Trndi will, by default, assume values are high over 8.9mmol/L (160mg/dL), and low under 3.3 (60mg/dL). You can however __set personal levels__ in the settings, under the __Customization__ tab.

### Setting personal levels
#### Personal range
In addition to "High over" and "Low under" alarms, Trndi can also use a "personal range." Originally a NightScout feature, it allows you to set a smaller target range within the high/low range. 

This can be summarized as "While 8.9 mmol/L is high, 3 is low, my personal target is 6.0-8.2". 

As a result, you get an earlier indication before your levels actually reach "High/Low". If you turn color in the "Time-in-rage" config tab on, this can be shown on your graph's background.

### Defining the personal levels
The personal levels can be set in the settings, under the Time-in-range tab; "Override custom range".