# Set up Dexcom

## Preparations
> NOTE: You don't need to create a user if you're already using Follow!
### Create a user
Firstly, you need to create a __Follow user__, the name and password doesn't matter. Creating a user enables the data-sharing.
### Enable Follow
Secondly, you need to enable Follow in your Dexcom app

## Setup Trndi
Open Trndi and click the _SETUP_ text, or right-click and choose settings (long-press for touch screen):
1. Use the drop-down and choose Dexcom
2. Enter your __Dexcom username__, this is _not_ the share user - it's the account you log-in to Clarity/the app with
3. Enter your Clarity/app password
4. Choose mmol/L in the bottom, if you live in a country (such as Sweden) where it's used
> NOTE: No server supports mmol/L, its handled by the client-side. Thus, if a reading is 5.555554 the Dexcom mobile app might show 5.5 while Trndi shows 5.6. This is very seldom, but if there's a 0.1 difference this is why.

Now close the window, then close Trndi and start it again.


# Setting limits
Personal limits and goals, such as "values over 10.5 are high", is not provided by the Dexcom servers. 

Trndi will, by default, assume values are high over 8.9mmol/L (160mg/dL) and low under 3.3 (60mg/dL). You can however __set personal levels.

## Setting personal levels
### Personal range
In addition to "High over" and "Low under" alarms, Trndi can also use a "personal range." Originally a NightScout feature, it allows you to set a smaller target range within the normal limits. As a result, you get an earlier notice before your levels actually reach "High/Low".

### Defining the levels
To set these levels, you need to create a simple plugin as explained in the [Extension guide](/doc/Extensions.md), search for Dexcom.
> NOTE You can also use [the script for Windows](https://raw.githubusercontent.com/slicke/trndi/refs/heads/develop/doc/setlimit.ps1) to create a file automatically. For Linux, you will need to  [the Linux script](https://raw.githubusercontent.com/slicke/trndi/refs/heads/develop/doc/setlimit.sh).