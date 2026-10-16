# Set up Dexcom

## Preparations
### Create a user
> NOTE: You don't need to create a user if you're already using Follow!

Firstly, you need to create a __Follow user__, the name and password doesn't matter. Creating a user just enables the data-sharing.
### Enable Follow
Secondly, you need to enable Follow in your Dexcom app.

## Setup Trndi
Open Trndi and open the settings (if they aren't alredy showing), by right-clicking and choosing settings (long-press for touch screens).

### Configuring the settings dialog
1. Use the drop-down and choose Dexcom (with your region)
2. Enter your __Dexcom username__ or __e-mail__
> This is __not__ the Dexcom Share username - it's the account you log-in to _Clarity_ and _the app_ with
3. Enter your __Clarity__/__app__ password

#### Display units
4. Choose __mmol/L__ in the bottom, if you live in a country (such as Sweden) where it's used, or __mg/dL__ in countries such as the USA where that is used

Now close the window, then close Trndi (if it doesn't close itself) and start it again.

## Troubleshooting
Most Dexcom problems come from the account itself rather than from Trndi. Check these before anything else:

* **"Incorrect username or password" when the password works in the Dexcom app.** The wrong region gives exactly this error. Dexcom answers the same way for a wrong password and for an account that lives on another region's server. The "Test connection" button tries the other regions for you after this error and names the one that accepts your account.
* **Long passwords.** Dexcom's sharing servers accept passwords of at most 20 characters, even though the app and website let you set longer ones. Change the password to 20 characters or fewer if it keeps being rejected.
* **Use the sharer's account, not a follower's.** Trndi needs the account of the person wearing the sensor, the one used to log in to the Dexcom app and Clarity. A Follow account cannot read the data.
* **Children's accounts.** A child's readings are fetched with the child's own username and password, not the parent's.
* **Sharing must be on, with at least one follower.** If sharing is off, or no follower has been invited, the servers return no readings at all. Trndi then shows no data rather than an error.
* **No readings but no error.** The Dexcom app on the phone is not uploading. Toggle sharing off and on in the app, or restart it.
* **Phone number as username.** Write it with the country code, as in `+46701234567`, without spaces.
* **Too many attempts.** After repeated failed sign-ins Dexcom locks the account for a while. Stop every app that uses the account, wait 10-15 minutes, and try again with the right details. Trying sooner keeps it locked.
* **Dexcom Stelo** does not use Dexcom Share and cannot be read by Trndi.

# Personal Settings
## Setting limits
Personal limits and goals, such as "values over 10.5 are high", are not provided by the Dexcom servers. 

Trndi will, by default, assume values are high over 8.9mmol/L (160mg/dL), and low under 3.3 (60mg/dL). You can however __set personal levels__ in the settings, under the __Customization__ tab.

### Setting personal levels
#### Personal range
In addition to "High over" and "Low under" alarms, Trndi can also use a "personal range." Originally a NightScout feature, it allows you to set a smaller target range within the high/low range. 

This can be summarized as "While 8.9 mmol/L is high, 3 is low, my personal target is 6.0-8.2". 

As a result, you get an earlier indication before your levels actually reach "High/Low". If you turn color in the "Time-in-rage" config tab on, this can be shown on your graph's background.

### Defining the personal levels
The personal levels can be set in the settings, under the Time-in-range tab; "Override custom range".