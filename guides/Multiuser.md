
# Setting up an additional user

## Adding a user
* Start Trndi as usual
* Right-click and open settings
* Open the __Accounts__ page (under _App & system_ in the sidebar)
* Click __+ Add__ and enter a name
* Close the window and __save settings__
* __A new user has been created__
![User select](/doc/img/user_config.png)

## Logging in as a user
When users exist, Trndi will ask which one you want to use on start-up:

![User select](/doc/img/user_select.png)

Every row shows the account's colour dot, its nickname and, underneath, the
account name you typed when creating it. The default account is the first row.
The account you used last time is already highlighted when the dialog opens, so
returning to the same account is a single Enter or double-click; typing the
first letters of a nickname jumps to it. Closing the dialog or pressing
__Default__ starts the default account. Each account's language setting is
applied once you have chosen — accounts can run Trndi in different languages.

To skip the dialog and always start the default account — for a kiosk, a
scripted launch or an autostart entry — add the `--no-multi` flag:
```bash
trndi --no-multi
```
On macOS: `open -a Trndi --args --no-multi`. The flag does not change which
account the dialog highlights next time you start without it.

## Setting up the new user's data source
Adding a user only creates an empty account: it has no server, thresholds or other settings of its own yet. To fill them in:
* Restart Trndi and pick the new user in the start-up dialog
* Right-click and open settings
* Configure the backend (Nightscout, Dexcom, etc.) and anything else, just as for a single-user Trndi
* Close the window and __save settings__

Repeat for every user you added. Each user's settings are stored separately, so changing one never affects another. This is also what makes a user appear in [trndi-multi](https://github.com/slicke/trndi-multi): users without a backend are skipped there.

## Customizing a user
* In the user list, click the user's username
* In the __Account Settings__ panel next to the list, choose an account color and nickname
> Edits are kept while you browse the user list, but nothing is stored until you close the settings window and __save settings__ — closing without saving discards them, just like every other setting.

The color chosen will be assigned to the window when Trndi loads the user.
The username is also displayed in the title bar.
On Windows and macOS the nickname appears as a coloured badge in the top-right of the title bar (using the account colour); click it to open Settings. The titlebar also changes colour. Linux gets the same badge whenever Trndi draws its own title bar (Wayland, or `ux.own_titlebar=on`) — without an account colour the badge tints itself from the title bar so it stays readable as the bar follows your glucose colour. Where none of those applies (X11, or a fullscreen window, which has no title bar to carry the badge) the name prefixes the window title as `[name] Trndi` and a sidebar appears.

## Standard user
There is always a standard user, you can assign it a nickname and color aswell. The standard user is the information you've saved before adding additional users. It cannot be removed.

## Removing a user
* Select the user in the list and click Remove, and confirm.
* Trndi then asks whether to also __erase the user's stored settings__ (server, color, nickname etc.). If you keep them, the account can be restored later.
* Close settings and apply changes — removals (and erasures) only take effect when you save.

## Retrieving a user
If you add a username again, which has been removed before (without erasing its settings), the settings will still be there. Thus, removing a user takes them out of the list, but only deletes their data if you asked for that when removing!

## Reverting to single user
* Remove all users, close settings and apply
* The standard user is now the only user available, and you will not be asked to choose an account at start up