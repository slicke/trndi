
# Setting up an additional user

## Adding a user
* Start Trndi as usual
* Richt-click and open settings
* Click the "Multi User" tab
* Click Add
* Close the window and __save settings__
* __A new user has been created__
![User select](/doc/img/user_config.png)

## Logging in as a user
When users exist, Trndi will ask which one you want to use on start-up:

![User select](/doc/img/user_select.png)

The account you used last time is already selected when the dialog opens, so
returning to the same account is a single click. Each account's language
setting is applied once you have chosen — accounts can run Trndi in different
languages.

## Customizing a user
* In the user list, click the user's username
* Below choose an account color and nickname
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