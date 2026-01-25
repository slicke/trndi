![RPi Display](/doc/img/img_rpi.png)
# Creating an always-on display with a RaspberryPi and Trndi

## Disclaimer
There's no guarantee that this setup will work for you, or that it will continue to work with future updates to Linux or similar operating systems. I take no responsibility for any purchases or commitments you make based on this guide.

## Hardware
1. Aquire a RaspberryPi (tested on RPi4 Model B)
2. Aquire a power supply (eg by getting a Rpi kit with a charger included)
3. Aquire a screen
   * An old screen
   * A touch screen from a retailer, 7" is a good size

## Setup
1. Install a Linux distro, this guide uses Raspbian
2. Connect via [TigerVNC](https://tigervnc.org) and setup the system to your liking
3. Make sure Qt is installed, in a terminal run:
```bash
sudo apt update
sudo apt install -y \
  libqt6core6 libqt6gui6 libqt6widgets6 libqt6network6 \
  libcurl4 ca-certificates \
  libxcb1 libx11-6 libxext6 libxrender1 libxrandr2 \
  libfontconfig1 libfreetype6 libdbus-1-3
  ```
4. Install the Qt6Pas library with ```apt install libqt6pas6```
  ## Install Trndi

  1. In this repo, visit the [latest release](https://github.com/slicke/trndi/releases/latest), and down load the _arm64_ package: ```trndi_X.Y.Z_arm64.deb```
  2. Install the package via GUI, or via ```dpkg -i <package name>```

  # Running Trndi
  1. Start Trndi from the menu; ```Top-left Menu > Accessories > Trndi```
  2. Setup as you'd do on your desktop
  3. Once up, right-click Trndi and choose ```Full screen``` 

  ## Done!
  You now have an always-on display

# My setup
> This setup is overly advanced and is just provided as an example of what you can do
  ## Hardware
  I use a RaspberyPi 4B with 4GB RAM, and a HDMI/USB touch screen
  ## Setting up the SD card
  1. Download/Install the [Raspberry Pi Imager](https://www.raspberrypi.com/software/) version 2 or later
  2. Insert your (16GB or larger) SD card
  3. install __Raspberry Pi OS _Lite_ (64bit)__ (found under "Raspberry Pi (other)").
  4. Setup Wifi and Enable SSH in the guide
  5. Write the image
  6. Let it verify, and remove the SD card when asked to
  ## Setting up the Raspberry
  1. Boot the new SD card and make note of the IP address (The display will say "My IP address is...")
  2. SSH into the IP address and update apt: ```apt update```
  3. Install KDE and dependancies:
     1. ```sudo apt install kde-standard libqt6pas6``` to install KDE and Trndi dependancies
     2. ```sudo reboot```, and reconnect when the pi is online again
     3. ```sudo install lightdm``` to install a login manager. Chose _lightdm_ in the popup when asked
  4. Set these in ```sudo raspi-config```:
     1. System options > Boot: Set GUI
     2. Advanced Options > Wayland: Set Labwc
     3. Interface Options > VNC: Enable VNC (allow the APT install that triggers)
  5. Reboot the system: ```sudo reboot```
     - NO GUI? See this note: [LightDM Bug](#lightdm-bug)
  6. Log in using your keyboard/mouse - or connect via VNC.

  ## Install Trndi
  > You can use your browser and the GUI for this too!
  ### SSH install
  1. Go to [Latest Downloads](https://github.com/slicke/trndi/releases/latest)
  2. Get the URL to the ARM release (eg  ```trndi_4.2.200_arm64.deb```)
  3. Download it: ```wget <URL>```
  4. Install it ```sudo dpkg -i <filename>```
  ## Run Trndi
  In KDE:s menu, navigate to Utilities and find Trndi.

  ## Extra Notes

  ### Power saving
  You should disable power saving so that the Pi won't enter sleep mode!
  ### LightDM Bug
  There's a bug in the current Raspberry Pi OS that makes lightdm fail. To fix it, edit ```/etc/lightdm/lightdm.conf```, locate the line:
```
greeter-session=pi-greeter-labwc
``` 
and change it to
```
greeter-session=lightdm-greeter
```
You can also use SSDM, but that will break VNC support!