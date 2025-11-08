# Creating an always-on display with a RaspberryPi and Trndi

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
  ## Install Trndi

  1. In this repo, visit the [latest release](https://github.com/slicke/trndi/releases/latest), and down load the _arm64_ package: ```trndi_X.Y.Z_arm64.deb```
  2. Install the package via GUI, or via ```dpkg -i <package name>```

  # Running Trndi
  1. Start Trndi from the menu; ```Top-left Menu > Accessories > Trndi```
  2. Setup as you'd do on your desktop
  3. Once up, right-click Trndi and choose ```Full screen``` 

  ## Done!
  You now have an always-on display