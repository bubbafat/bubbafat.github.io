---
title: 'Setting up OctoPi with Your Printer (I am using an Anycubic Neo)'
subtitle: 'What you will need and how to do it'
description: 'A brief, but relatively complete, guide for buying what you will need, installing and configuring software, and getting your first successful print on your Anycubic Kobra Neo with OctoPi on a Raspberry Pi.'
date: 2024-04-29
featured_image: '/images/post/octopi/octopi-device.webp'
categories: 
  - "3DPrinting#Anycubic"
published: true
---

{% include toc.html %}

{% include tip.html content="This entire setup process will work for many 3D printers however I am writing this in the context of an Anycubic Kobra Neo (First Gen). The main thing you need to know is that the printer setup, including print area dimensions, may differ for your printer. Additionally, your printer might have a different USB connection or capabilities. The raspberry pi and OctoPi installation will be the same for any printer" %}

<h2>Why You Want This</h2>

Printing via the Anycubic Neo is a little bit of a pain. Sure, you can connect a USB cable to a nearby PC and use Cura, but this means you need to have your printer near your computer and it makes it quite a bit more difficult to monitor or send jobs remotely.

OctoPi solves these problems by having a service that can run on a USB-powered Raspberry Pi device (the size of a deck of cards) that you can install right with the printer and now manage the printer remotely with a suite of plugins that don't just make management easier but can actually improve the quality of your prints.

<h2>How Hard is This?</h2>

This is probably 3/10 on the difficulty scale. All you need is the hardware (described below), a computer (to configure the pi and run your slicer - Cura in this document), and about an hour of time.

<h2>What You Need to Buy (if you don't have it)</h2>

There are a lot of options but if you want a specific recomendations, this is what I am using:

- [Raspberry Pi 4 Model B (2GB)](https://www.adafruit.com/product/4292)
- [USB-c Power Supply (5v)](https://www.adafruit.com/product/4298) (you cannot power the pi off the Neo's USB port as that is needed for the data connection)
- [USB Type A to Type C cable](https://www.adafruit.com/product/4474). This cable must support data transfer and not just charging as this will connect the pi to the printer.
- [32GB microSDHC Card (DOS format)](https://www.amazon.com/PNY-Elite-microSDHC-Memory-P-SDU32GU185GW-GE/dp/B07R8GVGN9/)
- MicroSD Reader - [USB-A adaptor](https://www.adafruit.com/product/939), [USB-C adaptor](https://www.adafruit.com/product/5212). The Neo came with one so you might not need this.
- [(Optional) USB-A Web Camera](https://community.octoprint.org/t/usb-webcams-known-to-work-with-mjpg-streamer/21149). If you have a spare USB web cam, there is a good chance it will already work with Raspberry PI. I'm using a Logi 925e Business Webcam.
- [(Optional) Micro-HDMI to HDMI cable](https://www.adafruit.com/product/1322). Not needed for setup, but a nice-to-have if you ever need to connect a monitor.

<h2>Installing OctoPi</h2>

The steps to install OctoPi are:

1. Insert the microSD card into the adpator and plug into your computer.
2. If the card is not already FAT32 formatted, use the Windows drive format tool to format as FAT32.
  ![windows format tool screenshot](/images/post/octopi/microsd-format-screenshot.webp)
3. Download the [Raspberry Pi Imager](https://www.raspberrypi.com/software/) for your platform (I'm on Windows).
4. Install the imager application.
5. Run the imager application
  ![pi imager tool home screen screenshot](/images/post/octopi/pi-imager-home-screen.webp)
6. Click "CHOOSE OS"
7. Scroll down to "other specific-purpose OS"
8. Choose "3D printing"
9. Choose "OctoPi"
10. Choose "OptoPi (stable)" (this should bring you back to the main screen)
11. Click "CHOOSE STORAGE"
12. Choose your USB device that has the micro SD card (make sure you get the right device - this WILL overwrite your drive)
13. Click "NEXT"
14. On the "Use OS customization" window, click "EDIT SETTINGS"
  ![use os customization screen screenshot](/images/post/octopi/pi-imager-customize.webp)
15. Set the hostname (this will be the name of the raspberry pi on your network) - you need to remember this!
16. Set the username to "pi" and set a password
17. Configure the wireless LAN with your wifi details
18. Optionally, set the locale settings
  ![customizaing screen screenshot](/images/post/octopi/pi-imager-settings-1.webp)
19. Switch to the "SERVICES" tab
20. Click "Enable SSH" and select "Use password authentication"
  ![enabling ssh screenshot](/images/post/octopi/pi-imager-enable-ssh.webp)
21. Click "SAVE" (this returns you to the "USe OS customization? window)
22. Click "YES"
23. On the Warning dialog, if you are 100% positive you have selected the right driver, click "YES"
  ![are you sure screenshot](/images/post/octopi/pi-imager-are-you-sure.webp)

At this point the micro SD is being prepared, written to, and verified. This will take a about 5 minutes.

![sd card writing and verifying](/images/post/octopi/pi-imager-sdcard-writing.webp)

<h2>Booting OctoPI</h2>

1. Ensure there is not an SD card in the Neo - remove it if there is.
2. Insert the 32GB microSD card into the SD card slot on the bottom of the Raspberry PI board.
3. Insert the USB-A to USB-C cable into a black USB-A port on the Pi.
4. Insert the USB-C end of that cable into the Neo
5. If you have a camera, plug it into a blue USB-A port on the Pi
6. Connect the USB-C power supply to the pi - It will take a few minutes for your pi to boot. Be patient!

{% include tip.html content="It can take several minutes for your raspberry pi to boot. Be patient!" %}

![octopi physically connected to printer](/images/post/octopi/octopi-device.webp)

<h2>Visiting OctoPi</h2>

You've waited a few minutes and can now visit your OctoPi service on your network. While on the same wifi network you configured the device to use, visit the machine name on your network. Since I named mine "neo" the URL I will visit is http://neo.local

{% include tip.html content="Notice that I used &ldquo;http&rdquo; and not &ldquo;https&rdquo;. If you want to use https, you need to <a href='https://www.pico.net/kb/how-do-you-get-chrome-to-accept-a-self-signed-certificate/'>add the self-signed cert to your trusted root store</a>" %}

<h2>Setting up OctoPi</h2>

You should now be looking at the OctoPi setup wizard.

![octopi setup wizard screenshot](/images/post/octopi/octopi-setup-wizard-start.webp)

- Click "Next"
- If you are reading this, you've probably never setup OctoPi before so you don't need to restore a previous config. Click "Next"
- On the "Access Control" page you will need to setup an OctoPi admin. This is not the same account you created when creating the image. This is the account you can log into the website OctoPi with. Create a username and password and make sure to remember them!
- Click "Create Account" - when it completes you will get a popup notice letting you know the account is created and you are now logged in as it. 
- Click "Next"
- Since you're connected via wifi, things are probably working but I would suggest clicking on "Test host & port" and "Test name resolution" - both should succeed.
- Click "Enable Connectivity Check" (or don't - but I suggest it)
- Click Next
- Configuring anonymous usage tracking is totally up to you. Click an option and then click "Next"
- On the "Configure Plugin Blacklist Processing" page, click "Enable Plugin Blacklist Processing" and then click "Next" (or don't enable it - but I did).
- If you connected a webcam during the hardware setup, at the Webcam and Timelapse Recording page, click the two "Test" buttons to make sure you can see an image from your camera.
- Click "Next"
- On the "Set up your printer profile" page, on the General tabe give your printer a name and model. These are free-form strings. I entered "Neo" for the name and "Anycubic Kobra" for the model.
- Click "print bed & build volume"
- Make sure the following are set:
  - Form Factor: Rectangular
  - Origin: Lower Left
  - Heated Bed: Enabled
  - Heated Chamber: Disabled
  - Width (X): <b>220</b>
  - Depth (Y): <b>220</b>
  - Height (Z): <b>250</b>
  - Custom Bounding box: Disabled
- Click "Axes"
 - All the defaults should be fine (6000, 6000, 200, 
 300)
- Click "Hotend & extruder"
 - Nozzle Diameter: 0.4 (unless you have changed your nozzle)
 - Number of Extruders: 1
 - Default extrusion length: 5
- Click "Next"
- On the "All Done!" page, read and agree to the three points.
- Click "Next" and if prompted, reload.

<h2>No printer connection?</h2>

If when you get to OctoPi's main UI, you see this message:

{% include tip.html content="No serial port found, are you sure your printer is physically connected and supported? Try freshing and if that doesn't help please see the FAQ" %}

First check that your USB-A to USB-C cable is connected from the pi to the printer and that the printer is on.

Next, try another cable. You are probably using a charge-only cable and not a data-capable one.

![error due to serial port error](/images/post/octopi/octopi-no-serial-port.webp)

<h2>Installing Plugins</h2>

You can [install OctoPi plugins](https://plugins.octoprint.org/help/installation/) which can not only improve usability of OctoPi, but can actually improve the quality of your prints.

I would suggest the following plugins to get started:

- Themeify
- Simple Emergency Stop
- Resource Monitor
- OctoEverywhere!
- OctoPod

<h2>Connecting Your Phone</h2>

If you have an iPhone, once you've installed OctoEverywhere and OctoPod you can then install the OctoPod app on your [iPhone](https://apps.apple.com/us/app/octopod-for-octoprint/id1412557625). On an Android you could use [OctoEverywhere](https://play.google.com/store/apps/details?id=com.qlabs.octoeverywhere.twa&hl=en_US&gl=US&pli=1).

This will allow you to view the web camera remotely (without opening a port on your router), manage your print jobs remotely, and control your printer without having to use the front control panel for many operations.

There are a bunch of possibly tools you could use - experiment with them and see what you like!

<h2>Configuring Cura</h2>

To get Cura to see OctoPi you need to install the [OctoPrint Connection](https://marketplace.ultimaker.com/app/cura/plugins/fieldofview/OctoPrintPlugin) plug-in in Cura. Once you have done that, visit your printer setup page and click "Connect OctoPrint" and setup an API key.

This is documented on this [All3DP blog post](https://all3dp.com/2/cura-octoprint-plugin-connection/).

<h2>Your First Print</h2>

With all of this done - you are ready to use Cura to send a print directly to OctoPrint!