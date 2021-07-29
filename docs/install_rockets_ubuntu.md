---
layout: default
title: Rockets 2.0 Documentation - Installing Rockets on Ubuntu Linux
---

# Installing Rockets on Ubuntu Linux

## Step 1

First, you need an installation of Ubuntu 20.04 LTS. You can download an .ISO (a file that simulates a DVD-ROM install disc) here:

https://ubuntu.com/download/server

1. Click on "Option 2 - Manual Server Installation"
2. Click on the big green "Download Ubuntu Server 20.04.2 LTS" button

Save the .ISO file somewhere on your computer, such as your "Downloads" directory.

## Step 2

Second, you need to install Ubuntu Linux somewhere.

This can be:

1. On a spare PC you have lying around
2. On a Virtual PC that you run in an emulator on your main computer
3. On a cloud PC you have running on a cloud service provider like Amazon AWS or Linode

### For option 1, you need to:

* Burn the .ISO to a DVD-ROM using a burning program like Nero

OR

* Install it on a USB Thumb Drive by double-clicking the .ISO and dragging the contents to your thumb drive in Windows Explorer

Then reboot your spare PC and hit Delete (or sometimes another key, like F8) in the boot-up process to enter the BIOS menu, enter the "Boot" menu, and select "Boot from DVD" or "Boot from USB"

### For option 2, you need to:

1. Download VirtualBox here: https://www.virtualbox.org/wiki/Downloads
2. Install and launch VirtualBox
3. Click the "New" button
4. Use the following settings:
  * Name: Give your virtual PC a name
  * Type: Linux
  * Version: Ubuntu (64-bit)
5. Click "Next" and make sure the memory is set to 1024 MB of RAM, then click Next again
6. Select "Create a virtual hard disk now" and click Next
7. Select "VDI (Virtual Disk Image)" and click Next
8. Select "Dynamically allocated" and click Next
9. Choose 10 GB as the default size, or make it larger if you like (the OS and Rockets will take up about 6 GB)
10. Click "Create"
11. When your new Virtual PC is created, select it on the left-hand panel, and then click the "Settings" button
12. Click the "Storage" tab on the left hand side of the Settings panel
13. Click the icon of the DVD-ROM disk on the right-hand side at the top of the Storage panel, and select "Choose a disk file" from the dropdown menu. Select the Ubuntu 20.04 LTS ISO that you downloaded earlier
14. Click the "Networking" tab on the left-hand side of the Settings panel
15. Make sure "Enable Network Adapter" is checked, and select "Bridged Adapter" from the "Attached to:" dropdown menu. **This last step is important! You won't be able to browse your Rockets website from your PC unless you select this option.**
16. Click OK to save all settings
17. Click the "Run" button to start your Virtual PC
18. THe PC will book from the Ubuntu DVD ISO and start installing Ubuntu. Choose all standard settings (basically hit Enter a bunch of times until it is installed, but you will also choose a default user name and password here)
19. Log into your new Virtual PC with the user account name and password that you chose in the last step
20. Linux is now installed. You can access it from the VirtualBox console.

### For option 3, you need to:

1. Go to your the web site for your cloud service provider (eg Amazon AWS or Linode) 
2. Follow the instructions for making a new Virtual PC (on Amazon this is called an EC2 Instance, on Linode it is called a Node)
3. Install Linux on your new cloud Virtual PC

## Step 3

Now you're ready to install Rockets on your Linux PC. 

Don't worry about any other software you need to install (like Apache, etc) because the Rockets install script will add all necessary software automatically.

1. Log into your Linux PC (usually just entering your user name and password)
2. From the command line, download the install script by typing:

`wget http://newlisponrockets.com/downloads/install_rockets.sh`

This downloads the installation script for Rockets into your "home" directory.

3. Make the script executable by typing:

`sudo chmod +x install_rockets.sh`

This makes sure that you can run your script.

4. Run the script:

`sudo ./install_rockets.sh`

This will take some time to run (five to ten minutes, depending on the speed of your Linux PC). 

When it is finished, the script will clear the screen and ask you some questions about how you would like to customize your Rockets installation, eg:

* Your site's name (long and short)
* Your Rockets admin user name and password (different from the Linux PC user name, this is what you will use to log into your Rockets website)
* Your database name (keep it to one word)
* Your email address to use for your Rockets' site

If everything goes well, Rockets will be installed!

## Step 4

To view your site, you need to know the IP address of the Linux PC, so you can point your web browser to it.

From the Linux PC command line, type:

`ifconfig`

The IP address is usually the first one in the long list that is displayed. A typical IP address for a Virtual PC will be something like "10.40.1.3"

If your Virtual PC is in a cloud, it can have both a private and public IP address. Consult your cloud service provider docs to learn more. A private IP is something like "10.40.1.3" and is only visible to you. A public IP is visible to anyone on the Internet. You may have to specifically enable access to port 80 to make your public IP visible. 

## Step 5

Open a web browser tab (on your main PC and enter in the address you got from the last step, like this:

`10.40.1.3`

Hit Enter and you should see your new Rockets website!
