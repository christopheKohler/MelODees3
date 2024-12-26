FloppyBridge Plugin for WinUAE
------------------------------

This plugin is multi-licensed under the terms of the Mozilla Public License Version 2.0 as published by Mozilla Corporation and 
the GNU General Public License, version 2 or later, as published by the Free Software Foundation.
* MPL2: https://www.mozilla.org/en-US/MPL/2.0/
* GPL2: https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

Source Code: https://github.com/RobSmithDev/FloppyDriveBridge

Installation: Copy the DLL files into the PLUGINS folder in the WinUAE installation folder. The PLUGINS folder may need creating!

Currently Supported Hardware: DrawBridge (aka Arduino Reader/Writer), Greaseweazle and Supercard Pro

More Information: For more information see https://amiga.robsmithdev.co.uk/winuae

Consider supporting me on Patreon to keep the development of this going at https://www.patreon.com/RobSmithDev


INSTALLATION - Expansions/Disk Controllers is *NOT( how its configured now.
------------
1. Find your WinUAE folder and find the WinUAE.EXE or WinUAE64.exe
2. Locate or Create a folder called 'plugins'
3. Copy the DLL files from this zip file to the plugins folder.
4. Configuration of "FloppyBridge" is now done from the "HADRWARE/FLOPPY DRIVES" section
5. Choose the drive type drop down (usually says [3.5" DD]) and choose "Configure FloppyBridge"
6. For more information see the video at https://youtu.be/2E-6gG1Rwsw



Release Notes:
--------------
1.4:       Fixed issues with stalling mode not working properly, or not as expected
           Updated stalling mode to work better with diskspare device which seems to be very impatient! (writing is not recommended!)
           Updated the icons for the supported hardware and rendered them transparently
1.3        Fixed Greaseweazle Drive A/B not selectable properly
           Fixed issue with warning message about diskchange on Greaseweazle always re-appearing
	   Fixed Greaseweazle Shugart support. (**Does not support disk change** - it will manually check for disks. A PC Drive is strongly recommended)
           Re-worked the profile listing dialog a little 
1.2        Added no-click disk check when it has to be done manually
           Added support for tracks 82 and 83 if supported by the drive (firmware update needed for DrawBridge)
           Updated Greaseweazle driver to also support Shugart interfaces
	   Some cosmetic fixes
1.1        Greatly increased compatability due to changes in the PLL and Rotation Extractor handling.  Check the games that didn't boot before!
	   (DrawBridge Firmware 1.9.24 required!)
1.0        Added option to toggle auto-update check.  First full release
0.12 Beta: Fixed random crash in Rotation Extractor
           Further stability improvements for Linux (or rather not-Windows)
	   Lots of minor changes, improvements and testing with the help of Dimitris Panokostas aka MiDWaN (Amiberry)
0.11 Beta: Added support for Supercard Pro (V1.3 firmware required)
           Changed read mode to use a more accurate PLL (taken from SCP design from SCP.cpp in *UAE) which **improves compatability with some games**
	   Added support for DrawBridge new PLL read command for better accuracy (Firmware 1.9.23) - HIGHLY recommended
	   Fixed a few bugs in rotation extractor that caused a few crashes
	   In "Normal" mode, if a perfect revolution cannot be guessed it automatically switches to compatible for that track  
           Fixed 64-Bit support for Greaseweazle under some linux distros
0.10 Beta: Faster DrawBridge startup (FTDI option recommended) and Faster Auto-Detection too
          Fixed a lockup bug if driver failed to start but was still used
0.9 Beta: Fixed bug in the 64-bit version where it would lock up getting a list of FTDI com ports.
          DrawBridge now starts up a little faster
          Fixed a few cosmetic issues in the dialogs
0.8 Beta: Added support for Auto-Cache - attempts to load the entire disk into memory while the emulator isn't using it
          Added support for SmartSpeed - If using Normal or Compatiable mode, this looks at the data and switches to turbo automatically if it thinks its safe to do so (may break some copy protections)
          Fixed an issue with the URL shown when you change the driver
          Adjusted some small changes in bit-sequences
          Re-adjusted some seek timings for GW
          Updated COM port detection code
          Hopefully a fix with intermittent write failures for HD disks.
0.7 Beta: Fixed disk detetction on startup for GW
          Fixed 30-second lockup if you use GW and have the wrong drive cable selected (A vs B)
          Links now have the hand-cursor and are highlighted in 'link' colour
0.6 Beta: Fixed a small number of GW issues
0.5 Beta: Faster seeking for GW, bug fix in update check, and improvements for DrawBridge
0.4 Beta: Updated GUI so when you create a new profile it uses the driver from the current selection
          Updated the DrawBridge Icon
0.3 Beta  Created profile system to make managing the setup much easier
0.2 Beta  Fixed some silly errors with the original version including variable names being reversed
0.1 Beta  First version.