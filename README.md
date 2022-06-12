*****************************************************************************

TEOS
====

  **A replacement Operating System for Krikzz's Turbo Everdrive v2**

  Copyright John Brandwood 2019-2022.

  Distributed under the Boost Software License, Version 1.0.

  (See the accompanying file LICENSE_1_0.txt or the copy at
        http://www.boost.org/LICENSE_1_0.txt)


*****************************************************************************

About
-----

This is a replacement Operating System for Krikzz's Turbo Everdrive v2.

Please do NOT try to use this on an old Turbo Everdrive v1, IT WILL NOT WORK!


While TEOS is graphically nicer than the original Turbo Everdrive OS, the real reasons for using it are ...

* It allows the TurboGrafx (and PC Engine) to use the Turbo Everdrive v2 as a Super System Card, avoiding the need to buy the very-expensive US TurboGrafx Super System Card.
* It avoids the potentially damaging memory bus-fighting that occurs with Krikzz's OS when running the PC Engine (or TurboGrafx) Super System Card HuCARD on the Turbo Everdrive.
* It supports copying the PC Engine's "Backup RAM" contents to and from the Turbo Everdrive's SD card.
* It supports copying the PC Engine's "Memory Base 128" contents to and from the Turbo Everdrive's SD card.
* It fixes the graphics corruption in the Japanese "POPULOUS" HuCARD game.
* It supports the use of the Japanese "Tennokoe Bank" HuCARD utility.


*****************************************************************************

TEOS Release v3.01 (2022/06/12)
-------------------------------

Just extract the contents of the release "TEOS-3.01-2022-06-12.ZIP" file, and then copy the extracted files to the root of your Turbo EverDrive's SD card.

In particular ... the contents of the /TBED/ directory *must* go in that directory.

If you have already got some files with the same name in your /TBED/ directory, then please do NOT overwrite those files, or you will lose your existing BRAM saves!


Note that TEOS works both as a HuCARD image, and as a replacement for Krikzz's original Turbo Everdrive OS.

* To run it as a HuCARD, just select and run TEOS.PCE from your existing Turbo Everdrive OS.
* To replace Krikzz's original Turbo Everdrive OS with TEOS, just rename TEOS.PCE to OS.PCE and copy the OS.PCE file to the /TBED/ folder on your SD card.


For Turbo GT (and Turbo Express) owners there is also a version of TEOS called TEOS-GT.PCE which avoids using the high-resolution text mode, and so is a bit more readable on their LCD screen.


If you find any problems, then it would be better to report them in the "Homebrew Development" section of the "PC Engine Software Bible forum" (https://pcengine.proboards.com/board/5/homebrew-development), rather than cluttering up KRIKzz's official Turbo Everdrive forum.


*****************************************************************************

Changes since TEOS Beta 5 (2021/01/26)
--------------------------------------

* Support uploading HuCARD games to the TED2 through USB with either Krikzz's "turbo-usb2.exe", or Dave Shadoff's PCE_TurboEverdrive_USB (https://github.com/dshadoff/PCE_TurboEverdrive_USB).
* Backup and restore NEC's Memory Base 128 (a.k.a. Koei's Save Kun) images to and from the TED2's SD card.
* Enable the TED2's "Street Fighter 2" memory mapper for all HuCARDs larger than 1MByte, to allow for future homebrew games using the mapper.
* Fix missing color in "Order of the Gryffon" password screen.


*****************************************************************************

Changes for TEOS Beta 5 (2021/01/26)
------------------------------------

TEOS Beta 5 fixes initialization problems with the following games ...

* Tower of Druaga
* Space Harrier
* Shiryou Sensen
* Out Live
* Benkei Gaiden
* Gekisha Boy
* Night Creatures


*****************************************************************************

Building a new TEOS from Source Code
------------------------------------

The source code for TEOS can be found on GITHUB at https://github.com/jbrandwood/teos

You will need to use a recent version of the PCEAS assembler to successfully build TEOS.


The PCEAS assembler is one component of the HuC project which can be found on GITHUB at https://github.com/jbrandwood/huc


*****************************************************************************
