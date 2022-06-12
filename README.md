*****************************************************************************

TEOS
====

  **A replacement Operating System for KRIKzz's Turbo Everdrive v2**

  Copyright John Brandwood 2019-2022.

  Distributed under the Boost Software License, Version 1.0.

  (See the accompanying file LICENSE_1_0.txt or the copy at
        http://www.boost.org/LICENSE_1_0.txt)


*****************************************************************************

About
-----

This is a replacement Operating System for KRIKzz's Turbo Everdrive v2.

Please do NOT try to use this on an old Turbo Everdrive v1, IT WILL NOT WORK!


*****************************************************************************

TEOS Release v3.01 (2022/06/12)
-------------------------------

Just extract the contents of the .ZIP file, and copy the extracted files to the root of your Turbo EverDrive's SD card.

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

Building a new TEOS from Source Code
------------------------------------

The source code for TEOS can be found on GITHUB at https://github.com/jbrandwood/teos

You will need to use a recent version of the PCEAS assembler to successfully built TEOS.


The PCEAS assembler is one component of the HuC project which can be found on GITHUB at https://github.com/jbrandwood/huc


*****************************************************************************
