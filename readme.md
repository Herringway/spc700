# snes_spc 0.9.0: SNES SPC-700 APU Emulator
This library includes a full SPC emulator and an S-DSP emulator that can be used on its own. Two S-DSP emulators are available: a highly accurate one for use in a SNES emulator, and a 3x faster one for use in an SPC music player or a resource-limited SNES emulator.

## Features
* Can be used from C and D code
* Full SPC-700 APU emulator with cycle accuracy in most cases
* Loads, plays, and saves SPC music files
* Can save and load exact full emulator state
* DSP voice muting, surround sound disable, and song tempo adjustment
* Uses 7% CPU average on 400 MHz Mac to play an SPC using fast DSP

The accurate DSP emulator is based on past research by others and hundreds of hours of recent research. It passes over a hundred strenuous timing and behavior validation tests that were also run on the SNES. It's the first DSP emulator with cycle accuracy, properly emulating every DSP register and memory access at the exact SPC cycle it occurs at, whereas previous DSP emulators emulated these only to the nearest sample (which occurs every 32 clocks).

Author : Shay Green <gblargg@gmail.com>, Cameron R. <elpenguino@gmail.com>

License: GNU Lesser General Public License (LGPL)


## Getting Started
Build the 'player' subpackage with `dub build :player` in the repository root.

Read snes_spc.txt for more information.

----
Shay Green <gblargg@gmail.com>

Cameron R <elpenguino@gmail.com>
