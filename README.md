#rott-rtl
These are utilities to read RTL/RTC maps from the old DOS game _Rise of the
Triad_ (ROTT), and also utilities to read in some graphics from the old TED5
tile editor.

I'm really not that into games, in general, and I'm also not really hardcore
about first person shooters, but after all these years ROTT still remains my #1
favorite FPS of all time.

##Requirements
To run these you'll need [Racket](https://racket-lang.org/). You'll also need
some ROTT maps, as none are currently provided here. However, the original
ROTT tiles from TED5 are provided.

##File Descriptions

###rtl.rkt
Functions to read map data from RTL/RTC files. Currently there is no
functionality to write maps.

###ted5-gfx.rkt
Functions to read in tiles from the EGADICT, EGAGRAPH, EGAHEAD, and GFXINFOE
files from TED5. Currently only handles 16x16 EGA tiles. Tested with the ROTT
sample maps distributed with TED5 and with the freeware release of _Bio
Menace_.

###tester-ted5-gfx-gui.rkt
A simple GUI program to display individual TED5 tiles.

###rtl-tester.rkt
Some simple examples for reading in ROTT maps.

###rottview.rkt
![rottview screenshot](https://raw.githubusercontent.com/lz444/screenshots/master/rottview.png)

This is a simple GUI program to view ROTT maps. This is only a viewer; there
are no editing capabilities, although a lot of the interface was modeled after
the TED5 editor. It is still useful to look at existing maps in case you want
to see how a certain level is designed, or you want some help playing the game.
Requires graphics files from TED5 which are provided in the graphics
subdirectory.

##Licence
I wrote all the *.rkt files, and they are in the public domain. I did not
make the TED5 tiles provided, so they are not public domain. They are provided
for convience though. Realistically anyway, I don't think anyone's going to
care about some 16 color EGA tiles, intended for use with a game that was
already considered outdated when it was new.
