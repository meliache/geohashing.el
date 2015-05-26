# geohashing library for emacs lisp
Work in progress. At the moment just an incomplete collection of
emacs functions for the game of geohashing.
Not yet recommended for use by others yet, it is probably buggy and mostly undocumented.
I don't have much time at the moment and will fix that later.

What is geohashing: http://wiki.xkcd.com/geohashing/
The algorithm: http://wiki.xkcd.com/geohashing/Algorithm

For user interaction, it has only two interactive functions at the moment,
which can be thought of as temporary and are on my TODO list.
The final version would ideally be able to select the date from the calendar
and have some clever way to select the coordinates or the graticule.
* * *

At the moment, it should be used like this:

Add the file *"geohashing.el"* somewhere into your emacs path and add

    (require 'geohashing)

to your emacs config, e.g. ".emacs". Also, set the calendar variables
*calendar-latitude* and *calendar-longitude*, e.g.

    (setq calendar-latitude 42.42)
    (setq calendar-longitude 31.41)

I use them to display the sunrise and sunset times
by typing <S> while being in the emacs calendar mode.

Then you can type for example

    M-x nearest-geohash-today
    M-x nearest-geohash-tomorrow

And you will get the position and distance of the nearest geohash
from the four graticules next nearest to your coordinates.
* * *

More things are possible when using it non-interactively:

    (geohash-coordinates '(graticule-latitude graticule-longitude)
                         '(year month day))

will return the geohash coordinates for this graticules on that particular date.

    (calc-nearest-geohash '(exact-latitude exact-longitude)
                          '(year month day))

will return a list of the latitude and longitude of the nearest geohash to your
exact coordinates.

    (calc-distance '(geohash-coordinates) '(exact-coordinates))

will return the distance between two coordinates.teractively:

    (geohash-coordinates '(graticule-latitude graticule-longitude)
                         '(year month day))

will return the geohash coordinates for this graticules on that particular date.

    (calc-nearest-geohash '(exact-latitude exact-longitude)
                          '(year month day))

will return a list of the latitude and longitude of the nearest geohash to your
exact coordinates.

    (calc-distance '(geohash-coordinates) '(exact-coordinates))

will return the distance between two coordinates.teractively:

    (geohash-coordinates '(graticule-latitude graticule-longitude)
                         '(year month day))

will return the geohash coordinates for this graticules on that particular date.

    (calc-nearest-geohash '(exact-latitude exact-longitude)
                          '(year month day))

will return a list of the latitude and longitude of the nearest geohash to your
exact coordinates.

    (calc-distance '(geohash-coordinates) '(exact-coordinates))

will return the distance between two coordinates.

    (get-osm-link geohash-coordinates)

will format a string to get a link to an OSM map for these coordinates.
