# geohashing library for emacs lisp
Collection of interactive and non-interactive functions in emacs lisp for the
game of geohashing. Work in progress. Has probably bugs,
is not guaranteed to be stable, code style might be bad and not well documented.
So use it with care.

Explanation of the mgeohashing game: http://wiki.xkcd.com/geohashing/

The algorithm: http://wiki.xkcd.com/geohashing/Algorithm

Explanation of the W30 rule, which is an additon to *the algorithm* and is implemented here:
http://wiki.xkcd.com/geohashing/30W_Time_Zone_Rule

* * *
## Installation and basic interactive use

Copy the file `geohashing.el` somewhere into your emacs path and add

    (require 'geohashing)

to your emacs config, e.g. `.emacs`.

Also, set the calendar variables `calendar-latitude` and
`calendar-longitude`, e.g. by inserting the following into your `.emacs`:

    (setq calendar-latitude 42.42)
    (setq calendar-longitude 31.41)

(Those variables are used for example when typing `S`
in the emacs calendar mode to display the sunrise and sunset times.)

Then you can type

    M-x geohashing

and you will be prompted for a date same as in org-mode.
Then you will get the geohash coordinates for that date,
the distance and you will be asked if you want to open the coordinates
on a map in the webbrowser.

* * *
## Advanced use

More things are possible when using it non-interactively:

    (geohash-coordinates '(graticule-latitude graticule-longitude)
                         '(year month day))

will return the geohash coordinates for this graticule on that particular date.

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

    (get-osm-url geohash-coordinates)

will format a string to get a url to an OSM map for these coordinates.
