(require 'cl)
(require 'calendar)
(require 'org)

(setq home-coordinates
      (list calendar-latitude calendar-longitude))
(setq home-graticule
      (list (floor calendar-latitude) (floor calendar-longitude)))

(defun latitude (coordinates)
  "Takes coordinates as a list '(latitude longitude), returns latitude.
At the moment alias for car."
  (car coordinates))

(defun longitude (coordinates)
  "Takes coordinates as a list '(latitude longitude), returns longitude.
At the moment alias for cadr"
  (cadr coordinates))

(defun date-today ()
  "Returns today's date as list of the form '(year month day)."
  (cl-destructuring-bind (month day year) (calendar-current-date)
    (list year month day)))

(defun +days (date day-delta)
  (cl-destructuring-bind (year month day) date
    (list year month (+ day day-delta))))

(defun w30-rule (coordinates)
  "True if longitude west of -30deg.
Takes coordinates as '(latitude longitude)"
  (< (car coordinates) -30))

(defun get-djia-string (date)
  "Takes date as list of '(year month day)
and returns the Dow Jones Industrial Average as a string,
if known for that date.
In case that DJIA for that date is unkown, fails, as no error handling
is implemented yet.
Uses API at carabiner.peeron.com. "
  (let* ((year (car date))
         (month (second date))
         (day (third date))
         (djia-url (format "http://carabiner.peeron.com/xkcd/map/data/%d/%02d/%02d"
                           year month day))
         (buffer (url-retrieve-synchronously djia-url))
         (djia-string nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^[0-9\.]+" nil 'move)
      (setq djia-string
            (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
      (kill-buffer (current-buffer)))
    (if (equal "" djia-string)
        (error "Couldn't get DJIA for that date.")
      djia-string)))

(defun get-md5-string (date w30)
  "Calculates the md5 sum from the date and DJIA
according to the geohashing algorithm."
  (let* ((year (car date))
         (month (second date))
         (day (third date))
         (djia-string (get-djia-string
                       (list year month (if w30 day (1- day)))))
         (date-djia-string
          (format "%d-%02d-%02d-%s" year month day djia-string)))
    (secure-hash 'md5 date-djia-string)))

(defun geohash-offset (date w30)
  "Takes date as a list of the form '(year month date) and boolean for W30 rule
and returns the offset of the geohash of that date in respect to a graticule
as a 2-value list."
  (let ((md5-string (get-md5-string date w30)))
    (cl-loop for (start end) in '((0 16) (16 32))
             collect (/ (string-to-number
                         (subseq md5-string start end)
                         16)
                        (expt 16.0 16)))))

(defun geohash-coordinates (graticule date)
  "Returns geohash coordinates for given graticule '(latitude longitudeg)
and date '(year month day) as a 2-value list '(latitude longitudeg)."
  (let ((offset (geohash-offset date (w30-rule graticule))))
    (list (+ (latitude graticule) (latitude offset))
          (+ (longitude graticule) (longitude offset)))))

(defun haversin (angle)
  "Haversine function of an angle given in radians"
  (/ (- 1 (cos angle)) 2))
(defun calc-distance (coord1 coord2)
  "Calculates the distance between two coordinates on earth.
Uses the haversine formula .
http://en.wikipedia.org/wiki/Haversine_formula#The_haversine_formula"
  (let* ((earth-radius 6371)
         (rcoord1 (mapcar (lambda (x) (* degrees-to-radians x))
                          coord1))
         (rcoord2 (mapcar (lambda (x) (* degrees-to-radians x))
                          coord2))
         (delta (cl-mapcar '- rcoord2 rcoord1)))
    (* 2 earth-radius
       (asin (sqrt (+ (haversin (latitude delta))
                      (* (cos (latitude rcoord1)) (cos (latitude rcoord2))
                         (haversin (longitude delta)))))))))

(defun adjecent-graticules (home-coord)
  "Returns the coordinates of the four graticules which are nearest
to the exact 'home' coordinates and should be searched for geohashes.
home-coord should be a list of two reals numbers, '(lat lon).
Assumes rectangle graticules and might be problematic near the poles."
  (let* ((lat (floor (latitude home-coord)))
         (lon (floor (longitude home-coord)))
         (delta-lat (if (>= (mod lat 1) 0.5) 1 -1))
         (delta-lon (if (>= (mod lon 1) 0.5) 1 -1)))
    (list (list lat lon)
          (list lat (+ delta-lon lon))
          (list (+ delta-lat lat) lon)
          (list (+ delta-lat lat) (+ delta-lon lon)))))

(defun minimize (fn-to-min list)
  "Takes the function fn-to-min and applies it to each element of list.
Returns the element of list for which fn-to-min gets minimal."
  (second (reduce
           (lambda (x y) (if (< (car x) (car y)) x y))
           (mapcar (lambda (l) (list (funcall fn-to-min l) l))
                   list))))

(defun calc-nearest-geohash (home-coords date)
  "Takes exact home coordinates as '(lat long) and date as '(year month day)
and returns list of coordinates of 'nearest geohash'.
The 'nearest geohash' is determined by calculating the geohash locations
for the 4 adjecant graticules and returning the geohash location with the
smallest distance to the home coordinates as returned by calc-distance."
  (let* ((offset (geohash-offset date (w30-rule home-coords)))
         (adjecent-grats (adjecent-graticules home-coords)))
    (minimize
     (lambda (hash-coords) (calc-distance hash-coords home-coords))
     (mapcar (lambda (grat)
               (list (+ (latitude grat) (latitude offset))
                     (+ (longitude grat) (longitude offset))))
             adjecent-grats))))

(defun get-osm-link (coordinates &optional zoomlevel)
  "Returns and OSM link to the given coordinates, with an optional OSM zoom level."
  (cl-destructuring-bind (lat lon) coordinates
    (if zoomlevel
        (format "https://www.openstreetmap.org/?mlat=%f&mlon=%f#map=%d/%f/%f"
                lat lon zoomlevel lat lon)
      (format "https://www.openstreetmap.org/?mlat=%f&mlon=%f#map=%d/%f/%f"
              lat lon 10 lat lon))))

(defun geohash-today ()
  "Uses the calendar-longitude and calendar-latitude variables
to calculate the nearest geohash for today.
Intended for quick interactive use."
  (interactive)
  (let* ((date (date-today))
         (home-coords
          (list calendar-latitude calendar-longitude))
         (geohash-coordinates
          (calc-nearest-geohash home-coords date)))
    (message (concat 
              (format "Nearest geohash at %s\nwith a distance of %f km.\n"
                      geohash-coordinates
                      (calc-distance home-coords geohash-coordinates))
              (get-osm-link geohash-coordinates)))))

(defun geohashing ()
  "Uses the calendar-longitude and calendar-latitude variables and prompt
the user for the date with the org-read-date function to calculate the nearest
geohash coordinates for that date.
Intended for quick interactive use."
  (interactive)
  (let* ((decoded-time (decode-time (org-read-date nil t)))
         (date (reverse (cl-subseq decoded-time 3 6)))
         (home-coords (list calendar-latitude calendar-longitude))
         (geohash-coordinates (calc-nearest-geohash home-coords date)))
    (message (concat 
              (format "Nearest geohash at %s\nwith a distance of %f km.\n"
                      geohash-coordinates
                      (calc-distance home-coords geohash-coordinates))
              (get-osm-link geohash-coordinates)))))

(provide 'geohashing)
