;;; geohashing.el --- Find geohashes near you.  -*- lexical-binding: t; -*-

;; Author: Michael Eliachevitch <m.eliachevitch@posteo.de>
;; Maintainer: Michael Eliachevitch <m.eliachevitch@posteo.de>
;; Homepage: https://github.com/meliache/geohashing.el

;;; Commentary:
;; TODO: add commentary

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'cl-macs)
(require 'org)
(require 'seq)

(defcustom geohashing-latitude 0.0
  "Decimal latitude used for calculating the nearest geohash."
  :type 'number
  :group 'geohashing)

(defcustom geohashing-longitude 0.0
  "Decimal longitude used for calculating the nearest geohash."
  :type 'number
  :group 'geohashing)


(defun geohashing--get-latitude (coordinates)
  "Take COORDINATES as a list `(latitude longitude)' [deg]. Return latitude."
  (car coordinates))

(defun geohashing--get-longitude (coordinates)
  "Takes COORDINATES as a list `(latitude longitude)' [deg]. Return longitude."
  (cl-second coordinates))

(defun calendar-date-to-iso (calendar-date)
  "Takes list `(month day year)', which is the date order that
calendar.el uses, and rearranges it to the ISO norm of 
`(year month day)', which is used by `org-mode' and and this library."
  (cl-destructuring-bind (month day year) calendar-date
    (list year month day)))

(defun iso-to-calendar-date (iso-date)
  "Takes a date `(year month day)' in the ISO norm as used in this library,
and rearranges it to `(month day year)', which is the order
that calendar.el uses."
  (cl-destructuring-bind (year month day) iso-date
    (list month day year)))

(defun date-compare (date1 date2)
  "Take two dates as `(year month day)', true if DATE1 before DATE2."
  (cl-destructuring-bind
      (year1 month1 day1 year2 month2 day2)
      (append date1 date2)
    (calendar-date-compare (cons (list month1 day1 year1) nil)
                           (cons (list month2 day2 year2) nil))))
(defun +days (date days-to-add)
  "Take DATE as `(year month day)' and add the DAYS-TO-ADD number of days."
  (calendar-date-to-iso
   (calendar-gregorian-from-absolute
    (+ (calendar-absolute-from-gregorian
        (iso-to-calendar-date date))
       days-to-add))))

(defun date-today ()
  "Return today's date as list of the form `(year month day)'."
  (calendar-date-to-iso (calendar-current-date)))

(defconst first-day-of-30W '(2008 05 27))

(defun 30W-rule (coordinates date)
  "Determine if 30W rule has to be applied for COORDINATES and given DATE.
True if longitude east of -30deg and date is after (or on) 2008-05-27"
  (and (> (elt coordinates 0) -30)
       (not (date-compare date first-day-of-30W))))

(30W-rule (list geohashing-latitude geohashing-longitude) (date-today))

(defun hex-to-fractional (hex-string)
  "Convert a lowercase HEX-STRING to a decimal fraction."
  (/ (string-to-number hex-string 16) (expt 16.0 (length hex-string))))

(defun get-djia-string (date)
  "Return DJIA string for DATE.
Takes DATE as list of `(year month day)' and returns the Dow
Jones Industrial Average as a string, if known for that date. In
case that DJIA for that date is unkown, fails, as no error
handling is implemented yet. Uses API at carabiner.peeron.com."
  (cl-destructuring-bind (year month day) date
    (let* ((djia-url
            (format "http://carabiner.peeron.com/xkcd/map/data/%d/%02d/%02d"
                    year month day))
           (buffer (url-retrieve-synchronously djia-url))
           (djia-string nil))
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^[0-9\.]+" nil 'move)
        (setq djia-string
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (kill-buffer (current-buffer)))
      (if (equal "" djia-string)
          (error "Couldn't get DJIA for that date")
        djia-string))))

(defun get-md5-string (date djia-string)
  "Calculate the md5 hash string from the DATE and DJIA-STRING."
  (cl-destructuring-bind (year month day) date
    (secure-hash 'md5 (format "%d-%02d-%02d-%s" year month day djia-string))))

(defun geohash-offset (date 30W)
  "Calculate decimal points of geohash for DATE depending on 30W rule.
Takes DATE as a list of the form `(year month date)' and boolean
for 30W rule and returns the offset of the geohash of that date
in respect to a graticule as a 2-value list."
  (let* ((djia-string (get-djia-string (if 30W (+days date -1) date)))
         (md5-string (get-md5-string date djia-string)))
    (cl-loop for (start end) in '((0 16) (16 32))
             collect (hex-to-fractional (substring md5-string start end)))))

(defun geohash-coordinates (graticule date)
  "Return geohash coordinates for given GRATICULE and DATE."
  (let ((offset (geohash-offset date (30W-rule graticule date))))
    (list (+ (geohashing--get-latitude graticule) (geohashing--get-latitude offset))
          (+ (geohashing--get-longitude graticule) (geohashing--get-longitude offset)))))

(defun haversin (angle)
  "Haversine function of ANGLE given in radians.
 See
 https://en.wikipedia.org/wiki/Haversine_formula#The_haversine_formula"
  (/ (- 1 (cos angle)) 2))

(defun calc-distance (coord1 coord2)
  "Calculate the distance between two coordinates on earth COORD1 and COORD2."
  (let* ((earth-radius 6371)
         (rcoord1 (mapcar (lambda (x) (degrees-to-radians x)) coord1))
         (rcoord2 (mapcar (lambda (x) (degrees-to-radians x)) coord2))
         (delta (cl-mapcar '- rcoord2 rcoord1)))
    (* 2 earth-radius (asin (sqrt (+ (haversin (geohashing--get-latitude delta))
                                     (* (cos (geohashing--get-latitude rcoord1))
                                        (cos (geohashing--get-latitude rcoord2))
                                        (haversin (geohashing--get-longitude delta)))))))))

(defun adjecent-graticules (home-coord)
  "Return the coordinates of the four graticules nearest to HOME-COORD.
These are which should be searched for the nearest geohash.
Assumes rectangle graticules and might be problematic near the
poles."
  (let* ((lat (floor (geohashing--get-latitude home-coord)))
         (lon (floor (geohashing--get-longitude home-coord)))
         (delta-lat (if (>= (mod lat 1) 0.5) 1 -1))
         (delta-lon (if (>= (mod lon 1) 0.5) 1 -1)))
    (list (list lat lon)
          (list lat (+ delta-lon lon))
          (list (+ delta-lat lat) lon)
          (list (+ delta-lat lat) (+ delta-lon lon)))))

(defun minimize (fn-to-min list)
  "Take the function FN-TO-MIN and apply it to each element of LIST.
Returns the element of list for which fn-to-min gets minimal."
  (cl-second (cl-reduce (lambda (x y) (if (< (car x) (car y)) x y))
                  (mapcar (lambda (l) (list (funcall fn-to-min l) l)) list))))

(defun calc-nearest-geohash (home-coords date)
  "Take exact home coordinates as `(lat long)' and DATE as `(year month day)'
and returns list of coordinates of nearest geohash.
The nearest geohash is determined by calculating the geohash locations
for the four adjecant graticules and returning the geohash location with the
smallest distance to the home coordinates as returned by calc-distance."
  (let* ((offset (geohash-offset date (30W-rule home-coords date)))
         (adjecent-grats (adjecent-graticules home-coords)))
    (minimize
     (lambda (hash-coords) (calc-distance hash-coords home-coords))
     (mapcar (lambda (grat)
               (list (+ (geohashing--get-latitude grat) (geohashing--get-latitude offset))
                     (+ (geohashing--get-longitude grat) (geohashing--get-longitude offset))))
             adjecent-grats))))

(defun get-osm-url (coordinates &optional zoom-level)
  "Return an OSM url for COORDINATES, with an optional ZOOM-LEVEL."
  (cl-destructuring-bind (lat lon) coordinates
    (if zoom-level
        (format "https://www.openstreetmap.org/?mlat=%f&mlon=%f#map=%d/%f/%f"
                lat lon zoom-level lat lon)
      (format "https://www.openstreetmap.org/?mlat=%f&mlon=%f#map=%d/%f/%f"
              lat lon 10 lat lon))))

(defun geohashing ()
  "Use the GEOHASHING-LONGITUDE and GEOHASHING-LATITUDE variables and prompt
the user for the date with the `org-read-date' function to
calculate the nearest geohash coordinates for that date. Intended
for quick interactive use."
  (interactive)
  (let* ((decoded-time (decode-time (org-read-date nil t)))
         (date (reverse (seq-subseq decoded-time 3 6)))
         (home-coords (list geohashing-latitude geohashing-longitude))
         (geohash-coordinates (calc-nearest-geohash home-coords date))
         (osm-url (get-osm-url geohash-coordinates)))
    (when (yes-or-no-p
           (format "Nearest geohash at %s\nwith a distance of %f km.\nOpen in webbrowser?"
                   geohash-coordinates
                   (calc-distance home-coords geohash-coordinates)))
      (browse-url osm-url))))

(provide 'geohashing)

;;; geohashing.el ends here
