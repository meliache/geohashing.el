# Geohashing 🎲🌐 with the Emacs text editor

[Geohashing](https://geohashing.site/geohashing/Main_Page) coordinates calculator for the [Emacs](https://www.gnu.org/software/emacs) text-editor.

## Disclaimer

- This is package still in early development and work-in-progress.
- `geohashing.el` was originally my first emacs lisp programming project, which I made when I was very new to programming in general. As I rediscovered geohashing for myself recently, I starting cleaning it up and make this a proper emacs package, but it's still not following many emacs best practices. 
- Not all [quirks of the algorithm](https://geohashing.site/geohashing/The_Algorithm#Quirks) are implemented yet. `geohashing.el` takes into account

## Installation and configuration

### Requirements
- [osm.el](https://github.com/minad/osm) for opening the geohash coordinates in a map within Emacs.

### Manual installation
Copy the file `geohashing.el` somewhere into your emacs load-path and add
``` emacs-lisp
(require 'geohashing)
```
to your emacs config, e.g. `.emacs`.

To calculate the nearest geohash, `geohashing.el` needs your home-coordinates, which can be customized via `geohashing-home`. By default, the package looks if you have set `osm-home` variable from the `osm.el` package or the `calendar-latitude` and `calendar-longitude` variables from `calendar.el` package and if one of those is bound, it uses that as the default home coordinate for geohashing. To set a custom value for `geohashing.el` (or if you haven't the above variable set), use the customize menu via `M-x customize-group geohashing` set the coordinates in  your init file via
``` emacs-lisp
(customize-set-variable 'geohashing-home '(31.14 15.92))
```

(**Warning:** Your home coordinates are sensitive information, don't set them in your init file if it is in a public repository or you intend to share it otherwise.)

### Installation via `straight` + `use-package`

If you use [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/radian-software/straight.el), you can also install it comfortably by just adding to your init file:
``` emacs-lisp
(use-package geohashing
  :straight (:type git :host github :repo "meliache/geohashing.el"))
  :custom
  (geohashing-home '(31.14 15.92)
```

## Usage

Run `M-x geohashing` and you will be prompted for a date. Then you will get the
geohash coordinates for that date, the distance and you will be asked if you
want to open the coordinates on a map in the webbrowser.
