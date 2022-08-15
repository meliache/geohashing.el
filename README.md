# Geohashing 🎲🌐 with the Emacs text editor

[Geohashing](https://geohashing.site/geohashing/Main_Page) coordinates calculator for the [Emacs](https://www.gnu.org/software/emacs) text-editor.

## Disclaimer

- This is package still in early development and work-in-progress.
- `geohashing.el` was originally my first emacs lisp programming project, which I made when I was very new to programming in general. As I rediscovered geohashing for myself recently, I starting cleaning it up and make this a proper emacs package, but it's still not following many emacs best practices. 
- Not all [quirks of the algorithm](https://geohashing.site/geohashing/The_Algorithm#Quirks) are implemented yet. `geohashing.el` takes into account

## Installation 

### Manual
Copy the file `geohashing.el` somewhere into your emacs load-path and add
``` emacs-lisp
(require 'geohashing)
```
to your emacs config, e.g. `.emacs`.

Then, add your home coordinates or whichever coordinates you want to use for the nearest-geohash calculation, either via `M-x customize-group geohashing` or in your init file via
``` emacs-lisp
(customize-set-variable 'geohashing-latitude 31.14)
(customize-set-variable 'geohashing-longitude 15.92)
```
(**Warning:** Your home coordinates are sensitive information, don't set them in your init file if it is in a public repository or you intend to share it otherwise.)

### `straight` + `use-package`

If you use [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/radian-software/straight.el), you can also install it comfortably by just adding to your init file:
``` emacs-lisp
(use-package geohashing
  :straight (:type git :host github :repo "meliache/geohashing.el"))
  :custom
  (geohashing-latitude 31.14)
  (geohashing-longitude 15.92)
```

## Usage

Run `M-x geohashing` and you will be prompted for a date. Then you will get the
geohash coordinates for that date, the distance and you will be asked if you
want to open the coordinates on a map in the webbrowser.
