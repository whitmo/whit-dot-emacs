;;; eimp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (eimp-roll-image-down eimp-roll-image-up eimp-roll-image-left
;;;;;;  eimp-roll-image-right eimp-decrease-image-contrast eimp-increase-image-contrast
;;;;;;  eimp-decrease-image-brightness eimp-increase-image-brightness
;;;;;;  eimp-rotate-image-anticlockwise eimp-rotate-image-clockwise
;;;;;;  eimp-flop-image eimp-flip-image eimp-radial-blur-image eimp-gaussian-blur-image
;;;;;;  eimp-emboss-image eimp-sharpen-image eimp-mouse-resize-image-preserve-aspect
;;;;;;  eimp-mouse-resize-image eimp-fit-image-width-to-window eimp-fit-image-height-to-window
;;;;;;  eimp-fit-image-to-whole-window eimp-fit-image-to-window eimp-decrease-image-size
;;;;;;  eimp-increase-image-size eimp-negate-image eimp-stop-all
;;;;;;  eimp-mode) "eimp" "eimp.el" (21555 57961 342419 664000))
;;; Generated autoloads from eimp.el

(autoload 'eimp-mode "eimp" "\
Toggle Eimp mode.

\(fn &optional ARG)" t nil)

(autoload 'eimp-stop-all "eimp" "\
Stop all running processes; remove queued processes.
If ERROR, signal an error with this string.

\(fn &optional ERROR)" t nil)

(autoload 'eimp-negate-image "eimp" "\
Negate image.

\(fn)" t nil)

(autoload 'eimp-increase-image-size "eimp" "\
Increase image size by ARG or default `eimp-resize-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-decrease-image-size "eimp" "\
Decrease image size by ARG or default `eimp-resize-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-fit-image-to-window "eimp" "\
Scale image to fit in the current window.
With a prefix arg, ARG, don't preserve the aspect ratio.

\(fn ARG)" t nil)

(autoload 'eimp-fit-image-to-whole-window "eimp" "\
Scale image to fit the whole of the current window.
The aspect ratio is not preserved.

\(fn)" t nil)

(autoload 'eimp-fit-image-height-to-window "eimp" "\
Scale image height to fit in the current window.
With a prefix arg, ARG, don't preserve the aspect ratio.

\(fn ARG)" t nil)

(autoload 'eimp-fit-image-width-to-window "eimp" "\
Scale image width to fit in the current window.
With a prefix arg, ARG, don't preserve the aspect ratio.

\(fn ARG)" t nil)

(autoload 'eimp-mouse-resize-image "eimp" "\
Resize image with mouse.
Argument EVENT is a mouse event.

\(fn EVENT)" t nil)

(autoload 'eimp-mouse-resize-image-preserve-aspect "eimp" "\
Resize image with mouse, preserving aspect ratio.
Argument EVENT is a mouse event.

\(fn EVENT)" t nil)

(autoload 'eimp-sharpen-image "eimp" "\
Sharpen image by ARG or default `eimp-blur-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-emboss-image "eimp" "\
Emboss image by ARG or default `eimp-blur-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-gaussian-blur-image "eimp" "\
Gaussian blur image by ARG or default `eimp-blur-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-radial-blur-image "eimp" "\
Radial blur image by ARG or default `eimp-blur-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-flip-image "eimp" "\
Flip image vertically.

\(fn)" t nil)

(autoload 'eimp-flop-image "eimp" "\
Flip image horizontally.

\(fn)" t nil)

(autoload 'eimp-rotate-image-clockwise "eimp" "\
Rotate image clockwise by ARG or default `eimp-rotate-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-rotate-image-anticlockwise "eimp" "\
Rotate image anticlockwise by ARG or default `eimp-rotate-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-increase-image-brightness "eimp" "\
Increase image brightness by ARG or default `eimp-brightness-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-decrease-image-brightness "eimp" "\
Decrease image brightness by ARG or default `eimp-brightness-amount'.

\(fn ARG)" t nil)

(autoload 'eimp-increase-image-contrast "eimp" "\
Increase image contrast.

\(fn)" t nil)

(autoload 'eimp-decrease-image-contrast "eimp" "\
Decrease image contrast.

\(fn)" t nil)

(autoload 'eimp-roll-image-right "eimp" "\
Roll image right by ARG pixels.

\(fn ARG)" t nil)

(autoload 'eimp-roll-image-left "eimp" "\
Roll image left by ARG pixels.

\(fn ARG)" t nil)

(autoload 'eimp-roll-image-up "eimp" "\
Roll image up by ARG pixels.

\(fn ARG)" t nil)

(autoload 'eimp-roll-image-down "eimp" "\
Roll image down by ARG pixels.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("eimp-pkg.el") (21555 57961 358727 529000))

;;;***

(provide 'eimp-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eimp-autoloads.el ends here
