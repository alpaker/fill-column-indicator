;;; fill-column-indicator.el --- graphically indicate the fill column

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <alp.tekin.aker@gmail.com>
;; Version: 1.63
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Many modern editors and IDEs can graphically indicate the location of the
;; fill column by drawing a thin line (in design parlance, a `rule') down the
;; length of the editing window.  Fill-column-indicator implements this
;; facility in Emacs.

;; Installation and Usage
;; ======================

;; Put this file in your load path and put
;;
;;   (require 'fill-column-indicator)
;;
;; in your .emacs.

;; To toggle graphical indication of the fill column in a buffer, use the
;; command `fci-mode'.

;; Configuration
;; =============

;; On displays that can show images, the fill-column rule is drawn using a
;; bitmap.  Its color is controlled by the variable `fci-rule-color'; its
;; value can be any valid color name.  The rule's width in pixels is
;; determined by the variable `fci-rule-width'; the default value is 2.

;; The image formats fci-mode can use are XPM, PBM, and XBM.  By default it
;; uses XPM images if Emacs has been compiled with the appropriate library;
;; if not, it uses PBM images, which are natively supported.  You can specify
;; a particular format by setting `fci-rule-image-format' to either 'xpm,
;; 'xpm, or 'xbm.

;; On character terminals the rule is drawn using the character specified by
;; `fci-rule-character'; the default is `|' (ascii 124).  If
;; `fci-rule-character-color' is nil, then it is drawn using fci-rule-color
;; (or the closest appromixation thereto that the terminal is capable of); if
;; it is a color name, then that color is used instead.

;; If you'd like the rule to be drawn using fci-rule-character even on
;; graphical displays, set `fci-always-use-textual-rule' to a non-nil value.

;; Other Options
;; =============

;; When `truncate-lines' is nil, the effect of drawing a fill-column
;; indicator is very odd looking. Indeed, in a window with continuation
;; lines, it makes little sense to indicate the position of the fill column
;; relative to the window edge (think about what it would mean to talk about
;; "the" location of the fill column in that case).  For this reason,
;; fci-mode sets truncate-lines to t in buffers in which it is enabled and
;; restores it to its previous value when disabled.  You can turn this
;; feature off by setting `fci-handle-truncate-lines' to nil.

;; If `line-move-visual' is t, then vertical navigation can behave oddly in
;; several edge cases while fci-mode is enabled (this is due to a bug in C
;; code).  Accordingly, fci-mode locally sets line-move-visual to nil when
;; enabled and restores it when disabled.  This can be suppressed by setting
;; `fci-handle-line-move-visual' to nil.  (But you shouldn't want to do
;; this.  There's no reason to use line-move-visual if truncate-lines is t,
;; and it doesn't make sense to use something like fci-mode when
;; truncate-lines is nil.)

;; Troubleshooting
;; ===============

;; o If the fill-column rule is misaligned on some lines but otherwise looks
;;   normal, then you're most likely not displaying the buffer contents with
;;   a monospaced font.  Check whether the lines in question contain
;;   non-ascii characters that are wider or shorter than the normal character
;;   width.  Also, be aware that certain font-lock themes set some faces so
;;   that they look monospaced but aren't quite so.

;; o Although the XBM format is natively supported by Emacs, the
;;   implementation on some ports is incomplete; the v23 and v24 Mac OS X
;;   ports are examples.  On these systems XBM images are always drawn in
;;   black, in which case one cannot control the color of the rule.  Use XPM
;;   or PBM images instead.

;; o Fci-mode needs free use of two characters (specifically, it needs the
;;   use of two characters whose display table entries it can change
;;   arbitrarily).  By default, it uses the first two characters of the
;;   Private Use Area of the Unicode BMP, viz. U+E000 and U+E001.  If you
;;   need to use those characters for some reason, set `fci-padding-char' and
;;   `fci-eol-char' to different values.

;; Known Issues
;; ============

;; o The indicator extends only to end of the buffer contents (as opposed to
;;   running the full length of the editing window).

;; o When portions of a buffer are invisible, such as when outline mode is
;;   used to hide certain lines, the fill-column rule is hidden as
;;   well.  

;; o An issue specific to the Mac OS X (NextStep) port, versions 23.0-23.2:
;;   Emacs won't, in these particular versions, draw a cursor on top of an
;;   image.  Thus on graphical displays the cursor will disappear when
;;   positioned directly on top of the fill-column rule.  The best way to
;;   deal with this is to upgrade to 23.3 or to 24 (or downgrade to 22).  If
;;   that isn't practical, a fix is available via the mini-package
;;   fci-osx-23-fix.el, which can be downloaded from:
;;
;;     github.com/alpaker/Fill-Column-Indicator
;;
;;  Directions for its use are given in the file header.

;; Todo
;; ====

;; o Accomodate non-nil values of `hl-line-sticky-flag' and similar cases.

;; o Accomodate linum-mode more robustly.

;;; Code:

;;; ---------------------------------------------------------------------
;;; Version Specifics
;;; ---------------------------------------------------------------------

(unless (version<= "22" emacs-version)
  (error "Fill-column-indicator requires version 22 or later"))

(if (fboundp 'characterp)
    (defalias 'fci-character-p 'characterp)
  ;; For v22.
  (defun fci-character-p (c)
    (and (wholenump c)
         (/= 0 c)
         ;; MAX_CHAR in v22 is (0x1F << 14).  We don't worry about
         ;; generic chars.
         (< c 507904))))

;;; ---------------------------------------------------------------------
;;; User Options
;;; ---------------------------------------------------------------------

(defgroup fill-column-indicator nil
  "Graphically indicate the fill-column."
  :tag "Fill-Column Indicator"
  :group 'convenience
  :group 'fill)

(defcustom fci-rule-color "#cccccc"
  "Color used to draw the fill-column rule.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :group 'fill-column-indicator
  :tag "Fill-column rule color"
  :type 'color)

;; We should be using :validate instead of :match, but that seems not to
;; work with defcustom widgets.
(defcustom fci-rule-width 2
  "Width, in pixels, of the fill-column rule on graphical displays.
The value must be less than the default character width.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Width"
  :group 'fill-column-indicator
  :type  '(integer :match (lambda (w val) (<= val (frame-char-width)))))

(defcustom fci-rule-image-format
  (if (image-type-available-p 'xpm) 'xpm 'pbm)
  "Image format used for the fill-column rule on graphical displays.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Image Format"
  :group 'fill-column-indicator
  :type '(choice (symbol :tag "XPM" 'xpm)
                 (symbol :tag "PBM" 'pbm)
                 (symbol :tag "XBM" 'xbm)))

(defcustom fci-rule-character ?|
  "Character use to draw the fill-column rule on character terminals.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Character"
  :group 'fill-column-indicator
  :type 'character)

(defcustom fci-rule-character-color nil
  "Color used to draw the fill-column rule with characters.
If nil, the same color is used as for the graphical rule.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :group 'fill-column-indicator
  :tag "Fill-column rule color"
  :type '(choice (const :tag "Use same color as graphical rule" nil)
                 (color :tag "Specify a color")))

(defcustom fci-always-use-textual-rule nil
  "When non-nil, the rule is always drawn using textual characters.
Specifically, fci-mode will use `fci-rule-character' intead of
bitmap images to draw the rule on graphical displays.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Don't Use Image for Fill-Column Rule"
  :group 'fill-column-indicator
  :type 'boolean)

(defcustom fci-handle-truncate-lines t
  "Whether fci-mode should set truncate-lines to t while enabled.
If non-nil, fci-mode will set truncate-lines to t in buffers in
which it is enabled, and restore it to its previous value when
disabled.

Leaving this option set to the default value is recommended."
  :group 'fill-column-indicator
  :tag "Locally set truncate-lines to t during fci-mode"
  :type 'boolean)

(defcustom fci-handle-line-move-visual (version<= "23" emacs-version)
  "Whether fci-mode should set line-move-visual to nil while enabled.
If non-nil, fci-mode will set line-move-visual to nil in buffers
in which it is enabled, and restore t to its previous value when
disabled.

Leaving this option set to the default value is recommended."
  :group 'fill-column-indicator
  :tag "Locally set line-move-visual to nil during fci-mode"
  :type 'boolean)

(defcustom fci-eol-char ?\uE000
  "Character used for internal purposes by fci-mode.
If you need to use this character, set this variable's value to a
character you do not care about (a good choice is a random
character from the Private Use Area of the Unicode BMP, i.e.,
something in the range \uE000-\uF8FF, inclusive)."
  :group 'fill-column-indicator
  :type 'character)

(defcustom fci-padding-char ?\uE001
  "Character used for internal purposes by fci-mode.
If you need to use this character, set this variable's value to a
character you do not care about (a good choice is a random
character from the Private Use Area of the Unicode BMP, i.e.,
something in the range \uE000-\uF8FF, inclusive)."
  :group 'fill-column-indicator
  :type 'character)

;;; ---------------------------------------------------------------------
;;; Internal Variables and Constants
;;; ---------------------------------------------------------------------

;; Save prior state of buffer.
(defvar fci-saved-line-move-visual nil)
(defvar fci-saved-truncate-lines nil)
(defvar fci-saved-eol nil)
(defvar fci-made-display-table nil)

;; State of fci initialization in this buffer.
(defvar fci-display-table-processed nil)
(defvar fci-local-vars-set nil)

;; Save current state of some quantities so we can detect changes to them.
(defvar fci-column nil)
(defvar fci-newline-sentinel nil)
(defvar fci-tab-width nil)
(defvar fci-char-width nil)
(defvar fci-char-height nil)

;; Data used in setting the fill-column rule that only need to be
;; occasionally updated in a given buffer.
(defvar fci-buffer-windows nil)
(defvar fci-limit nil)
(defvar fci-pre-limit-string nil)
(defvar fci-at-limit-string nil)
(defvar fci-post-limit-string nil)

;; The preceding internal variables need to be buffer local.
(dolist (var '(fci-saved-line-move-visual
               fci-saved-truncate-lines
               fci-saved-eol
               fci-made-display-table
               fci-display-table-processed
               fci-local-vars-set
               fci-column
               fci-newline-sentinel
               fci-tab-width
               fci-char-width
               fci-char-height
               fci-buffer-windows
               fci-limit
               fci-pre-limit-string
               fci-at-limit-string
               fci-post-limit-string))
  (make-variable-buffer-local var))

;; Hooks we use.
(defconst fci-hook-assignments
  '((after-change-functions . fci-redraw-region)
    (before-change-functions . fci-extend-rule-for-deletion)
    (window-scroll-functions . fci-update-window-for-scroll)
    (window-configuration-change-hook . fci-schedule-full-update)
    (post-command-hook . fci-post-command-check)
    (change-major-mode-hook . (lambda () (fci-mode 0)))
    (longlines-mode-hook . fci-full-update)))

;;; ---------------------------------------------------------------------
;;; Mode Definition
;;; ---------------------------------------------------------------------

(define-minor-mode fci-mode
  "Toggle fci mode on and off.
Fci-mode indicates the location of the fill column by drawing a
thin line (a `rule') at the fill column.

With prefix ARG, turn fci-mode on if and only if ARG is positive.

The following options control the appearance of the fill-column
indicator: `fci-rule-width', `fci-rule-color',
`fci-rule-character', and `fci-rule-character-color'.  For
further options, see the Customization menu or the package
file.  (See the latter for tips on troubleshooting.)"

  nil nil nil

  (if fci-mode
      ;; Enabling.
      (condition-case error
          (progn
            (unless (memq fci-rule-image-format '(xpm xbm pbm))
              (error "Unrecognized value of `fci-rule-image-format'"))
            (unless (color-defined-p fci-rule-color)
              (error "Value of `fci-rule-color' must be a color name"))
            (when (and fci-rule-character-color
                       (not (color-defined-p fci-rule-character-color)))
              (error "Value of `fci-rule-character-color' must be a color name"))
            (unless (fci-character-p fci-rule-character)
              (error "Value of `fci-rule-character' must be a character"))
            (fci-process-display-table)
            (setq fci-column fill-column
                  fci-tab-width tab-width
                  fci-limit (if fci-newline-sentinel
                                (1+ (- fill-column (length fci-saved-eol)))
                              fill-column))
            (fci-make-overlay-strings)
            (fci-set-local-vars)
            (fci-full-update))
        (error 
         (fci-mode 0)
         (signal (car error) (cdr error))))

    ;; Disabling.
    (fci-restore-display-table)
    (fci-restore-local-vars)
    (remove-hook 'post-command-hook #'fci-full-update t)
    (fci-delete-overlays-buffer)
    (setq fci-column nil
          fci-limit nil
          fci-tab-width nil
          fci-char-width nil
          fci-char-height nil
          fci-buffer-windows nil
          fci-pre-limit-string nil
          fci-at-limit-string nil
          fci-post-limit-string nil)))

;;; ---------------------------------------------------------------------
;;; Initialization and Clean Up: Display Table, Hooks, Etc.
;;; ---------------------------------------------------------------------

(defun fci-process-display-table ()
  (unless fci-display-table-processed
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)
            fci-made-display-table t))
    (aset buffer-display-table fci-padding-char [32])
    (setq fci-saved-eol (aref buffer-display-table 10))
    ;; Assumption: the display-table entry for character 10 is either nil or
    ;; a vector whose last element is the newline glyph.
    (let ((glyphs (butlast (append fci-saved-eol nil)))
          eol)
      (if glyphs
          (setq fci-newline-sentinel [10]
                eol (vconcat glyphs))
        (setq fci-newline-sentinel nil
              eol [32]))
      (aset buffer-display-table 10 fci-newline-sentinel)
      (aset buffer-display-table fci-eol-char eol))
    (setq fci-display-table-processed t)))

(defun fci-restore-display-table ()
  (aset buffer-display-table 10 fci-saved-eol)
  ;; Don't set buffer-display-table to nil even if we created it; only do so
  ;; if it's also empty.
  (when (and fci-made-display-table
             (equal buffer-display-table (make-display-table)))
    (setq buffer-display-table nil))
  (setq fci-display-table-processed nil
        fci-made-display-table nil
        fci-newline-sentinel nil
        fci-saved-eol nil))

(defun fci-set-local-vars ()
  (unless fci-local-vars-set
    (dolist (hook fci-hook-assignments)
      (add-hook (car hook) (cdr hook) nil t))
    (when (and fci-handle-line-move-visual
               (boundp 'line-move-visual))
      (setq fci-saved-line-move-visual line-move-visual)
      (set (make-local-variable 'line-move-visual) nil))
    (when fci-handle-truncate-lines
      (setq fci-saved-truncate-lines truncate-lines)
      (set (make-local-variable 'truncate-lines) t))
    (setq fci-local-vars-set t)))

(defun fci-restore-local-vars ()
  (dolist (hook fci-hook-assignments)
    (remove-hook (car hook) (cdr hook) t))
  (when fci-local-vars-set
    (when (and fci-handle-line-move-visual
               (boundp 'line-move-visual))
      (setq line-move-visual fci-saved-line-move-visual
            fci-saved-line-move-visual nil))
    (when fci-handle-truncate-lines
      (setq truncate-lines fci-saved-truncate-lines
            fci-saved-truncate-lines nil)))
  (setq fci-local-vars-set nil))

;;; ---------------------------------------------------------------------
;;; Initialization:  Overlay Strings
;;; ---------------------------------------------------------------------

;; Return true if there is some other overlay at POS that wants to fontify
;; POS with a background color.
(defun fci-overlay-check (pos)
  (not (memq t (mapcar #'(lambda (x)
                           (and (not (overlay-get x 'fci))
                                (overlay-get x 'face)
                                (not (eq (face-attribute
                                          (overlay-get x 'face)
                                          :background nil t)
                                         'unspecified))))
                       (overlays-at pos)))))

;; The display spec used in overlay before strings to pad out the rule to the
;; fill-column.  If some other overlay wants to color that region, don't
;; display anything.
(defconst fci-padding-display
  '((when (fci-overlay-check buffer-position)
      . (space :align-to fci-column))
    (space :width 0)))

;; Generate the display spec for the rule.  Basic idea is to display the
;; textual rule if the display doesn't support images and the graphical rule
;; if it does.  But in both cases, only display if no other overlay wants to
;; fill the background of this region. 
(defun fci-rule-display (blank img str pre)
  "Generate a display specification for a fill-column rule overlay string."
  (let ((cursor (if (and (not pre) (not fci-newline-sentinel)) 1)))
    (propertize blank
                'cursor cursor
                'display
                (if img
                    `((when (and (not (display-images-p)) 
                                 (fci-overlay-check buffer-position))
                        . ,(propertize str 'cursor cursor))
                      (when (fci-overlay-check buffer-position)
                        . ,img)
                      (space :width 0))
                  `((when (fci-overlay-check buffer-position)
                      . ,(propertize str 'cursor cursor))
                    (space :width 0))))))

(defun fci-make-overlay-strings ()
  "Generate the overlay strings used to display the fill-column rule."
  (let* ((str (fci-make-rule-string))
         (img (fci-make-rule-img))
         (blank (char-to-string fci-padding-char))
         (eol-str (char-to-string fci-eol-char))
         (end-cap (propertize blank 'display '(space :width 0)))
         (eol (propertize blank
                          'cursor 1
                          'display (propertize eol-str 'cursor 1)))
         (padding (propertize blank 'display fci-padding-display))
         (before-rule (fci-rule-display blank img str t))
         (at-rule (fci-rule-display blank img str fci-newline-sentinel))
         (at-eol (if fci-newline-sentinel eol "")))
    (setq fci-pre-limit-string (concat eol padding before-rule)
          fci-at-limit-string (concat at-eol padding at-rule)
          fci-post-limit-string (concat eol end-cap))))

;;; ---------------------------------------------------------------------
;;; Initialization:  Generate Rule Image and Propertized Rule String
;;; ---------------------------------------------------------------------

(defun fci-make-rule-string ()
  "Return a string for drawing the fill-column rule."
  (let ((color (or fci-rule-character-color
                   fci-rule-color)))
    ;; Make sure we don't pick up weight or slant from font-lock.
    (propertize (char-to-string fci-rule-character)
                'face `
                (:foreground ,color :weight normal :slant normal))))

(defun fci-make-rule-img ()
  "Return an image descriptor for the fill-column rule."
  (unless fci-always-use-textual-rule
    (unless fci-char-width
      (setq fci-char-width (frame-char-width)
            fci-char-height (frame-char-height)))
    (when (< 1 fci-char-width)
      (cond
       ((eq fci-rule-image-format 'xbm)
        (fci-make-xbm-img))
       ((eq fci-rule-image-format 'pbm)
        (fci-make-pbm-img))
       ((eq fci-rule-image-format 'xpm)
        (fci-make-xpm-img))))))

(defun fci-make-xbm-img ()
  "Return an image descriptor for the fill-column rule in XBM format."
  (let* ((img-width (+ fci-char-width (- 8 (% fci-char-width 8))))
         (row-pixels (make-bool-vector fci-char-width nil))
         (raster (make-vector fci-char-height row-pixels))
         (left-margin (/ (- fci-char-width fci-rule-width) 2)))
    (dotimes (i fci-rule-width)
      (aset row-pixels (+ i left-margin) t))
    `(image :type xbm
            :data ,raster
            :foreground ,fci-rule-color
            :mask heuristic
            :ascent center
            :height ,fci-char-height
            :width ,fci-char-width)))

(defun fci-make-pbm-img ()
  "Return an image descriptor for the fill-column rule in PBM format."
  (let* ((height-str (number-to-string fci-char-height))
         (width-str (number-to-string fci-char-width))
         (margin (/ (- fci-char-width fci-rule-width) 2.0))
         (left-margin (floor margin))
         (right-margin (ceiling margin))
         (identifier "P1\n")
         (dimens (concat width-str " " height-str "\n"))
         (left-pixels (mapconcat #'identity (make-list left-margin "0") " "))
         (rule-pixels (mapconcat #'identity (make-list fci-rule-width "1") " "))
         (right-pixels (mapconcat #'identity (make-list right-margin "0") " "))
         (row-pixels (concat left-pixels " " rule-pixels " " right-pixels))
         (raster (mapconcat #'identity 
                            (make-list fci-char-height row-pixels) 
                            "\n"))
         (data (concat identifier dimens raster)))
    `(image :type pbm
            :data ,data
            :mask heuristic
            :foreground ,fci-rule-color
            :ascent center)))

(defun fci-make-xpm-img ()
  "Return an image descriptor for the fill-column rule in XPM format."
  (let* ((height-str (number-to-string fci-char-height))
         (width-str (number-to-string fci-char-width))
         (margin (/ (- fci-char-width fci-rule-width) 2.0))
         (left-margin (floor margin))
         (right-margin (ceiling margin))
         (identifier "/* XPM */\nstatic char *rule[] = {\n")
         (dims (concat "\"" width-str " " height-str " 2 1\",\n"))
         (color-spec (concat "\"1 c " fci-rule-color "\",\n \"0 c None\",\n"))
         (row-pixels (concat "\""
                             (make-string left-margin ?0)
                             (make-string fci-rule-width ?1)
                             (make-string right-margin ?0)
                             "\",\n"))
         (raster (mapconcat #'identity 
                            (make-list fci-char-height row-pixels)
                            ""))
         (end "};")
         (data (concat identifier dims color-spec raster end)))
    `(image :type xpm
            :data ,data
            :ascent center)))

;;; ---------------------------------------------------------------------
;;; Drawing and Erasing 
;;; ---------------------------------------------------------------------

(defmacro fci-sanitize-actions (&rest body)
  "Wrap fill-column rule-drawing functions in protective special forms."
  `(save-match-data
     (save-excursion
       (let ((inhibit-point-motion-hooks t))
         ,@body))))

(defsubst fci-get-overlays-region (start end)
  "Return all overlays between START and END displaying the fill-column rule."
  (delq nil (mapcar #'(lambda (o) (if (overlay-get o 'fci) o)) 
                    (overlays-in start end))))

(defsubst fci-delete-overlays-region (start end)
  "Delete overlays displaying the fill-column rule between START and END."
  (mapc #'(lambda (o) (if (overlay-get o 'fci) (delete-overlay o)))
        (overlays-in start end)))

;; It would be slightly faster to run this backwards from END to START, but
;; only if we maintained the overlay center at an early position in the
;; buffer.  Since other packages that use overlays typically place them while
;; traversing the buffer in a forward direction, that would be a bad idea.
(defun fci-put-overlays-region (start end)
  "Place overlays displaying the fill-column rule between START and END."
  (goto-char start)
  (let (found o cc)
    (while (search-forward "\n" end t)
      (goto-char (match-beginning 0))
      (setq cc (current-column)
            o (make-overlay (match-beginning 0) (match-beginning 0)))
      (overlay-put o 'fci t)
      (cond 
       ((< cc fci-limit)
        (overlay-put o 'after-string fci-pre-limit-string))
       ((> cc fci-limit)
        (overlay-put o 'after-string fci-post-limit-string))
       (t
        (overlay-put o 'after-string fci-at-limit-string)))
      (goto-char (match-end 0)))))

;; Inlining this matters only for the case in which the user creates repeated
;; scroll events (by, e.g., holding down C-v or M-v).  But that's a common
;; enough action that we should accomodate it.
(defsubst fci-delete-unneeded ()
  "Erase the fill-column rule at buffer positions not visible in any window."
  (let ((olays (fci-get-overlays-region (point-min) (point-max)))
        (ranges (mapcar #'(lambda (w) (cons (window-start w) (window-end w t)))
                        fci-buffer-windows))
        pos)
    (dolist (o olays)
      (setq pos (overlay-start o))
      (unless (memq t (mapcar #'(lambda (range) (and (<= (car range) pos) 
                                                     (< pos (cdr range))))
                              ranges))
        (delete-overlay o)))))

(defun fci-redraw-region (start end _ignored)
  "Erase and redraw the fill-column rule between START and END."
  (fci-sanitize-actions
   (goto-char end)
   (setq end (line-beginning-position 2))
   (fci-delete-overlays-region start end)
   (fci-put-overlays-region start end)))

;; This doesn't determine the strictly minimum amount by which the rule needs
;; to be extended, but the amount used is always sufficient, and in the modal
;; case determining the genuine minimum is slower.
(defun fci-extend-rule-for-deletion (start end)
  "Extend the fill-column after a deletion that spans newlines."
  (unless (= start end)
    (let ((delenda (fci-get-overlays-region start end)))
      (when delenda
        (mapc #'delete-overlay delenda)
        (let ((lossage 0)
              (max-end 0)
              win-end)
          (save-excursion
            (goto-char start)
            (while (search-forward "\n" end t)
              (setq lossage (1+ lossage))))
          (dolist (win fci-buffer-windows)
            ;; Do not ask for an updated value of window-end.
            (setq win-end (window-end win))
            (when (and (< 0 (- (min win-end end) 
                               (max (window-start win) start)))
                       (< max-end win-end))
              (setq max-end win-end)))
          (unless (= max-end (point-max))
            (fci-redraw-region max-end 
                               (save-excursion 
                                 (goto-char max-end)
                                 (line-beginning-position lossage)) nil)))))))

;; If N windows display the buffer, then window-configuration-change-hook
;; calls this function N times.  Since we only need to run the window update
;; once, we engage in a bit of misdirection and incur the lesser cost of N-1
;; unnecessary calls to `add-hook'.
(defun fci-schedule-full-update ()
  "Arrange to redraw the fill-column rule in all windows on this buffer."
  (add-hook 'post-command-hook #'fci-full-update nil t))

(defun fci-full-update ()
  "Redraw the fill-column rule in all windows on this buffer."
  (remove-hook 'post-command-hook #'fci-full-update t)
  (overlay-recenter (point-max))
  (setq fci-buffer-windows 
        (get-buffer-window-list (current-buffer) 'no-minibuf t))
  (fci-delete-unneeded)
  (let (start end)
    (fci-sanitize-actions
     ;; If some windows on this buffer overlap, we end up redrawing the
     ;; overlap once for each window, but the cost of doing so is less than
     ;; the cost of trying to avoid such redrawing.
     (dolist (win fci-buffer-windows)
       (setq start (window-start win)
             end (window-end win t))
       (fci-delete-overlays-region start end)
       (fci-put-overlays-region start end)))))

(defun fci-update-window-for-scroll (win start)
  "Redraw the fill-column rule in WIN after it has been been scrolled."
  (fci-sanitize-actions
   (fci-delete-unneeded)
   (let ((end (window-end win t)))
     (fci-delete-overlays-region start end)
     (fci-put-overlays-region start end))))

(defun fci-delete-overlays-buffer ()
  "Delete all overlays displaying the fill-column rule in the current buffer."
  (fci-delete-overlays-region (point-min) (point-max)))

;;; ---------------------------------------------------------------------
;;; Workarounds
;;; ---------------------------------------------------------------------

;; This in placed in post-command-hook and does four things:
;; 1. If the display table has been deleted or something has changed the
;;    display table for newline chars, then we need to regenerate overlay
;;    strings after reprocessing the display table.
;; 2. If default char width or height has changed, we regenerate the
;;    rule.  (This handles both font changes and also cases where we activate
;;    the mode while displaying on a char terminal then subsequently display
;;    the buffer on a window frame.)
;; 3. If the value of `tab-width' or `fill-column' has changed, we need to
;;    reset the rule.  (We could set things up so that the rule adjusted
;;    automatically to such changes, but (a) it's slower, and (b) it wouldn't
;;    work on v22 or v23.)  
;; 4. Cursor properties are ignored when they're out of sight because of
;;    horizontal scrolling.  We need to detect such situations and force a
;;    return from hscrolling to bring our requested cursor position back into
;;    view.
(defun fci-post-command-check ()
  (cond
   ((not (and buffer-display-table
              (equal (aref buffer-display-table 10) fci-newline-sentinel)))
    (setq fci-display-table-processed nil)
    (fci-mode 1))
   ((and (< 1 (frame-char-width))
         (not (and (= (frame-char-width) fci-char-width)
                   (= (frame-char-height) fci-char-height))))
    (setq fci-char-width nil)
    (fci-mode 1))
   ((not (and (= fill-column fci-column)
              (= tab-width fci-tab-width)))
    (fci-full-update))
   ((and (< 0 (window-hscroll))
         auto-hscroll-mode
         (<= (current-column) (window-hscroll)))
    ;; Fix me:  Rather than setting hscroll to 0, this should reproduce the
    ;; relevant part of the auto-hscrolling algorithm.  Most people won't
    ;; notice the difference in behavior, though.
    (set-window-hscroll (selected-window) 0))))

(provide 'fill-column-indicator)

;;; fill-column-indicator.el ends here
