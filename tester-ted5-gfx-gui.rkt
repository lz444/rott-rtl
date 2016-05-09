#lang racket/gui

(require framework)
(require "ted5-gfx.rkt")

;; Display TED5 tiles

;; Paths to the various graphics files
(define rott-ted5-files
  (list
    (list
      (expand-user-path "~/dosbox_apps/ted5/SAMPLMAP/GFXINFOE.SAM")
      (expand-user-path "~/dosbox_apps/ted5/SAMPLMAP/EGAHEAD.SAM")
      (expand-user-path "~/dosbox_apps/ted5/SAMPLMAP/EGAGRAPH.SAM")
      (expand-user-path "~/dosbox_apps/ted5/SAMPLMAP/EGADICT.SAM"))))

(define bmenace-ted5-files
  (list
    (list
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/GFXINFOE.BM1")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGAHEAD.BM1")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGAGRAPH.BM1")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGADICT.BM1"))
    (list
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/GFXINFOE.BM2")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGAHEAD.BM2")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGAGRAPH.BM2")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGADICT.BM2"))
    (list
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/GFXINFOE.BM3")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGAHEAD.BM3")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGAGRAPH.BM3")
      (expand-user-path "~/dosbox_apps/apogeerg/bmfreew/EGADICT.BM3"))))

;; Remember to change num-info-tiles if you're going to try different games.
(define ted5-files (car rott-ted5-files))
(define gfxinfoe-path (car ted5-files))
(define egahead-path (cadr ted5-files))
(define egagraph-path (caddr ted5-files))
(define egadict-path (cadddr ted5-files))

;; TED5 constants (may need to change for different games)
(define rott-info (* 1 18))
(define bmenace-info (* 5 18))
(define num-info-tiles rott-info)

;; Load in the graphics
(define background-tiles (read-background-tiles-ega16 gfxinfoe-path egahead-path egagraph-path egadict-path))
(define masked-tiles (read-masked-tiles-ega16 gfxinfoe-path egahead-path egagraph-path egadict-path))

;; Some GUI constants
(define tile-textbox-length 55)
(define tile-pane-spacing 10)
(define combined-tiles-spacing 15)
(define max-tile-display-size 64)
(define b-canvas-bg (send the-color-database find-color "white"))
(define f-canvas-bg (send the-color-database find-color "thistle"))
(define i-canvas-bg (send the-color-database find-color "lightcyan"))
(define b-notile-bg (send the-color-database find-color "black"))
(define b-notile-fg (send the-color-database find-color "white"))
(define b-notile-alpha 1.0)
(define f-notile-bg (send the-color-database find-color "lightpink"))
(define f-notile-fg (send the-color-database find-color "black"))
(define f-notile-alpha 0.85)
(define i-notile-bg (send the-color-database find-color "white"))
(define i-notile-fg (send the-color-database find-color "black"))
(define i-notile-alpha 0.85)

;; Functions for refreshing the window
;; Fills the canvas with the given color
(define (erase-canvas canvas color)
  (define dc (send canvas get-dc))
  (send dc clear)
  (send dc set-scale 1 1)
  (send dc set-brush color 'solid)
  (send dc set-pen "white" 0 'transparent)
  (define-values (maxx maxy) (send canvas get-client-size))
  (send dc draw-rectangle 0 0 maxx maxy))

;; Takes which type of tile it is ('b, 'f 'i), the value to display, the canvas
;; to display into, x y coordinates to display the tile, and a vector
;; containing the tile bitmaps then displays the given tile from the bitmap
;; vector, or a hex code if the tile is not used.
(define (display-tile type val canvas x y tiles)
  (cond
    ((eq? 'b type)
     ;; Tile exceeds the maximum tile length or is not defined, display as hex value
     (if (or (>= val (vector-length tiles)) (not (vector-ref tiles val)))
       (paint-hex canvas x y val b-notile-fg b-notile-bg b-notile-alpha (get-current-scaling))
       (paint-tile canvas x y (vector-ref tiles val) (get-current-scaling))))
    ((eq? 'f type)
     ;; Tile is not zero and is outside the range of foregound tiles, or not defined,
     ;; then display as hex value
     (if (and (not (zero? val))
              (or (>= val (vector-length tiles))
                  (< val num-info-tiles)
                  (not (vector-ref tiles val))))
       (paint-hex canvas x y val f-notile-fg f-notile-bg f-notile-alpha (get-current-scaling))
       (paint-tile canvas x y (vector-ref tiles val) (get-current-scaling))))
    ((eq? 'i type)
     ;; Tile is not zero and is outside the range of info tiles, or not defined,
     ;; then display as hex value
     (if (and (not (zero? val))
              (or (>= val num-info-tiles) (not (vector-ref tiles val))))
       (paint-hex canvas x y val i-notile-fg i-notile-bg i-notile-alpha (get-current-scaling))
       (paint-tile canvas x y (vector-ref tiles val) (get-current-scaling))))))


;; Turns a typed string to a value, or #f if it is not a value
;; Prefix a $ for a hex number "$abcd"
(define (str->val s)
    (cond
      ((zero? (string-length s))
       #f)
      ((char=? (string-ref s 0) #\$)
       (string->number (substring s 1) 16))
      (else
        (string->number s))))

;; Gets the current scaling from the scaling radio-box
(define (get-current-scaling)
  (add1 (send size-selector get-selection)))

;; Refreshes the tile display & tile values
(define (refresh-individual-tiles)
  ;; Pull new values from the textfileds
  (define new-b (str->val (send bg-selector get-value)))
  (define new-f (str->val (send fg-selector get-value)))
  (define new-i (str->val (send info-selector get-value)))
  ;; If a valid number was entered, then update the tiles & value. Otherwise just let the old tile stay
  (if new-b
    (begin
      (erase-canvas bg-tile b-canvas-bg)
      (display-tile 'b new-b bg-tile 0 0 background-tiles)
      (send bg-tile-num set-label (~r new-b))
      (send bg-tile-num-hex set-label (string-append "$" (~r new-b #:base '(up 16) #:min-width 4 #:pad-string "0"))))
    (void))
  (if new-f
    (begin
      (erase-canvas fg-tile f-canvas-bg)
      (display-tile 'f new-f fg-tile 0 0 masked-tiles)
      (send fg-tile-num set-label (~r new-f))
      (send fg-tile-num-hex set-label (string-append "$" (~r new-f #:base '(up 16) #:min-width 4 #:pad-string "0"))))
    (void))
  (if new-i
    (begin
      (erase-canvas info-tile i-canvas-bg)
      (display-tile 'i new-i info-tile 0 0 masked-tiles)
      (send info-tile-num set-label (~r new-i))
      (send info-tile-num-hex set-label (string-append "$" (~r new-i #:base '(up 16) #:min-width 4 #:pad-string "0"))))
    (void)))

;; Refreshes the combined tile display
(define (refresh-combined-tiles)
  ;; Pull old values from the labels
  (define old-b (str->val (send bg-tile-num get-label)))
  (define old-f (str->val (send fg-tile-num get-label)))
  (define old-i (str->val (send info-tile-num get-label)))
  ;; Update B, F, I if they are checked
  (send (send combined-tile get-dc) clear)
  (if (send show-bg? get-value)
    (display-tile 'b old-b combined-tile 0 0 background-tiles)
    (void))
  (if (send show-fg? get-value)
    (display-tile 'f old-f combined-tile 0 0 masked-tiles)
    (void))
  (if (send show-info? get-value)
    (display-tile 'i old-i combined-tile 0 0 masked-tiles)
    (void)))

;; Define a GUI window
(application:current-app-name "TED5 Tiles")
(define mainwindow (new frame%
                        [label "TED5 Tiles"]
                        [width 250]
                        [height 350]))
(define layout (new vertical-pane%
                    [parent mainwindow]))
(define displayarea (new vertical-pane%
                         [parent layout]
                         [stretchable-width #f]
                         [stretchable-height #f]))
(define layout-padding (new pane%
                            [parent layout]))
(define control-halves (new horizontal-pane%
                            [parent layout]
                            [alignment '(left top)]
                            [stretchable-height #f]))

;; Control widgets
(define size-selector (new radio-box%
                           [label "Display Size: "]
                           [choices '("16" "32" "48" "64")]
                           [parent control-halves]
                           [selection 3]
                           [style '(vertical vertical-label)]
                           [callback (λ (rad event)
                                        (refresh-individual-tiles)
                                        (refresh-combined-tiles))]))
(define controls-padding (new panel%
                              [parent control-halves]))
(define control-layout (new vertical-pane%
                            [parent control-halves]
                            [alignment '(right top)]
                            [min-height 50]
                            [stretchable-height #f]))
(define bg-panel (new horizontal-panel%
                      [parent control-layout]
                      [stretchable-width #f]))
(define bg-label (new message%
                      [label "Background:"]
                      [parent bg-panel]))
(define bg-selector (new text-field%
                         [label #f]
                         [parent bg-panel]
                         [init-value "0"]
                         [min-width tile-textbox-length]
                         [stretchable-width #f]))
(define fg-panel (new horizontal-panel%
                      [parent control-layout]
                      [stretchable-width #f]))
(define fg-label (new message%
                      [label "Foreground:"]
                      [parent fg-panel]))
(define fg-selector (new text-field%
                         [label #f]
                         [parent fg-panel]
                         [init-value "0"]
                         [min-width tile-textbox-length]
                         [stretchable-width #f]))
(define info-panel (new horizontal-panel%
                        [parent control-layout]
                        [stretchable-width #f]))
(define info-label (new message%
                        [label "Info:"]
                        [parent info-panel]))
(define info-selector (new text-field%
                           [label #f]
                           [parent info-panel]
                           [init-value "0"]
                           [min-width tile-textbox-length]
                           [stretchable-width #f]))
(define get-button (new button%
                        [label "Get Tile"]
                        [parent control-layout]
                        [callback (λ (button event)
                                     (refresh-individual-tiles)
                                     (refresh-combined-tiles))]))

;; Tile display
(define individual-tiles (new horizontal-pane%
                              [parent displayarea]
                              [alignment '(center center)]
                              [stretchable-width #f]))
(define combined-tiles-area (new vertical-pane%
                            [parent displayarea]
                            [alignment '(center center)]
                            [vert-margin combined-tiles-spacing]
                            [stretchable-width #f]))
(define bg-area (new vertical-pane%
                     [parent individual-tiles]
                     [alignment '(center top)]
                     [horiz-margin tile-pane-spacing]
                     [stretchable-width #f]
                     [stretchable-height #f]))
(define bg-tile-msg (new message%
                         [label "B"]
                         [parent bg-area]))
(define bg-tile (new canvas%
                     [parent bg-area]
                     [style '(no-focus)]
                     [min-width max-tile-display-size]
                     [min-height max-tile-display-size]
                     [stretchable-width #f]
                     [stretchable-height #f]))
(define bg-tile-num-hex (new message%
                             [label "$0"]
                             [parent bg-area]
                             [stretchable-width #t]))
(define bg-tile-num (new message%
                         [label "0"]
                         [parent bg-area]
                         [stretchable-width #t]))
(define fg-area (new vertical-pane%
                     [parent individual-tiles]
                     [alignment '(center top)]
                     [horiz-margin tile-pane-spacing]
                     [stretchable-width #f]
                     [stretchable-height #f]))
(define fg-tile-msg (new message%
                         [label "F"]
                         [parent fg-area]))
(define fg-tile (new canvas%
                     [parent fg-area]
                     [style '(no-focus)]
                     [min-width max-tile-display-size]
                     [min-height max-tile-display-size]
                     [stretchable-width #f]
                     [stretchable-height #f]))
(define fg-tile-num-hex (new message%
                             [label "$0"]
                             [parent fg-area]
                             [stretchable-width #t]))
(define fg-tile-num (new message%
                         [label "0"]
                         [parent fg-area]
                         [stretchable-width #t]))
(define info-area (new vertical-pane%
                       [parent individual-tiles]
                       [alignment '(center top)]
                       [horiz-margin tile-pane-spacing]
                       [stretchable-width #f]
                       [stretchable-height #f]))
(define info-tile-msg (new message%
                           [label "I"]
                           [parent info-area]))
(define info-tile (new canvas%
                       [parent info-area]
                       [style '(no-focus)]
                       [min-width max-tile-display-size]
                       [min-height max-tile-display-size]
                       [stretchable-width #f]
                       [stretchable-height #f]))
(define info-tile-num-hex (new message%
                               [label "$0"]
                               [parent info-area]
                               [stretchable-width #t]))
(define info-tile-num (new message%
                           [label "0"]
                           [parent info-area]
                           [stretchable-width #t]))

(define combined-tile (new canvas%
                           [parent combined-tiles-area]
                           [style '(no-focus)]
                           [min-width max-tile-display-size]
                           [min-height max-tile-display-size]
                           [stretchable-width #f]
                           [stretchable-height #f]))
(define combined-controls (new horizontal-pane%
                               [parent combined-tiles-area]
                               [stretchable-width #f]
                               [stretchable-height #f]))
;; Place the labels for each checkbox below the checkbox
(define show-bg-controls (new vertical-pane%
                              [parent combined-controls]
                              [alignment '(center top)]
                              [stretchable-width #f]
                              [stretchable-height #f]))
(define show-bg? (new check-box%
                      [label ""]
                      [value #t]
                      [parent show-bg-controls]
                      [callback (λ (chk event)
                                   (refresh-combined-tiles))]))
(define show-bg-label (new message%
                           [label "B"]
                           [parent show-bg-controls]))
(define show-fg-controls (new vertical-pane%
                              [parent combined-controls]
                              [stretchable-width #f]
                              [stretchable-height #f]))
(define show-fg? (new check-box%
                      [label ""]
                      [value #t]
                      [parent show-fg-controls]
                      [callback (λ (chk event)
                                   (refresh-combined-tiles))]))
(define show-fg-label (new message%
                           [label "F"]
                           [parent show-fg-controls]))
(define show-info-controls (new vertical-pane%
                                [parent combined-controls]
                                [stretchable-width #f]
                                [stretchable-height #f]))
(define show-info? (new check-box%
                        [label ""]
                        [value #t]
                        [parent show-info-controls]
                        [callback (λ (chk event)
                                     (refresh-combined-tiles))]))
(define show-info-label (new message%
                             [label "I"]
                             [parent show-info-controls]))

(send mainwindow show #t)

; vim: expandtab:sw=2
