#lang racket/gui

(require framework)
(require "ted5-gfx.rkt")
(require "rtl.rkt")

;; A viewer for ROTT RTL/RTC maps
;; Requires the EGADICT, EGAGRAPH, EGAHEAD, GFXINFOE files from TED5.

;; These were taken from the sample map distributed with TED5
(define egadict-path "graphics/EGADICT.SAM")
(define egagraph-path "graphics/EGAGRAPH.SAM")
(define egahead-path "graphics/EGAHEAD.SAM")
(define gfxinfoe-path "graphics/GFXINFOE.SAM")

;; The first 18 masked tiles are INFO tiles
(define num-info-tiles 18)

;; 16x16 tile size
(define base-tile-size 16)

;; Load in the graphics
(define background-tiles (read-background-tiles-ega16 gfxinfoe-path egahead-path egagraph-path egadict-path))
(define masked-tiles (read-masked-tiles-ega16 gfxinfoe-path egahead-path egagraph-path egadict-path))

;; Extended class for the tiles display
;; Displays the mouse cursor's position as a tile location
;; Scroll by arrow keys & mousewheel movements
;; Adjust zoom with +/- keys
(define tiles-canvas%
  (class canvas%
    (super-new)
    (define/override
      (on-event evt)
      (unless (null? current-type)
        ;;(when (send evt button-down? 'middle) (bell)) ;; Just testing mousebuttons
        (if (send evt leaving?)
          (begin
            (send coords-dec-x set-label (string-append coords-x-label coords-dec-unknown))
            (send coords-dec-y set-label (string-append coords-y-label coords-dec-unknown))
            (send coords-hex-x set-label (string-append coords-hex-prefix coords-hex-unknown))
            (send coords-hex-y set-label (string-append coords-hex-prefix coords-hex-unknown)))
        (let*-values ([(start-x start-y) (send this get-view-start)]
                      [(offset-x offset-y) (values (+ (send evt get-x) start-x) (+ (send evt get-y) start-y))]
                      [(tile-x tile-y) (canvas-location->tile-location offset-x offset-y)])
          (send coords-dec-x set-label (string-append coords-x-label (~r tile-x)))
          (send coords-dec-y set-label (string-append coords-y-label (~r tile-y)))
          (send coords-hex-x set-label (string-append coords-hex-prefix (~r tile-x #:base '(up 16) #:min-width 2 #:pad-string "0")))
          (send coords-hex-y set-label (string-append coords-hex-prefix (~r tile-y #:base '(up 16) #:min-width 2 #:pad-string "0"))))))
      #t)
    (define/override
      (on-char evt)
      (define keycode (send evt get-key-code))
      (define ctrl-down? (send evt get-control-down))
      (cond
        ((null? current-type) #f)
        ((or (and (not ctrl-down?) (eq? keycode 'left)) (eq? keycode 'wheel-left))
         (scroll-by-tiles (- scroll-step) 0)
         #t)
        ((or (and (not ctrl-down?) (eq? keycode 'right)) (eq? keycode 'wheel-right))
         (scroll-by-tiles scroll-step 0)
         #t)
        ((or (and (not ctrl-down?) (eq? keycode 'up)) (eq? keycode 'wheel-up))
         (scroll-by-tiles 0 (- scroll-step))
         #t)
        ((or (and (not ctrl-down?) (eq? keycode 'down)) (eq? keycode 'wheel-down))
         (scroll-by-tiles 0 scroll-step)
         #t)
        ((and ctrl-down? (eq? keycode 'left))
         (scroll-by-tiles 'screen- 0)
         #t)
        ((and ctrl-down? (eq? keycode 'right))
         (scroll-by-tiles 'screen+ 0)
         #t)
        ((and ctrl-down? (eq? keycode 'up))
         (scroll-by-tiles 0 'screen-)
         #t)
        ((and ctrl-down? (eq? keycode' down))
         (scroll-by-tiles 0 'screen+)
         #t)
        ((or (eq? keycode #\_) (eq? keycode #\-) (eq? keycode 'subtract))
         (unless (= min-scaling current-scaling)
           (set! current-scaling (sub1 current-scaling))
           (send size-selector set-value current-scaling)
           (update-size)
           ;; NOTE we should readjust the scrollbars before generating the map bitmap,
           ;; otherwise sometimes the map display is incorrect.
           (readjust-displayarea-scrollbars #f)
           (generate-map-bitmap #t))
         #t)
        ((or (eq? keycode #\+) (eq? keycode #\=) (eq? keycode 'add))
         (unless (= max-scaling current-scaling)
           (set! current-scaling (add1 current-scaling))
           (send size-selector set-value current-scaling)
           (update-size)
           (readjust-displayarea-scrollbars #f)
           (generate-map-bitmap #t))
         #t)
        (else
          #f)))))

;; Some GUI constants
(define scroll-step 2) ;; Original TED5 scroll step
(define appname "ROTT Viewer")
(define appwidth 600)
(define appheight 500)
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
(define open-file-button-msg "Open File")
(define type-rtl-msg "Single Player")
(define type-rtc-msg "Comm-Bat™")
(define type-rtr-msg "RandROTT")
(define wait-msg "Loading map, please wait...")
(define type-unknown-msg "")
(define no-file-loaded-msg "(no file loaded)")
(define loaded-map-msg "")
(define size-slider-msg "Tile Size:")
(define b-label-msg "B")
(define f-label-msg "F")
(define i-label-msg "I")
(define min-scaling 1)
(define max-scaling 8)
(define size-slider-minwidth 180)
(define filename-minwidth 110)
(define tile-size-minwidth 40)
(define coords-x-label "X= ")
(define coords-y-label "Y= ")
(define coords-dec-unknown "???")
(define coords-hex-unknown "??")
(define coords-hex-prefix "$")
(define coords-dec-minwidth 40)
(define coords-hex-minwidth 40)
;; Use these TED tiles as previous/next icons. Saves me the bother of drawing
;; my own icons!
(define prev-button-msg (vector-ref masked-tiles 76))
(define next-button-msg (vector-ref masked-tiles 72))

;; Global variables to hold state
(define loaded-path "")           ;; Loaded file
(define current-scaling 2)        ;; Display size of tiles, multiplied by base-tile-size
(define current-type '())         ;; Will be 'rtl, 'rtc, 'rtr, or '() for no file loaded
(define current-map-list '())     ;; All the levels in the RTL/RTC file
(define current-map-num 0)        ;; Which map is currently loaded, starting from 0
(define current-map-header '())   ;; Map header information
(define current-map-data '())     ;; Map data information
(define current-map-bitmap '())   ;; We draw into this bitmap then display the bitmap onto the canvas
(define map-bitmap-offset-x 0)    ;; Defines quadrants in the bitmap for wraparound fast scrolling
(define map-bitmap-offset-y 0)
(define visible-tiles-start-x -1) ;; Which tiles are currently visible
(define visible-tiles-start-y -1)
(define visible-tiles-end-x -1)
(define visible-tiles-end-y -1)

;; Functions for refreshing the window
;; Draws a blank tile on the given location
(define (erase-tile color bitmap x y)
  (define dc (send bitmap make-dc))
  (send dc set-scale 1 1)
  (send dc set-brush color 'solid)
  (send dc set-pen "white" 0 'transparent)
  (define-values (maxx maxy) (values (* base-tile-size current-scaling) (* base-tile-size current-scaling)))
  (send dc draw-rectangle x y maxx maxy))

;; Takes which type of tile it is ('b, 'f 'i), the value to display, the
;; scaling, the bitmap to display into, x y coordinates to display the tile,
;; and a vector containing the tile bitmaps then displays the given tile from
;; the bitmap vector, or a hex code if the tile is not used.
(define (display-tile type val scaling bitmap x y tiles)
  (cond
    ((eq? 'b type)
     ;; Tile exceeds the maximum tile length or is not defined, display as hex value
     (if (or (>= val (vector-length tiles)) (not (vector-ref tiles val)))
       (paint-hex bitmap x y val b-notile-fg b-notile-bg b-notile-alpha scaling)
       (paint-tile bitmap x y (vector-ref tiles val) scaling)))
    ((and (eq? 'f type) (not (zero? val)))
     ;; Optimization: Since foreground & info planes are mostly zero, only call
     ;; paint methods if the value is nonzero.
     ;; Tile is not zero and is outside the range of foregound tiles, or not defined,
     ;; then display as hex value
     (if (or (>= val (vector-length tiles))
             (< val num-info-tiles)
             (not (vector-ref tiles val)))
       (paint-hex bitmap x y val f-notile-fg f-notile-bg f-notile-alpha scaling)
       (paint-tile bitmap x y (vector-ref tiles val) scaling)))
    ((and (eq? 'i type) (not (zero? val)))
     ;; Tile is not zero and is outside the range of info tiles, or not defined,
     ;; then display as hex value
     (if (or (>= val num-info-tiles) (not (vector-ref tiles val)))
       (paint-hex bitmap x y val i-notile-fg i-notile-bg i-notile-alpha scaling)
       (paint-tile bitmap x y (vector-ref tiles val) scaling)))))

;; Updates the filename display
(define (update-filename-message)
  (define-values (dir name root?) (split-path loaded-path))
  (send filename-message set-label (path->string name))
  (cond
    ((eq? current-type 'rtl)
     (send filetype-message set-label type-rtl-msg))
    ((eq? current-type 'rtc)
     (send filetype-message set-label type-rtc-msg))
    ((eq? current-type 'rtr)
     (send filetype-message set-label type-rtr-msg))
    (else
      (send filetype-message set-label type-unknown-msg))))

;; Update the map choicebox
(define (update-map-choices)
  (send map-names clear)
  (for-each (λ (n) (send map-names append (~a (car n) ": " (cadr n)))) current-map-list))

;; Update the size label
(define (update-size)
  (send size-label set-label (number->string (* (send size-selector get-value) base-tile-size))))

;; Prompts the user for an RTL/RTC file
(define (open-file)
  (define file (finder:get-file))
  ;; Load in the file if the user didn't click cancel
  (when file
    (set! loaded-path file)
    (if (read-signature)
      (begin
        (update-filename-message)
        (update-map-choices)
        (readjust-displayarea-scrollbars #t)
        (goto-map 'first)
        (send displayarea focus))
      ;; If the file is not valid, ask user for another file
      (open-file))))

;; Reads in the signature from the loaded file. If the signature is not correct
;; display an error dialog box and return #f. Otherwise return #t if the file
;; signature is correct. Also build the file list and set the file type variables.
(define (read-signature)
  (define inport (open-input-file loaded-path #:mode 'binary))
  (define signature (read-rtl-signature inport))
  (define version (read-rtl-version inport))
  (define retval
    (cond 
      ((not (or (bytes=? signature rtl-signature) (bytes=? signature rtc-signature) (bytes=? signature rtr-signature)))
       (message-box "File Error" "This is not an RTL/RTC/RTR file." #f '(stop ok))
       #f)
      ((not (bytes=? version rtl-version-1.01))
       (message-box "File Error" "RTL version is not 1.01." #f '(stop ok))
       #f)
      (else
        ;; Update the map list
        (define full-names (list-mapnames inport))
        (define numbers (build-list num-header-offsets values))
        (define full-names-with-numbers
          (map
            (λ (number name)
               (list number name))
            numbers
            full-names))
        (set! current-map-list (filter cadr full-names-with-numbers))
        #t)))
  (close-input-port inport)
  ;; Update the type
  (when retval
    (cond
      ((bytes=? signature rtl-signature)
       (set! current-type 'rtl))
      ((bytes=? signature rtc-signature)
       (set! current-type 'rtc))
      ((bytes=? signature rtr-signature)
       (set! current-type 'rtr))))
  retval)

;; Reads in the header for the given map, then uncompresses the map data.
;; If map num is 'first, then loads the first map from current-map-list
;; If map is 'previous or 'next, then loads the previous or next map from
;; current-map-list. If we're already at one end of the map list, then just
;; stay at whichever end we're on.
;; If mapnum is a number, than loads the given map starting with 0.
;; Does *not* check if the map is used.
;; Refreshes the map name, and sets current-map-header, current-map-data,
;; current-map-num appropriately.
(define (goto-map mapnum)
  ;; This is harder than just blinding adding or subtracting 1 to n, but this
  ;; should handle the case where an RTL file has maps not defined contiguously,
  ;; although I have never seen such a file.
  ;; UPDATE: The RANDROTT.RTR file _does_ have non-contiguous maps! This code
  ;; handles it no problem at all.
  (define n
    (cond
      ((eq? 'first mapnum)
       (send map-names set-selection 0)
       (car (car current-map-list)))
      ((eq? 'previous mapnum)
       (if (> current-map-num (car (car current-map-list)))
         (begin
           (send map-names set-selection (sub1 (send map-names get-selection)))
           (car (car (memf (λ (n) (< (car n) current-map-num)) (reverse current-map-list)))))
         '()))
      ((eq? 'next mapnum)
       (if (< current-map-num (car (car (reverse current-map-list))))
         (begin
           (send map-names set-selection (add1 (send map-names get-selection)))
           (car (car (memf (λ (n) (> (car n) current-map-num)) current-map-list))))
         '()))
      (else mapnum)))
  (unless (null? n)
    (let ([inport (open-input-file loaded-path #:mode 'binary)])
      (send filetype-message set-label wait-msg)
      (set! current-map-header (read-rtl-header inport n))
      (set! current-map-data (read-map-planes inport current-map-header))
      (set! current-map-num n)
      (close-input-port inport)
      (update-filename-message)
      (generate-map-bitmap #t)
      (send displayarea refresh))))

;; Callback for when the user selects a map from the choice
(define (goto-map-from-choice ch ev)
  (unless (null? current-type)
    (let* ([index (send ch get-selection)]
           [mapnum (car (list-ref current-map-list index))])
      (goto-map mapnum))))

;; Readjusts the displayarea's scrollbars
(define (readjust-displayarea-scrollbars re-origin?)
  (define tile-size (* base-tile-size current-scaling))
  (define-values (scrollx scrolly)
    (if re-origin?
      (begin
        ;; Gotta reset the offset too when we reset the scrollbars
        (set!-values (map-bitmap-offset-x map-bitmap-offset-y) (values 0 0))
        (values 0 0))
      (let-values ([(orig-x-st orig-y-st) (send displayarea get-view-start)]
                   [(orig-w-vis orig-h-vis) (send displayarea get-client-size)]
                   [(orig-w-full orig-h-full) (send displayarea get-virtual-size)])
        (values (/ orig-x-st (- orig-w-full orig-w-vis)) (/ orig-y-st (- orig-h-full orig-h-vis))))))
  (send displayarea init-auto-scrollbars (* tile-size map-width) (* tile-size map-height) scrollx scrolly))

;; Adjust scrolling by tiles
(define (scroll-by-tiles x y)
  (define-values (orig-x-st orig-y-st) (send displayarea get-view-start))
  (define-values (orig-w-vis orig-h-vis) (send displayarea get-client-size))
  (define-values (orig-w-full orig-h-full) (send displayarea get-virtual-size))
  (define-values (tiles-start-x tiles-end-x tiles-start-y tiles-end-y) (visible-tiles-on-canvas displayarea))
  (define-values (vis-tiles-x vis-tiles-y) (values (- tiles-end-x tiles-start-x 1) (- tiles-end-y tiles-start-y 1)))
  (define screened-x
    (cond
      ((eq? 'screen+ x)
       vis-tiles-x)
      ((eq? 'screen- x)
       (- vis-tiles-x))
      (else x)))
  (define screened-y
    (cond
      ((eq? 'screen+ y)
       vis-tiles-y)
      ((eq? 'screen- y)
       (- vis-tiles-y))
      (else y)))
  (define scroll-x
    (if (not (zero? screened-x))
      (let ([new-x (/ (+ (* screened-x base-tile-size current-scaling) orig-x-st) (- orig-w-full orig-w-vis))])
        (cond
          ((> new-x 1.0) 1.0)
          ((< new-x 0.0) 0.0)
          (else new-x)))
      #f))
  (define scroll-y
    (if (not (zero? screened-y))
      (let ([new-y (/ (+ (* screened-y base-tile-size current-scaling) orig-y-st) (- orig-h-full orig-h-vis))])
        (cond
          ((> new-y 1.0) 1.0)
          ((< new-y 0.0) 0.0)
          (else new-y)))
      #f))
  (send displayarea scroll scroll-x scroll-y))

;; Helper function to convert virtual canvas pixels to tile locations
(define (canvas-location->tile-location x y)
  (define tile-size (* base-tile-size current-scaling))
  (values (floor (/ x tile-size)) (floor (/ y tile-size))))

;; Helper function to convert tile locations to virtual scaled canvas pixels
(define (tile-location->canvas-location x y)
  (values (* x base-tile-size current-scaling) (* y base-tile-size current-scaling)))

;; Helper function to determine the visible tiles on a canvas
(define (visible-tiles-on-canvas canvas)
  (define-values (canvas-start-x canvas-start-y) (send canvas get-view-start))
  (define-values (tiles-start-x tiles-start-y) (canvas-location->tile-location canvas-start-x canvas-start-y))
  (define-values (canvas-vis-x canvas-vis-y) (send canvas get-client-size))
  (define-values (canvas-end-x canvas-end-y) (values (+ canvas-start-x canvas-vis-x) (+ canvas-start-y canvas-vis-y)))
  (define-values (tiles-end-x tiles-end-y) (canvas-location->tile-location canvas-end-x canvas-end-y))
  (define-values (extra-x extra-y)
    (values (remainder canvas-vis-x (* base-tile-size current-scaling)) (remainder canvas-vis-y (* base-tile-size current-scaling))))
  (define-values (tile00x tile00y) (tile-location->canvas-location tiles-start-x tiles-start-y))
  (define-values (notvis00x notvis00y) (values (- canvas-start-x tile00x) (- canvas-start-y tile00y)))
  ;; Usually there is one extra tile past the current canvas size, unless we're
  ;; at an edge of the map. We also store another extra tile to prevent
  ;; needless resizing as the canvas is scrolled.
  (define-values (tiles-addl-x tiles-addl-y)
    (values (if (<= (- (* base-tile-size current-scaling) notvis00x) extra-x) (add1 tiles-end-x) (add1 (add1 tiles-end-x)))
            (if (<= (- (* base-tile-size current-scaling) notvis00y) extra-y) (add1 tiles-end-y) (add1 (add1 tiles-end-y)))))
  ;; Note that we could return more than map-width or map-height if the canvas
  ;; is scrolled to the edge. Other functions which use these results should
  ;; constrain to <= 127 if necessary. This was done so the bitmap wouldn't be
  ;; needlessly regenerated when scrolling to the right/bottom edge of the map.
  (values tiles-start-x tiles-addl-x tiles-start-y tiles-addl-y))

;; Helper function to draw all three planes onto the bitmap
;(define (erase-tile color bitmap x y)
(define (draw-three-planes mapx mapy bitx bity)
  (if (send b-checkbox get-value)
    (let ([tile-val (tile-at mapx mapy (vector-ref current-map-data walls-plane))])
      (display-tile 'b (integer-bytes->integer tile-val #f #f) current-scaling current-map-bitmap bitx bity background-tiles))
    ;; If the background plane is off, need to draw a blank in the current location
    ;; otherwise the F/I tiles won't be erased when the canvas is scrolled
    (erase-tile b-canvas-bg current-map-bitmap bitx bity))
  (when (send f-checkbox get-value)
    (define tile-val (tile-at mapx mapy (vector-ref current-map-data sprites-plane)))
    (display-tile 'f (integer-bytes->integer tile-val #f #f) current-scaling current-map-bitmap bitx bity masked-tiles))
  (when (send i-checkbox get-value)
    (define tile-val (tile-at mapx mapy (vector-ref current-map-data info-plane)))
    (display-tile 'i (integer-bytes->integer tile-val #f #f) current-scaling current-map-bitmap bitx bity masked-tiles)))

;; Generates the visible area as a bitmap
;; Problem: Scrolling the map is VERY SLOW because painting tiles is slow.
;; Solution: Only draw the tiles that need to be redrawn. This is accomplished
;; by using a bitmap wraparound technique.
;;     Bitmap                     Canvas
;; -------------------       -------------------
;; |    4       | 3  |       | 1  |    2       |
;; |            |    |  ==>  |----+------------|
;; |------------+----|  ==>  | 3  |    4       |
;; |    2       | 1  |       |    |            |
;; -------------------       -------------------
;; We store an offset which defines the quadrants in the bitmap.
;; When we scroll, we paint the new tiles over the tiles not needed anymore
;; in the bitmap and adjust the offset.
;; If we reach the right or bottom edge while painting into the bitmap or
;; adjusting the offset we wraparound to the left or top edge of the bitmap.
;; When we paint onto the canvas we paint from each quadrant in the right
;; order.
;; This seems like such a simple and elegant solution that surely this must
;; have been done before.
(define (generate-map-bitmap full-map?)
  (define-values (tiles-start-x tiles-end-x tiles-start-y tiles-end-y) (visible-tiles-on-canvas displayarea))
  (define-values (new-visible-x new-visible-y) (values (- tiles-end-x tiles-start-x) (- tiles-end-y tiles-start-y)))
  (define-values (old-visible-x old-visible-y) (values (- visible-tiles-end-x visible-tiles-start-x) (- visible-tiles-end-y visible-tiles-start-y)))
  ;; Genereate new bitmap size if the amount of visible tiles has changed
  (define resized?
    (if (not (and (= new-visible-x old-visible-x) (= new-visible-y old-visible-y)))
      (begin
        ;;(bell) Debugging beep to let me know when the map was resized
        (set! current-map-bitmap (make-bitmap (* new-visible-x current-scaling base-tile-size) (* new-visible-y current-scaling base-tile-size) #f))
        #t)
      #f))
  ;; Calculate how may tiles that have been scrolled
  (define-values (tiles-changed-x tiles-changed-y) (values (- tiles-start-x visible-tiles-start-x) (- tiles-start-y visible-tiles-start-y)))
  ;; Draw the tiles onto the bitmap
  (define dc (send current-map-bitmap make-dc))
  (cond
    ;; Need to redraw the whole map when:
    ;; * Map was resized
    ;; * full-map? was set
    ;; * Or we have scrolled more tiles than are on the screen
    ((or resized? full-map? (> (abs tiles-changed-x) old-visible-x) (> (abs tiles-changed-y) old-visible-y))
     (send dc clear)
     (set!-values (map-bitmap-offset-x map-bitmap-offset-y) (values 0 0))
     (for* ([y (in-range tiles-start-y (min tiles-end-y map-height))]
            [x (in-range tiles-start-x (min tiles-end-x map-width))])
       (define-values (tile-disp-x tile-disp-y) (values (* (- x tiles-start-x) base-tile-size current-scaling) (* (- y tiles-start-y) base-tile-size current-scaling)))
       (draw-three-planes x y tile-disp-x tile-disp-y)))
    ;; Scroll in horizontal direction only
    ((and (not (zero? tiles-changed-x)) (zero? tiles-changed-y))
     (set! map-bitmap-offset-x (modulo (+ map-bitmap-offset-x tiles-changed-x) new-visible-x))
     (for* ([y (in-range tiles-start-y (min tiles-end-y map-height))]
            [x (in-range
                 (if (negative? tiles-changed-x)
                   tiles-start-x
                   visible-tiles-end-x)
                 (if (negative? tiles-changed-x)
                   visible-tiles-start-x
                   (min tiles-end-x map-width)))])
       (define-values
         (offs-x offs-y)
         (values
           (modulo (+ map-bitmap-offset-x (- x tiles-start-x)) new-visible-x)
           (modulo (+ map-bitmap-offset-y (- y tiles-start-y)) new-visible-y)))
       (define-values (tile-disp-x tile-disp-y) (values (* offs-x base-tile-size current-scaling) (* offs-y base-tile-size current-scaling)))
       (draw-three-planes x y tile-disp-x tile-disp-y)))
    ;; Scroll in vertical direction only
    ((and (zero? tiles-changed-x) (not (zero? tiles-changed-y)))
     (set! map-bitmap-offset-y (modulo (+ map-bitmap-offset-y tiles-changed-y) new-visible-y))
     (for* ([y (in-range
                 (if (negative? tiles-changed-y)
                   tiles-start-y
                   visible-tiles-end-y)
                 (if (negative? tiles-changed-y)
                   visible-tiles-start-y
                   (min tiles-end-y map-height)))]
            [x (in-range tiles-start-x (min tiles-end-x map-width))])
       (define-values
         (offs-x offs-y)
         (values
           (modulo (+ map-bitmap-offset-x (- x tiles-start-x)) new-visible-x)
           (modulo (+ map-bitmap-offset-y (- y tiles-start-y)) new-visible-y)))
       (define-values (tile-disp-x tile-disp-y) (values (* offs-x base-tile-size current-scaling) (* offs-y base-tile-size current-scaling)))
       (draw-three-planes x y tile-disp-x tile-disp-y))))
  ;; Diagonal scrolling is not done here, since even scrolling diagonally on
  ;; a trackpad generates only horizonal + vertical scroll movements.
  ;; Set the global variables
  (set! visible-tiles-start-x tiles-start-x)
  (set! visible-tiles-start-y tiles-start-y)
  (set! visible-tiles-end-x tiles-end-x)
  (set! visible-tiles-end-y tiles-end-y))

;; Draws the tile bitmap's visible area onto the canvas
(define (draw-tiles canvas dc)
  (define-values (start-x start-y) (send canvas get-view-start))
  (define-values (vis-x vis-y) (send canvas get-client-size))
  (define-values (tiles-start-x tiles-end-x tiles-start-y tiles-end-y) (visible-tiles-on-canvas canvas))
  ;; Redraw onto bitmap if the visible tile area is different
  (unless
    (and (= tiles-start-x visible-tiles-start-x)
         (= tiles-start-y visible-tiles-start-y)
         (= tiles-end-x visible-tiles-end-x)
         (= tiles-end-y visible-tiles-end-y))
    (generate-map-bitmap #f))
  ;; Paint four quadrants in the proper order
  (define-values (canvas-pixel-x canvas-pixel-y) (tile-location->canvas-location visible-tiles-start-x visible-tiles-start-y))
  (define-values (offset-bitmap-pixel-x offset-bitmap-pixel-y)
    (tile-location->canvas-location (+ visible-tiles-start-x map-bitmap-offset-x) (+ visible-tiles-start-y map-bitmap-offset-y)))
  (define-values (offset-canvas-pixel-x offset-canvas-pixel-y)
    (tile-location->canvas-location (- visible-tiles-end-x map-bitmap-offset-x) (- visible-tiles-end-y map-bitmap-offset-y)))
  (define-values (end-pixel-x end-pixel-y) (tile-location->canvas-location visible-tiles-end-x visible-tiles-end-y))
  (send dc draw-bitmap-section
        current-map-bitmap canvas-pixel-x canvas-pixel-y
        (* map-bitmap-offset-x current-scaling base-tile-size) (* map-bitmap-offset-y current-scaling base-tile-size)
        (- end-pixel-x offset-bitmap-pixel-x) (- end-pixel-y offset-bitmap-pixel-y))
  (send dc draw-bitmap-section
        current-map-bitmap offset-canvas-pixel-x canvas-pixel-y
        0 (* map-bitmap-offset-y current-scaling base-tile-size)
        (- offset-bitmap-pixel-x canvas-pixel-x) (- end-pixel-y offset-bitmap-pixel-y))
  (send dc draw-bitmap-section
        current-map-bitmap canvas-pixel-x offset-canvas-pixel-y
        (* map-bitmap-offset-x current-scaling base-tile-size) 0
        (- end-pixel-x offset-bitmap-pixel-x) (- offset-bitmap-pixel-y canvas-pixel-y))
  (send dc draw-bitmap-section
        current-map-bitmap offset-canvas-pixel-x offset-canvas-pixel-y
        0 0
        (- offset-bitmap-pixel-x canvas-pixel-x) (- offset-bitmap-pixel-y canvas-pixel-y)))

;; GUI Layout
(application:current-app-name appname)
(define mainwindow (new frame%
                        [label appname]
                        [width appwidth]
                        [height appheight]))
(define layout (new vertical-pane%
                    [parent mainwindow]))
(define topcontrols (new vertical-pane%
                         [parent layout]
                         [stretchable-height #f]))
(define buttons1 (new horizontal-pane%
                      [parent topcontrols]
                      [stretchable-height #f]))
(define buttons2 (new horizontal-pane%
                      [parent topcontrols]
                      [stretchable-height #f]))
(define displayarea (new tiles-canvas%
                         [parent layout]
                         [style '(vscroll hscroll)]
                         [paint-callback (λ (canvas dc)
                                            (unless (null? current-type)
                                              (draw-tiles canvas dc)))]))

(define infobar (new horizontal-pane%
                     [parent layout]
                     [stretchable-height #f]))

(define open-button (new button%
                         [label open-file-button-msg]
                         [parent buttons1]
                         [callback (λ (btn event)
                                      (open-file)
                                      )]))
(define filename-type-area (new vertical-pane%
                                [parent buttons1]
                                [min-width filename-minwidth]
                                [stretchable-width #t]))
(define filename-message (new message%
                              [label no-file-loaded-msg]
                              [parent filename-type-area]
                              [stretchable-width #t]))
(define filetype-message (new message%
                              [label type-unknown-msg]
                              [parent filename-type-area]
                              [stretchable-width #t]))
(define size-selector (new slider%
                           [label size-slider-msg]
                           [min-value min-scaling]
                           [max-value max-scaling]
                           [init-value current-scaling]
                           [parent buttons1]
                           [style '(horizontal plain)]
                           [min-width size-slider-minwidth]
                           [stretchable-width #f]
                           [callback (λ (sld event)
                                        (update-size)
                                        (unless (or (null? current-type) (= current-scaling (send sld get-value)))
                                          (set! current-scaling (send sld get-value))
                                          (readjust-displayarea-scrollbars #f)
                                          (generate-map-bitmap #t)))]))
(define size-label (new message%
                        [label "128"]
                        [parent buttons1]
                        [min-width tile-size-minwidth]))

(define previous-button (new button%
                             [label prev-button-msg]
                             [parent buttons2]
                             [callback (λ (btn event)
                                          (unless (null? current-type)
                                            (goto-map 'previous)))]))
(define next-button (new button%
                         [label next-button-msg]
                         [parent buttons2]
                         [callback (λ (btn event)
                                      (unless (null? current-type)
                                        (goto-map 'next)))]))
(define map-names (new choice%
                       [label #f]
                       [choices (list no-file-loaded-msg)]
                       [parent buttons2]
                       [stretchable-width #t]
                       [callback goto-map-from-choice]))

(define planes-checkboxes-layout (new horizontal-pane%
                                      [parent infobar]
                                      [stretchable-width #f]))
(define b-layout (new vertical-pane%
                      [parent planes-checkboxes-layout]
                      [stretchable-height #f]))
(define b-checkbox (new check-box%
                        [label ""]
                        [parent b-layout]
                        [value #t]
                        [callback (λ (chk event)
                                     (unless (null? current-type)
                                       (generate-map-bitmap #t)
                                       (send displayarea refresh)))]))
(define b-label (new message%
                     [label b-label-msg]
                     [parent b-layout]
                     [horiz-margin 0]))
(define f-layout (new vertical-pane%
                      [parent planes-checkboxes-layout]
                      [stretchable-height #f]))
(define f-checkbox (new check-box%
                        [label ""]
                        [parent f-layout]
                        [value #t]
                        [callback (λ (chk event)
                                     (unless (null? current-type)
                                       (generate-map-bitmap #t)
                                       (send displayarea refresh)))]))
(define f-label (new message%
                     [label f-label-msg]
                     [parent f-layout]
                     [horiz-margin 0]))
(define i-layout (new vertical-pane%
                      [parent planes-checkboxes-layout]
                      [stretchable-height #f]))
(define i-checkbox (new check-box%
                        [label ""]
                        [parent i-layout]
                        [value #t]
                        [callback (λ (chk event)
                                     (unless (null? current-type)
                                       (generate-map-bitmap #t)
                                       (send displayarea refresh)))]))
(define i-label (new message%
                     [label i-label-msg]
                     [parent i-layout]
                     [horiz-margin 0]))
(define coords-layout (new horizontal-pane%
                           [parent infobar]
                           [alignment '(right center)]))
(define coords-dec-layout (new vertical-pane%
                               [parent infobar]
                               [min-width coords-dec-minwidth]
                               [stretchable-width #f]))
(define coords-hex-layout (new vertical-pane%
                               [parent infobar]
                               [min-width coords-hex-minwidth]
                               [stretchable-width #f]))
(define coords-dec-x (new message%
                          [label (string-append coords-x-label coords-dec-unknown)]
                          [parent coords-dec-layout]))
(define coords-dec-y (new message%
                          [label (string-append coords-y-label coords-dec-unknown)]
                          [parent coords-dec-layout]))
(define coords-hex-x (new message%
                          [label (string-append coords-hex-prefix coords-hex-unknown)]
                          [parent coords-hex-layout]))
(define coords-hex-y (new message%
                          [label (string-append coords-hex-prefix coords-hex-unknown)]
                          [parent coords-hex-layout]))

(update-size)
(send mainwindow show #t)

; vim: expandtab:sw=2
