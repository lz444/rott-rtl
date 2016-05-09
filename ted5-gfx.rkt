#lang racket/gui

(provide huffnode)
(provide tilesize-ega16)
(provide tilesize-ega16m)
(provide dump16)
(provide dump16m)
(provide read-background-tiles-ega16)
(provide read-masked-tiles-ega16)
(provide reverse&flip)
(provide paint-tile)
(provide paint-hex)

;; Reads from the TED5 graphics files (?GAGRAPH, ?GAHEAD, ?GADICT)
;; The huffexpand functions were converted from JHUFF.C from the IGRAB souce.

(struct huffnode (bit0 bit1))

;; Reads a hufftable from a path to an ?GADICT file
(define (build-hufftable path)
  (call-with-input-file
    #:mode 'binary
    path
    (λ (in)
       (build-vector
         255
         (λ (n)
            (huffnode 
              (integer-bytes->integer (read-bytes 2 in) #f #f)       ;; bit0
              (integer-bytes->integer (read-bytes 2 in) #f #f))))))) ;; bit1

;; head node is always node 254
(define huff-headnode 254)

;; Recursive huffexpand
(define (huffexpand-recursive inport len hufftable node bit)
  (if (zero? len)
    #""
    (let*
      ([byte (peek-byte inport)]
       [code 
         (if (zero? (bitwise-and byte bit))
           (huffnode-bit0 (vector-ref hufftable node))
           (huffnode-bit1 (vector-ref hufftable node)))]
       [shiftedbit (arithmetic-shift bit 1)]
       [nextbit (if (= 256 shiftedbit) 1 shiftedbit)])
      (when (= 256 shiftedbit)
        (file-position inport (add1 (file-position inport))))
      (if (< code 256)
        (bytes-append
          (bytes code)
          (huffexpand-recursive inport (sub1 len) hufftable huff-headnode nextbit))
        (huffexpand-recursive inport len hufftable (- code 256) nextbit)))))

;; Takes an input port to the compressed data, the length of the expanded
;; data, and a hufftable then returns expanded data.
;; The input port should be set to the proper position before calling
;; huffexpand.
(define (huffexpand inport len hufftable)
  (huffexpand-recursive inport len hufftable huff-headnode 1))

;; Size of graphics tiles, in bytes
(define tilesize-ega16 (/ (* 16 16 4) 8))
(define tilesize-ega16m (/ (* 16 16 5) 8))
(define base-tile-size 16)

;; Problem: bitmap masks need to have each byte reversed & their bits flipped
;; A practical example of the "bit-reversal" problem.
;; This solution uses bit shifting on the input & output numbers.
;; I don't use this as the tiles looked better upscaled using the alpha channel
;; instead, but I'll leave it here in case anybody wants to use a bitmap mask.
;; b is the bits to flip
;; n is the number of bits to flip minus 1 (so for a byte, n would be 7)
(define (reverse&flip b n)
  (define flipped-bit (bitwise-bit-field (bitwise-not b) 0 1))
  (cond
    ((zero? n)
     flipped-bit)
    ((zero? b)
     ;; optimization for when the rest of the bits to be flipped are 0
     (bitwise-bit-field -1 0 (add1 n)))
    (else
      (bitwise-ior (arithmetic-shift flipped-bit n) (reverse&flip (arithmetic-shift b -1) (sub1 n))))))

;; Takes a byte string representing RGBI or RGBIM data and a flag on whether
;; the tile is masked then returns a 16x16 bitmap of the tile. 
(define (make-tile-bitmap-16 tile masked?)
  ;; Byte string to hold the pixels
  (define pixels (make-bytes (* 16 16 4)))
  ;; Locations for R, G, B, I
  (define red-start (if masked? (* 3 32) (* 2 32)))
  (define green-start (if masked? (* 2 32) (* 1 32)))
  (define blue-start (if masked? (* 1 32) (* 0 32)))
  (define intensity-start (if masked? (* 4 32) (* 3 32)))
  ;; Run through the RGBI byte string & generate a byte string for use with set-argb-pixels
  (for* ([y 16]
         [x 16])
    (define position (+ (* 16 4 y) (* 4 x)))
    ;; Use the alpha channel for the mask
    (if masked?
      (let* ([mask-pix (integer-bytes->integer (subbytes tile (* 2 y) (+ 2 (* 2 y))) #f #t)]
             [mask? (bitwise-bit-set? mask-pix (- 15 x))])
        ;; Non-transparent portions are stored as a 0 in the original TED5 graphic mask
        (unless mask? (bytes-set! pixels position 255)))
      ;; Alpha is not used for nonmasked tiles, so just set it to 255
      (bytes-set! pixels position 255))
    ;; Set the red values
    ;; Each row is 2 bytes long
    (define red-pix (integer-bytes->integer (subbytes tile (+ red-start (* 2 y)) (+ 2 red-start (* 2 y))) #f #t))
    (define red? (bitwise-bit-set? red-pix (- 15 x)))
    (when red? (bytes-set! pixels (+ 1 position) #xaa))
    ;; Set the green values
    (define green-pix (integer-bytes->integer (subbytes tile (+ green-start (* 2 y)) (+ 2 green-start (* 2 y))) #f #t))
    (define green? (bitwise-bit-set? green-pix (- 15 x)))
    (when green? (bytes-set! pixels (+ 2 position) #xaa))
    ;; Set the blue values
    (define blue-pix (integer-bytes->integer (subbytes tile (+ blue-start (* 2 y)) (+ 2 blue-start (* 2 y))) #f #t))
    (define blue? (bitwise-bit-set? blue-pix (- 15 x)))
    (when blue? (bytes-set! pixels (+ 3 position) #xaa))
    ;; Set the intensity values
    (define intense-pix (integer-bytes->integer (subbytes tile (+ intensity-start (* 2 y)) (+ 2 intensity-start (* 2 y))) #f #t))
    (define intense? (bitwise-bit-set? intense-pix (- 15 x)))
    (when intense?
      (bytes-set! pixels (+ 1 position) (bitwise-ior (bytes-ref pixels (+ 1 position)) #x55))
      (bytes-set! pixels (+ 2 position) (bitwise-ior (bytes-ref pixels (+ 2 position)) #x55))
      (bytes-set! pixels (+ 3 position) (bitwise-ior (bytes-ref pixels (+ 3 position)) #x55)))
    ;; Special case the brown color
    (when (and red? green? (not blue?) (not intense?))
      (bytes-set! pixels (+ 2 position) #x55)))

  ;; Create the tile bitmap
  (define tile-bitmap (make-bitmap 16 16 masked?))
  (define dc (send tile-bitmap make-dc))
  (send dc set-argb-pixels 0 0 16 16 pixels)
  tile-bitmap)

;; Reads in background tiles.
;; Needs paths to GFXINFOE, EGAHEAD, EGAGRAPH, and EGADICT,
;; then returns a vector with the graphics data. The length is the number of
;; tiles defined in GFXINFO. Each element is a bitmap for one tile.
;; If the tile is not used then the element is #f.
(define (read-background-tiles-ega16 gfxinfoe-path egahead-path egagraph-path egadict-path)
  ;; Read in data from GFXINFO
  (define gfxinfo-in (open-input-file gfxinfoe-path #:mode 'binary))
  (file-position gfxinfo-in #x4)
  (define numtiles (integer-bytes->integer (read-bytes 2 gfxinfo-in) #f #f))
  (file-position gfxinfo-in #x10)
  (define tiles-start (* 3 (integer-bytes->integer (read-bytes 2 gfxinfo-in) #f #f)))
  (close-input-port gfxinfo-in)

  (define egadict-table (build-hufftable egadict-path))

  ;; Read & Uncompress the graphics
  (define egahead-in (open-input-file egahead-path #:mode 'binary))
  (define egagraph-in (open-input-file egagraph-path #:mode 'binary))
  (define tiles
    (build-vector
      numtiles
      (λ (n)
         (define header-position (+ tiles-start (* 3 n)))
         (file-position egahead-in header-position)
         (define gfx-position (integer-bytes->integer (bytes-append (read-bytes 3 egahead-in) (bytes 0)) #f #f))
         ;; #xffffff in egahead means the tile is not defined
         (if (= gfx-position #xffffff)
           #f
           (begin
             (file-position egagraph-in gfx-position)
             (make-tile-bitmap-16 (huffexpand egagraph-in tilesize-ega16 egadict-table) #f))))))
  (close-input-port egahead-in)
  (close-input-port egagraph-in)
  tiles)

;; Reads in masked tiles.
;; Needs paths to GFXINFOE, EGAHEAD, EGAGRAPH, and EGADICT
;; then returns a vector with the graphics data. The length is the number of
;; tiles defined in GFXINFO. Each element is a bitmap for one tile, complete
;; with mask.
;; If the tile is not used then the element is #f.
(define (read-masked-tiles-ega16 gfxinfoe-path egahead-path egagraph-path egadict-path)
  ;; Read in data from GFXINFO
  (define gfxinfo-in (open-input-file gfxinfoe-path #:mode 'binary))
  (file-position gfxinfo-in #x6)
  (define numtilesm (integer-bytes->integer (read-bytes 2 gfxinfo-in) #f #f))
  (file-position gfxinfo-in #x12)
  (define tilesm-start (* 3 (integer-bytes->integer (read-bytes 2 gfxinfo-in) #f #f)))
  (close-input-port gfxinfo-in)

  (define egadict-table (build-hufftable egadict-path))

  ;; Read & Uncompress the graphics
  (define egahead-in (open-input-file egahead-path #:mode 'binary))
  (define egagraph-in (open-input-file egagraph-path #:mode 'binary))
  (define tiles
    (build-vector
      numtilesm
      (λ (n)
         (define header-position (+ tilesm-start (* 3 n)))
         (file-position egahead-in header-position)
         (define gfx-position (integer-bytes->integer (bytes-append (read-bytes 3 egahead-in) (bytes 0)) #f #f))
         ;; #xffffff in egahead means the tile is not defined
         (if (= gfx-position #xffffff)
           #f
           (begin
             (file-position egagraph-in gfx-position)
             (make-tile-bitmap-16 (huffexpand egagraph-in tilesize-ega16m egadict-table) #t))))))
  (close-input-port egahead-in)
  (close-input-port egagraph-in)
  tiles)

;; Functions for testing purposes
(define (dumprow16 outport row tile)
  (define startpos (* 8 row))
  (for ([i 8])
    (display
      (string-append
        (~r
          (bytes-ref tile (+ startpos i))
          #:base 16
          #:min-width 2
          #:pad-string "0")
        " ")
      outport))
  (fprintf outport "~n"))

(define (dump16 tile [outport (current-output-port)])
  (for ([i 16])
    (dumprow16 outport i tile)))

(define (dump16m tile [outport (current-output-port)])
  (for ([i 20])
    (dumprow16 outport i tile)))

;; Paints the given tile into the canvas or bitmap
(define (paint-tile drawingarea x y tile scaling)
  ;; Tile is unused, don't draw anything. Special case for games which
  ;; didn't define a tile for value 0 (such as Bio Menace)
  (when tile
    (let ([dc
            (cond
              ((is-a? drawingarea canvas%) (send drawingarea get-dc))
              ((is-a? drawingarea bitmap%) (send drawingarea make-dc)))])
      (send dc set-smoothing 'unsmoothed) ;; I like blocky pixelated tiles :]
      (send dc set-scale scaling scaling)
      (send dc draw-bitmap tile (floor (/ x scaling)) (floor (/ y scaling))))))

;; Paints a hex number into the canvas or bitmap
(define (paint-hex drawingarea x y n fgcolor bgcolor bgalpha scaling)
  (define dc
    (cond
      ((is-a? drawingarea canvas%) (send drawingarea get-dc))
      ((is-a? drawingarea bitmap%) (send drawingarea make-dc))))
  (send dc set-scale 1 1)
  (define color (make-color
                  (send bgcolor red)
                  (send bgcolor green)
                  (send bgcolor blue)
                  bgalpha))
  (send dc set-brush color 'solid)
  (send dc set-pen "white" 0 'transparent)
  (send dc draw-rectangle x y (* scaling base-tile-size) (* scaling base-tile-size))
  (send dc set-text-foreground fgcolor)
  (send dc set-font (make-font
                      #:size (/ (* base-tile-size scaling) 2)
                      #:family 'system
                      #:size-in-pixels? #t))
  (define hex (~r n #:base '(up 16) #:min-width 4 #:pad-string "0"))
  ;; Center the text horizontally
  (define-values (w1 h1 d1 e1) (send dc get-text-extent (substring hex 0 2)))
  (define-values (w2 h2 d2 e2) (send dc get-text-extent (substring hex 2 4)))
  (define extra1 (- (* base-tile-size scaling) w1))
  (define extra2 (- (* base-tile-size scaling) w2))
  (define start1 (/ extra1 2))
  (define start2 (/ extra2 2))
  (send dc draw-text (substring hex 0 2) (+ x start1) y)
  (send dc draw-text (substring hex 2 4) (+ x start2) (+ y (/ (* base-tile-size scaling) 2))))

; vim: expandtab:sw=2
