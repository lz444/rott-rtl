#lang racket

(provide (all-defined-out))

;; Functions to read RTL/RTC files used with the old DOS game
;; Rise of the Triad (ROTT)

;; Map constants
(define max-level-name-length 23)
(define allocated-level-name-length 24)
(define num-planes 3)
(define num-header-offsets 100)
(define map-width 128)
(define map-height 128)
(define map-special-toggle-pushwalls #x0001)

(define walls-plane 0)
(define sprites-plane 1)
(define info-plane 2)

(define rtl-signature (bytes
                        (char->integer #\R)
                        (char->integer #\T)
                        (char->integer #\L)
                        0))
(define rtc-signature (bytes
                        (char->integer #\R)
                        (char->integer #\T)
                        (char->integer #\C)
                        0))
(define rtr-signature (bytes
                        (char->integer #\R)
                        (char->integer #\T)
                        (char->integer #\R)
                        0))
(define rtl-version-1.01 (bytes 1 1 0 0))

(define sizeof-signature 4)
(define sizeof-long 4)
(define sizeof-rtl-header 64)

;; Type definitions
(struct rtl-header
        (used
          CRC
          RLEWtag
          MapSpecials
          planestart
          planelength
          Name))

;; read-rtl-signature
;; Reads the RTL signature from a file input port
(define (read-rtl-signature inport)
  (file-position inport 0)
  (read-bytes sizeof-signature inport))

;; read-rtl-version
;; Reads the RTL version number from a file input port
(define (read-rtl-version inport)
  (file-position inport sizeof-signature)
  (read-bytes sizeof-long inport))

;; read-rtl-header
;; Takes a file input port and a map number and returns a rtl-header struct
;; for the given map number.
;; Map numbers start from 0.
;; Does NOT check if the map is used. It only pulls the struct fields from the
;; RTL file.
(define (read-rtl-header inport mapnum)
  (file-position inport (+ sizeof-signature sizeof-long (* mapnum sizeof-rtl-header)))
  (rtl-header
    (read-bytes sizeof-long inport) ;; used
    (read-bytes sizeof-long inport) ;; CRC
    (read-bytes sizeof-long inport) ;; RLEWtag
    (read-bytes sizeof-long inport) ;; MapSpecials
    (build-vector num-planes (λ (n) (read-bytes sizeof-long inport))) ;; planestart
    (build-vector num-planes (λ (n) (read-bytes sizeof-long inport))) ;; planelength
    (read-bytes allocated-level-name-length inport))) ;; Name

;; strip-null-terminator
;; Takes a null-terminated byte string and returns a byte string without the
;; null terminator.
(define (strip-null-terminator s)
  (cond
    ((zero? (bytes-length s)) #"")
    ((zero? (bytes-ref s 0)) #"")
    (else (bytes-append (subbytes s 0 1) (strip-null-terminator (subbytes s 1))))))

;; list-mapnames
;; Takes a file input port and returns a list of length num-header-offsets
;; where each element in the list is the name of the map, or #f if there is no
;; map.
(define (list-mapnames inport)
  (build-list
    num-header-offsets
    (λ (n)
       (define header (read-rtl-header inport n))
       (if (not (zero? (integer-bytes->integer (rtl-header-used header) #f #f)))
         (~a (strip-null-terminator (rtl-header-Name header)))
         #f))))

;; read-compressed-plane
;; Takes an input port, an rtl-header, and a number and returns a byte string
;; representing the compressed plane given by the number
(define (read-compressed-plane inport header plane)
  (file-position
    inport
    (integer-bytes->integer (vector-ref (rtl-header-planestart header) plane) #f #f))
  (read-bytes
    (integer-bytes->integer (vector-ref (rtl-header-planelength header) plane) #f #f)
    inport))

;; make-word
;; creates a byte string consisting of the number of words provided
(define (make-word num val)
  (if (zero? num)
    #""
  (bytes-append val (make-word (sub1 num) val))))

;; RLEW-expand
;; Takes a byte string representing compressed data and another byte string
;; representing rlewtag then returns the expanded data.
(define (RLEW-expand compressed rlewtag)
  (cond
    ((zero? (bytes-length compressed))
     #"")
    ((bytes=? (subbytes compressed 0 2) (subbytes rlewtag 0 2))
     (bytes-append
       (make-word
         (integer-bytes->integer (subbytes compressed 2 4) #f #f)
         (subbytes compressed 4 6))
       (RLEW-expand (subbytes compressed 6) rlewtag)))
    (else
      (bytes-append
        (subbytes compressed 0 2)
        (RLEW-expand (subbytes compressed 2) rlewtag)))))

;; read-map-planes
;; Takes an input port and an rtl-header and returns a vector with the
;; uncompressed map data. Each element in the vector is one map plane.
(define (read-map-planes inport header)
  (build-vector
    num-planes
    (λ (n)
       (RLEW-expand
         (read-compressed-plane inport header n)
         (rtl-header-RLEWtag header)))))

;; mapspot
;; Takes an x, y position and returns where it would be in the uncompressed
;; plane
(define (mapspot x y)
  (* 2 (+ (* map-width y) x)))

;; tile-at
;; Takes an x, y position and a byte string representing the uncompressed plane
;; and returns a byte string of what is at the given coordinates. The byte
;; string is 2 bytes long.
(define (tile-at x y plane)
  (let ([pos (mapspot x y)])
    (subbytes plane pos (+ 2 pos))))

;; Convienence functions for testing purposes
(define (as-hex s [big-endian? #f] [outport (current-output-port)])
  (fprintf outport "~x~n" (integer-bytes->integer s #f big-endian?)))

(define (dumprow outport row plane width)
  (for ([i width])
    (display
      (string-append
        (~r
          (integer-bytes->integer (tile-at i row plane) #f)
          #:base 16
          #:min-width 4
          #:pad-string "0")
        " ")
      outport))
  (fprintf outport "~n"))

;; Dumps all three planes as hex values to outport (ideally an output file)
(define (mapdump outport planes width height)
  (fprintf outport ";; Background Plane~n")
  (for ([i height])
    (dumprow outport i (vector-ref planes walls-plane) width))
  (fprintf outport "~n;; Foreground Plane~n")
  (for ([i height])
    (dumprow outport i (vector-ref planes sprites-plane) width))
  (fprintf outport "~n;; Info Plane~n")
  (for ([i height])
    (dumprow outport i (vector-ref planes info-plane) width)))


; vim: expandtab:sw=2
