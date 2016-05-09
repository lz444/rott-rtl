#lang racket

(require "rtl.rkt")

;; Testing reading in RTL files
;; RTL files for testing purposes
(define huntbgin (expand-user-path "~/dosbox_apps/apogeerg/ROTT/HUNTBGIN.RTL"))
(define darkwar (expand-user-path "~/dosbox_apps/apogeerg/ROTT/DARKWAR.RTL"))
(define commbat-rottcd (expand-user-path "~/dosbox_apps/apogeerg/ROTT/ROTTCD.RTC"))
(define ted5-sample (expand-user-path "~/dosbox_apps/ted5/SAMPLMAP/SAMPLE.RTL"))

;; seeks to the position of the name of the first map
;(define inport1 (open-input-file ted5-sample #:mode 'binary))
; (file-position inport1 #x30)


;; Let's read stuff out of some RTL files
(define in1 (open-input-file ted5-sample #:mode 'binary))
(define using-keys-header (read-rtl-header in1 0))
(define plane0-compressed (read-compressed-plane in1 using-keys-header walls-plane))
(define plane0-expanded (RLEW-expand plane0-compressed (subbytes (rtl-header-RLEWtag using-keys-header) 0 2)))

(define in2 (open-input-file huntbgin #:mode 'binary))
(define huntbgin-header (read-rtl-header in2 8))
(define huntbgin-mapdata (read-map-planes in2 huntbgin-header))

(define out (open-output-file "rtl-dump.txt" #:mode 'binary #:exists 'replace))
(fprintf out "~a~n~n" (rtl-header-Name huntbgin-header))
(mapdump out huntbgin-mapdata map-width map-height)
(close-output-port out)

;(close-input-port in1)

; vim: expandtab:sw=2
