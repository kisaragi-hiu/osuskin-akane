#lang racket
;;; Copyright © 2019 Kisaragi Hiu <mail@kisaragi-hiu.com>
;;;
;;; This work is Free.
;;; You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2, as published by Sam Hocevar. A copy of the license is shown below.
;;;
;;;         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;;                     Version 2, December 2004
;;;
;;;  Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
;;;
;;;  Everyone is permitted to copy and distribute verbatim or modified
;;;  copies of this license document, and changing it is allowed as long
;;;  as the name is changed.
;;;
;;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;;
;;;     0. You just DO WHAT THE FUCK YOU WANT TO.
(require threading
         "helper.rkt")

(provide resize-@ resize-@2x resize-resizeto crop trim get-@-size)

; orig, target : (listof path?)
; ratio : ((listof number?) . or/c . number?)
(define/contract (resize-vips orig target ratio)
  (-> (listof path?)
      (listof path?)
      (or/c (listof number?)
            number?)
      any/c)
  (apply
   run-command
   `("parallel" "vips" "resize" "{1}" "{2}" "{3}"
     ":::" ,@(map path->string orig)
     ":::+" ,@(map path->string target)
     ,@(if (list? ratio)
           (cons ":::+" (map (compose1 number->string exact->inexact) ratio))
           (list ":::" (number->string (exact->inexact ratio)))))))

; orig, target : path?
; size : string?
(define (resize-im orig target size)
  (run-command "gm" "convert" "-resize"
               size
               (path->string orig)
               (path->string target)))

;; get-@-size : string -> number
(define (get-@-size string)
  (~> (string-replace string #rx".*@" "")
      (string-replace _ #rx"x.*" "")
      (string->number _)))

(define (resize-@ . paths)
  (define valid-paths
    (filter
     (lambda (path)
       (and (regexp-match #px".*@[[:digit:]]+x.*" (path->string (path-basename path)))
            (file-exists? path)))
     paths))
  (define twox-paths
    (filter
     (lambda (path)
       (regexp-match #px".*@2x.*" (path->string (path-basename path))))
     valid-paths))
  (define other-paths
    (set-subtract valid-paths twox-paths))
  (define other-paths-in-2x
    (map (lambda (orig) (path-replace orig #px"@[[:digit:]]+x" "@2x"))
         other-paths))
  (define other-paths-ratios
    (map (lambda (orig) (/ 2 (get-@-size (path->string orig))))
         other-paths))
  ;; Resize all 2x all at once
  (apply resize-@2x twox-paths)
  ;; Resize 4x etc. to 2x
  (resize-vips
   other-paths
   other-paths-in-2x
   other-paths-ratios)
  ;; Resize new 2x
  (apply resize-@2x other-paths-in-2x)
  ;; Delete the 4x etc. files
  (map delete-file other-paths))

(define (resize-@2x . paths)
  (define valid-paths
    (filter
     (lambda (path)
       (and (regexp-match #rx".*@2x.*" (path->string (path-basename path)))
            (file-exists? path)))
     paths))
  (resize-vips
   valid-paths
   (map (lambda (x) (path-replace x #rx"@2x" ""))
        valid-paths)
   0.5))

(define (resize-resizeto path)
  (define base-path (path-basename path))
  (cond [(not (or (regexp-match #px"resizeto[[:digit:]]+x[[:digit:]]+" (path->string base-path))
                  (regexp-match #px"resizeto[[:digit:]]+%" (path->string base-path))))
         #f]
        [(not (file-exists? path)) #f]
        [else
          (define target (path-replace path #rx"_resizeto.*.png$" ".png"))
          (define size (~> (path->string path)
                           (string-replace _ #rx"^.*resizeto" "")
                           (string-replace _ #rx"\\..*" "")))
          (resize-im path target size)
          (delete-file path)
          ;; return target
          target]))

(define (crop path)
  (define base-path (path-basename path))
  (cond [(not (regexp-match #px"tocrop" (path->string base-path))) #f]
        [(not (file-exists? path)) #f]
        [else
          (define crop-dimention
            (~> (path->string path)
                (regexp-match #px"[[:digit:]]+x[[:digit:]]+\\+[[:digit:]]+\\+[[:digit:]]" _)
                (first _))) ; regexp-match returns a list of matching substrings
          (define target (path-replace path (string-append "_tocrop" crop-dimention) ""))
          (run-command "convert" "-crop"
                       crop-dimention
                       (path->string path)
                       (path->string target))
          (delete-file path)
          ;; return target
          target]))

(define (trim . paths)
  (define valid-paths
    (filter
     (lambda (path)
       (and (regexp-match #px"totrim" (path->string (path-basename path)))
            (file-exists? path)))
     paths))
  (apply run-command
         `("parallel" "convert" "-trim" "+repage"
           ":::" ,@(map path->string valid-paths)
           ":::+" ,@(map (lambda (path)
                           (string-replace
                            (path->string path)
                            "totrim" ""))
                         valid-paths)))
  (map delete-file valid-paths))
