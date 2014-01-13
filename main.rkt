#lang racket

(require "controller.rkt"
         "sound.rkt")

(define controller-frame (make-object controller-frame%))
(send controller-frame show #t)

(queue-music-loop)
