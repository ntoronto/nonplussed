#lang racket

(require racket/runtime-path
         rsound)

(provide play-piece-slide-loud
         play-piece-slide-soft
         play-success-low
         play-success-high
         play-success-huge
         queue-music-loop
         (rename-out [stop stop-sounds]))

(define-runtime-path piece-slide-loud-path "sounds/piece-slide-big.wav")
(define-runtime-path piece-slide-soft-path "sounds/piece-slide.wav")
(define-runtime-path success-low-path "sounds/success-low.wav")
(define-runtime-path success-high-path "sounds/success-high.wav")
(define-runtime-path success-huge-path "sounds/success-huge.wav")
(define-runtime-path music-path "sounds/ninja-cat-band.wav")

(define piece-slide-loud (rs-read piece-slide-loud-path))
(define piece-slide-soft (rs-scale 0.25 (rs-read piece-slide-soft-path)))
(define success-low (rs-read success-low-path))
(define success-high (rs-read success-high-path))
(define success-huge (rs-read success-huge-path))

(define music (rs-scale 0.66 (rs-read music-path)))
(define music-frames (rs-frames music))

(define (queue-music-loop)
  (define stream (make-pstream #:buffer-time 0.2))
  (pstream-play stream music)
  (pstream-queue-callback stream queue-music-loop (+ music-frames (/ 44100 2))))

(define (play-piece-slide-loud) (play piece-slide-loud))
(define (play-piece-slide-soft) (play piece-slide-soft))
(define (play-success-low) (play success-low))
(define (play-success-high) (play success-high))
(define (play-success-huge) (play success-huge))
