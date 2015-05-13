#lang racket/base

(require racket/match)

(let loop ()
  (print (match (read)
           [(? eof-object?) (exit 0)]
           [`(send) `(response send goal)]
           [`(unsend) `(response unsend goal)]
           [_ `(bad-command)]))
  (newline)
  (loop))
