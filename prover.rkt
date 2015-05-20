#lang racket/base
(require racket/list
         racket/pretty
         racket/match)

(printf "ready\n")
(let loop ([goal empty])
  (match (read)
    [(? eof-object?) (exit 0)]
    [`(send ,sexpr)
     (define ngoal (cons sexpr goal))
     (println `(response "OK" ,(pretty-format (reverse ngoal))))
     (loop ngoal)]
    [`(unsend "")
     (define ngoal
       (if (null? goal) goal (cdr goal)))
     (println `(response "OK" ,(pretty-format ngoal)))
     (loop ngoal)]
    [_
     (println `(response "NO" ,(pretty-format goal)))
     (loop goal)]))
