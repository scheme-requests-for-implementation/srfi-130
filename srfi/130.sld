(define-library (srfi 130)

  (export

    string-cursor?
    string-cursor-start    string-cursor-end
    string-cursor-next     string-cursor-prev
    string-cursor-forward  string-cursor-back
    string-cursor=?
    string-cursor<?        string-cursor>?
    string-cursor<=?       string-cursor>=?
    string-cursor-diff
    string-cursor->index   string-index->cursor

    string-null?
    string-every string-any

    string-tabulate
    string-unfold   string-unfold-right

    string->list/cursors string->vector/cursors
    reverse-list->string string-join

    string-ref/cursor
    substring/cursors  string-copy/cursors
    string-take        string-take-right
    string-drop        string-drop-right
    string-pad         string-pad-right
    string-trim        string-trim-right string-trim-both

    string-prefix-length    string-suffix-length
    string-prefix?          string-suffix?

    string-index     string-index-right
    string-skip      string-skip-right
    string-contains  string-contains-right

    string-reverse
    string-concatenate  string-concatenate-reverse
    string-fold         string-fold-right
    string-for-each-cursor
    string-replicate    string-count
    string-replace      string-split
    string-filter       string-remove

    )

  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) last-pair)
          (except (srfi 13)
                  string-index
                  string-index-right
                  string-skip
                  string-skip-right
                  string-map
                  string-for-each)
          (prefix (only (srfi 13)
                        string-index
                        string-index-right
                        string-skip
                        string-skip-right)
                  srfi-13:))

  (cond-expand
   (larceny
    (import (primitives errmsg)))
   (else
    (begin
     (define (errmsg key) "illegal argument(s)"))))

  (cond-expand
   ((library (rnrs base))
    (import (only (rnrs base) div mod assertion-violation)))
   (else
    (begin
      (define (assertion-violation procname msg . irritants)
        (apply error msg irritants))

      (define (div-and-mod x y)
        (cond ((and (exact-integer? x) (exact-integer? y))
               (cond ((= y 0)
                      (assertion-violation 'div "zero divisor" x y))
                     ((>= x 0)
                      (values (quotient x y) (remainder x y)))
                     ((< y 0)
                                        ; x < 0, y < 0
                      (let* ((q (quotient x y))
                             (r (- x (* q y))))
                        (if (= r 0)
                            (values q 0)
                            (values (+ q 1) (- r y)))))
                     (else
                                        ; x < 0, y > 0
                      (let* ((q (quotient x y))
                             (r (- x (* q y))))
                        (if (= r 0)
                            (values q 0)
                            (values (- q 1) (+ r y)))))))
              ((or (not (real? x))
                   (not (real? y))
;                  (infinite? x)
;                  (nan? x)
                   (= y 0))
               (assertion-violation 'div "illegal arguments" x y))
              ((< 0 y)
               (let* ((q (floor (/ x y)))
                      (r (- x (* q y))))
                 (values q r)))
              (else
               (let* ((q (floor (/ x (- y))))
                      (r (+ x (* q y))))
                 (values (- q) r)))))

      (define (div x y)
        (cond ((and (exact-integer? x)
                    (exact-integer? y)
                    (>= x 0))
               (quotient x y))
              (else
               (call-with-values
                   (lambda () (div-and-mod x y))
                 (lambda (q r) q)))))

      (define (mod x y)
        (cond ((and (exact-integer? x)
                    (exact-integer? y)
                    (>= x 0))
               (remainder x y))
              (else
               (call-with-values
                   (lambda () (div-and-mod x y))
                 (lambda (q r) r))))))))


  (include "130.body.scm"))
