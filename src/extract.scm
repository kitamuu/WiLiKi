;;;
;;; extract translatable messages
;;;
;;;  Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.
;;;
;;; $Id: extract.scm,v 1.3 2003-02-07 22:01:52 shirok Exp $

(define (scan-src file)
  (define msgs '())
  (define (rec expr)
    (when (pair? expr)
      (if (and (eq? (car expr) '$$)
               (pair? (cdr expr))
               (null? (cddr expr))
               (string? (cadr expr)))
          (push! msgs
                 `(,(cadr expr) ,(pair-attribute-get expr 'source-info #f) ""))
          (for-each rec expr))))
  (rec (call-with-input-file file port->sexp-list))
  msgs)

(define (scan-msgfile file)
  (if (file-exists? file)
      (call-with-input-file file port->sexp-list)
      '()))

(define (main args)
  (unless (= (length args) 3)
    (error "usage: gosh extract.scm file.scm msg-file"))
  (let* ((src-file (cadr args))
         (dst-file (caddr args))
         (dst-tmp  (string-append dst-file ".t"))
         (src-msgs (scan-src src-file))
         (dst-msgs (scan-msgfile dst-file)))
    (for-each (lambda (dst-msg)
                (cond ((assoc (car dst-msg) src-msgs)
                       => (lambda (p)
                            (set! (caddr p) (cadr dst-msg))
                            (set! (cddr dst-msg) #t)))))
              dst-msgs)
    (call-with-output-file (string-append dst-file ".t")
      (lambda (p)
        (for-each (lambda (src-msg)
                    (apply format p ";; ~a : line ~a\n" (cadr src-msg))
                    (format p "(~s\n" (car src-msg))
                    (format p " ~s\n" (caddr src-msg))
                    (format p ")\n\n"))
                  (reverse src-msgs))
        (for-each (lambda (dst-msg)
                    (when (null? (cddr dst-msg))
                      (format p ";; obsoleted message\n")
                      (format p ";; ~s\n" (car dst-msg))
                      (format p ";; ~s\n\n" (cadr dst-msg))))
                  (reverse dst-msgs))))
    (when (file-exists? dst-file)
      (sys-rename dst-file (string-append dst-file ".orig")))
    (sys-rename dst-tmp dst-file))
  0)


                    






