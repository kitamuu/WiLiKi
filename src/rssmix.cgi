#!/usr/bin/gosh
;;;
;;; wiliki/rssmix - Fetch and show RSSs
;;;
;;;  Copyright (c) 2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: rssmix.cgi,v 1.2 2003-02-17 13:29:55 shirok Exp $
;;;

;; THIS IS AN EXPERIMENTAL SCRIPT.  Eventually this will be a part of
;; WiLiKi release.
;;
;; Requires the newest CVS snapshot of Gauche as of 2003/2/16, in which
;;   a crucial bug about multithreading is fixed.
;; Requires SXML-gauche 0.9.

(use srfi-1)
(use srfi-2)
(use srfi-13)
(use srfi-14)
(use srfi-19)
(use rfc.http)
(use rfc.uri)
(use text.html-lite)
(use util.list)
(use sxml.ssax)
(use gauche.threads)
(use gauche.uvector)
(use gauche.regexp)
(use gauche.charconv)
(use dbm)
(use www.cgi)

(autoload dbm.gdbm <gdbm>)

(define-constant USER_AGENT
  "wiliki/rssmix http://www.shiro.dreamhost.com/scheme/wiliki/rssmix.cgi")

(define-constant NAMESPACES
  '((rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rss . "http://purl.org/rss/1.0/")
    (dc  . "http://purl.org/dc/elements/1.1/")))

(define-class <rssmix> ()
  ((sites :init-keyword :sites :init-value '())
   ;; - list of monitoring sites.  Each entry should be
   ;;     (IDENT HOME-URI RSS-URI)
   (num-items :init-keyword :num-items :init-value 50)
   ;; - # of entries to show
   (title :init-keyword :title :init-value "Recent Changes")
   (db-name :init-keyword :db-name :init-value "/home/shiro/data/rssmix.dbm")
   (db-type :init-keyword :db-type :init-form <gdbm>)
   (cache-life :init-keyword :cache-life :init-value 1800)
   ;; - lifetime of cache, in seconds.
   (fetch-timeout :init-keyword :fetch-timeout :init-value 30)
   ;; - timeout value to fetch RSS
   (db      :init-value #f)
   ;; - opened dbm instance
   ))

;; temporary structure to represent site item info
(define-class <rss-item> ()
  ((site-id  :init-keyword :site-id)
   (site-url :init-keyword :site-url)
   (title    :init-keyword :title)
   (link     :init-keyword :link)
   (date     :init-keyword :date)
   (cached   :init-keyword :cached)
   ))

(define-syntax with-rss-db
  (syntax-rules ()
    ((_ self . body)
     (let* ((s  self)
            (db (dbm-open (ref s 'db-type)
                          :path (ref s 'db-name) :rwmode :write)))
       (set! (ref s 'db) db)
       (with-error-handler
           (lambda (e) (dbm-close db) (raise e))
         (lambda ()
           (receive r (begin . body) (dbm-close db) (apply values r)))))
     )))

(define-method rss-main ((self <rssmix>))
  (cgi-main
   (lambda (params)
     `("Content-Style-Type: text/css\n"
       ,(cgi-header)
       ,(html-doctype :type :transitional)
       ,(html:html
         (html:head
          (html:title (html-escape-string (ref self 'title)))
          (html:link :rel "stylesheet" :href "wiliki-sample.css" :type "text/css"))
         (html:body
          (html:h1 (html-escape-string (ref self 'title)))
          (html:div :align "right"
                  "[" (html:a :href "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?WiLiKi:RSSMix" "What's This?") "]")
          (html:hr)
          (html:table
           (let ((items (with-rss-db self (collect self))))
             (map (lambda (item)
                    (html:tr
                     (html:td (sys-strftime "%Y/%m/%d %H:%M:%S %Z"
                                            (sys-localtime
                                             (ref item 'date))))
                     (html:td
                      (html:a :href (ref item 'site-url)
                              (ref item 'site-id))
                      ": "
                      (html:a :href (ref item 'link)
                                      (html-escape-string (ref item 'title)))
                              ;; for testing
                              ;;(if (ref item 'cached) "(cached)" "")
                              )))
                  (take* items
                         (ref self 'num-items)))
             )
           ))))))
  0)

;; Collect RSS info from given sites.
(define (collect self)
  (let* ((db     (ref self 'db))
         (sites  (ref self 'sites))
         (rss-list (map (lambda (site) (get-rss self (car site) (caddr site)))
                        sites))
         (timeout (ref self 'fetch-timeout))
         )
    (sort (append-map
           (lambda (site rss)
             (or (and-let* ((items (if (thread? (cdr rss))
                                       (thread-join! (cdr rss) timeout (car rss))
                                       (car rss))))
                   (map (lambda (item)
                          (make <rss-item>
                            :site-id (car site) :site-url (cadr site)
                            :title (car item) :link (cadr item)
                            :date (caddr item)
                            :cached (eq? (cdr rss) #t)))
                        items))
                 '()))
           sites rss-list)
          (lambda (a b) (> (ref a 'date) (ref b 'date))))
    ))

(define (get-rss self id rss-url)
  (let ((rss&valid? (get-cache self id))
        (fetch-thread (lambda ()
                        (thread-start!
                         (make-thread (make-thunk self id rss-url) id))))
        )
    (if rss&valid?
        (cons (car rss&valid?)
              (or (cdr rss&valid?) (fetch-thread)))
        (cons '() (fetch-thread)))))

;; get cached result if any
(define (get-cache self id)
  (and-let* ((body  (dbm-get (ref self 'db) id #f))
             (sbody (read-from-string body))
             (timestamp (get-keyword :timestamp sbody 0)))
    (cons
     (get-keyword :rss-cache sbody #f)
     (> timestamp (- (sys-time) (ref self 'cache-life))))))

(define (put-cache! self id rss)
  (dbm-put! (ref self 'db) id
            (write-to-string (list :timestamp (sys-time) :rss-cache rss))))

;; Creates a thunk for thread
(define (make-thunk self id uri)
  (lambda ()
    (with-error-handler
        (lambda (e) (display (ref e 'message) (current-error-port)) #f)
      (lambda ()
        (let1 r (fetch uri)
          (when r (put-cache! self id r))
          r)))))

;; Fetch RSS from specified URI, parse it, and extract link information
;; with updated dates.  Returns list of items, in
;;  (TITLE URI DATETIME)
;; where DATETIME is in time-utc.
;; When error, returns #f.
(define (fetch uri)
  (and-let* ((match  (#/^http:\/\/([^\/]+)/ uri))
             (server (match 1))
             (path   (match 'after)))
    (receive (status headers body)
        (http-get server path :user-agent USER_AGENT)
      (and-let* (((equal? status "200"))
                 ((string? body))
                 (encoding (body-encoding body)))
        (extract-from-rdf
         (SSAX:XML->SXML
          (wrap-with-input-conversion (open-input-string body) encoding)
          NAMESPACES))))
    ))

;; Figure out the encoding of the returned body.  At this point,
;; the body might be an incomplete string, so we have to be careful.
;; Returns #f if body is not a valid xml doc.
(define (body-encoding body)
  (and-let* ((body   (string-complete->incomplete body))
             (before (string-scan body #*"?>" 'before))
             (enc    (string-scan before #*"encoding=\"" 'after))
             (enc2   (string-scan enc #*"\"" 'before)))
    enc2))

;; Traverse RDF tree and obtain necessary info.
;; It would be better to use SXPath, but for now...
(define (extract-from-rdf sxml)

  (define (find-node tag parent)
    (find (lambda (n) (eq? (car n) tag)) (cdr parent)))

  (define (filter-node tag parent)
    (filter (lambda (n) (eq? (car n) tag)) (cdr parent)))

  ;; NB: srfi-19's string->date fails to recognize time zone offset
  ;; with ':' between hours and minutes.  I need to parse it manually.
  (define (parse-date date)
    (and-let* 
        ((match (#/^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)([+-]\d\d):(\d\d)/ date)))
      (receive (year month day hour minute second zh zm)
          (apply values (map (lambda (i) (x->integer (match i))) (iota 8 1)))
        (time-second
         (date->time-utc (make-date 0 second minute hour day month year
                                    (* (if (negative? zh) -1 1)
                                       (+ (* (abs zh) 3600) (* zm 60))))))
        )))
  
  (let* ((rdf   (find-node 'rdf:RDF sxml))
         (items (filter-node 'rss:item rdf)))
    (filter-map (lambda (item)
                  (let ((title (and-let* ((n (find-node 'rss:title item)))
                                 (cadr n)))
                        (link  (and-let* ((n (find-node 'rss:link item)))
                                 (cadr n)))
                        (date  (and-let* ((n (find-node 'dc:date item)))
                                 (parse-date (cadr n)))))
                    (and title link date (list title link date))))
                items)))

;; Entry-point
(define (main args)
  (rss-main
   (make <rssmix>
     :sites '(("WiLiKi"
               "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi"
               "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?c=rss")
              ("SchemeXref"
               "http://www.shiro.dreamhost.com/scheme/wiliki/schemexref.cgi"
               "http://www.shiro.dreamhost.com/scheme/wiliki/schemexref.cgi?c=rss")
              ("�����"
               "http://slashdot.jp/"
               "http://slashdot.jp/slashdot.rdf")
              ("On Off and Beyond"
               "http://blog.neoteny.com/chika/"
               "http://blog.neoteny.com/chika/index.rdf")
              ("WikiLike"
               "http://ishinao.net/WikiLike/"
               "http://ishinao.net/WikiLike/rss.php")
              ;("@pm"
              ; "http://gnk.s15.xrea.com/"
              ; "http://gnk.s15.xrea.com/index.rdf")
              ;("Netry"
              ; "http://netry.no-ip.com/"
              ; "http://netry.no-ip.com/index.rdf")
              )
     :title "RSSMix: Recent Entries")))

;; Local variables:
;; mode: scheme
;; end: