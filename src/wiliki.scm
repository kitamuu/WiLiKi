;;;
;;; WiLiKi - Wiki in Scheme
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
;;;  $Id: wiliki.scm,v 1.88 2003-08-31 10:37:40 shirok Exp $
;;;

(define-module wiliki
  (use srfi-1)
  (use srfi-2)                          ;and-let*
  (use srfi-11)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use util.list)
  (use www.cgi)
  (use rfc.uri)
  (use dbm)
  (use gauche.charconv)
  (use gauche.version)
  (use gauche.parameter)
  (use gauche.sequence)
  (use wiliki.mcatalog)
  (export <wiliki> wiliki-main))
(select-module wiliki)

;; Load extra code only when needed.
(autoload dbm.gdbm <gdbm>)
(autoload wiliki.db      with-db
                         wdb-exists? wdb-record->page wdb-get
                         wdb-put! wdb-delete!
                         wdb-recent-changes wdb-map
                         wdb-search wdb-search-content)
(autoload "wiliki/macro" handle-reader-macro handle-writer-macro
                         handle-virtual-page virtual-page?)
(autoload wiliki.rss     rss-page)
(autoload wiliki.pasttime how-long-since)
(autoload wiliki.format  format-page format-footer format-content
                         format-time format-colored-box format-wikiname-anchor
                         format-wiki-name)
(autoload wiliki.log     wiliki-log-create wiliki-log-pick
                         wiliki-log-pick-from-file
                         wiliki-log-parse-entry wiliki-log-entries-after
                         wiliki-log-diff wiliki-log-diff*
                         wiliki-log-revert wiliki-log-revert*
                         wiliki-log-merge)
(autoload "wiliki/history" cmd-history cmd-diff)


;; Version check.
(when (version<? (gauche-version) "0.6.7.1")
  (print (tree->string
          `(,(cgi-header)
            ,(html:html (html:head (html:title "Error")))
            ,(html:body "Gauche 0.6.7.1 or later is required."))))
  (exit 0))

;; Some constants

(define *recent-changes* " %recent-changes")
(define *lwp-version* "1.0")            ;''lightweight protocol'' version
(define $$ gettext)

;; Parameters
(define page-format-history (make-parameter '()))
(define wiliki (make-parameter #f))     ;current instance
(define lang   (make-parameter #f))     ;current language
(define db     (make-parameter #f))     ;current database

(define (current-formatting-page)
  (let1 hist (page-format-history)
    (if (null? hist) #f (car hist))))

;; Class <wiliki> ------------------------------------------
;;   A main data structure that holds run-time information.
;;   Available as the value of the parameter wiliki in
;;   almost all locations.

(define-class <wiliki> ()
  ((db-path  :accessor db-path-of :init-keyword :db-path
             :init-value "wikidata.dbm")
   (db-type  :accessor db-type-of :init-keyword :db-type
             :initform <gdbm>)
   (title    :accessor title-of   :init-keyword :title
             :init-value "WiLiKi")
   (top-page :accessor top-page-of :init-keyword :top-page
             :init-value "TopPage")
   (language :accessor language-of :init-keyword :language
             :init-value 'jp)
   (charsets :accessor charsets-of :init-keyword :charsets
             :init-value ())
   (editable? :accessor editable?  :init-keyword :editable?
              :init-value #t)
   (style-sheet :accessor style-sheet-of :init-keyword :style-sheet
                :init-value #f)
   (image-urls :accessor image-urls-of :init-keyword :image-urls
               :init-value ())
   (description :accessor description-of :init-keyword :description
                :init-value "WiLiKi, a Wiki engine written in Scheme")
   (server-name :accessor server-name-of :init-keyword :server-name
                :init-form (or (sys-getenv "SERVER_NAME")
                               "localhost"))
   (script-name :accessor script-name-of :init-keyword :script-name
                :init-form (or (sys-getenv "SCRIPT_NAME")
                               "/wiliki.cgi"))
   (debug-level :accessor debug-level :init-keyword :debug-level
                :init-value 0)
   ;; log file path.  if specified, logging & history feature becomes
   ;; available.  If the name given doesn't hvae directory component,
   ;; it is regarded in the same directory as db-path.
   (log-file    :accessor log-file    :init-keyword :log-file
                :init-value #f)
   ;; customize edit text area size
   (textarea-rows :accessor textarea-rows-of :init-keyword :textarea-rows
                  :init-value 40)
   (textarea-cols :accessor textarea-cols-of :init-keyword :textarea-cols
                  :init-value 80)
   ))

(define (cgi-name-of wiliki)
  (sys-basename (script-name-of wiliki)))

(define (%url-format full? fmt args)
  (let ((self (wiliki))
        (fstr (if fmt #`"?,|fmt|&l=,(lang)" #`"?l=,(lang)")))
    (string-append
     (if full?
         #`"http://,(server-name-of self),(script-name-of self)"
         (cgi-name-of self))
     (if (null? args)
         fstr
         (apply format fstr
                (map (compose uri-encode-string x->string) args))))))

(define (url fmt . args) (%url-format #f fmt args))
(define (url-full fmt . args) (%url-format #t fmt args))

;; Creates a link to switch language
(define (language-link pagename)
  (receive (target label)
      (case (lang)
        ((jp) (values 'en "->English"))
        (else (values 'jp "->Japanese")))
    (html:a :href #`",(cgi-name-of (wiliki))?,|pagename|?l=,|target|"
            "[" (html-escape-string label) "]")))

;; fallback
(define-method title-of (obj) "WiLiKi")
(define-method debug-level-of (obj) 0)

;; Class <page> ---------------------------------------------
;;   Represents a page.
;;
(define-class <page> ()
  ((key   :init-keyword :key :accessor key-of)
   (ctime :initform (sys-time) :init-keyword :ctime :accessor ctime-of)
   (cuser :initform #f :init-keyword :cuser :accessor cuser-of)
   (mtime :initform #f :init-keyword :mtime :accessor mtime-of)
   (muser :initform #f :init-keyword :muser :accessor muser-of)
   (content :initform "" :init-keyword :content :accessor content-of)
   ))

;; Macros -----------------------------------------

(define (expand-writer-macros content)
  (with-string-io
   content
   (lambda ()
     (define (normal line)
       (cond ((eof-object? line))
             ((string=? line "{{{")
              (print line)
              (verbatim (read-line)))
             (else
              (display
               (regexp-replace-all
                #/\[\[($\w+)\]\]/ line
                (lambda (m) (tree->string (handle-writer-macro (m 1))))))
              (newline)
              (normal (read-line)))))
     (define (verbatim line)
       (cond ((eof-object? line) (print "}}}")) ;; close verbatim block
             ((string=? line "}}}")
              (print line) (normal (read-line)))
             (else
              (print line) (verbatim (read-line)))))

     (normal (read-line)))))

;; Character conv ---------------------------------

;; input conversion - get data from outside world
(define (cv-in str) (ces-convert str "*JP"))

;; output conversion - put data to outside world, according to charsets spec
(define (cv-out str)
  (ces-convert str (symbol->string (gauche-character-encoding))
               (output-charset)))

(define (output-charset)
  (or (and-let* (((wiliki))
                 (p (assoc (lang) (charsets-of (wiliki))))
                 ((symbol? (cdr p))))
        (cdr p))
      "EUC-JP")) ;; this is a fallback.

;; Logging ----------------------------------------

(define (log-file-path wiliki)
  (and wiliki
       (and-let* ((filename (log-file wiliki)))
         (if (or (string-prefix? "./" filename)
                 (string-prefix? "../" filename)
                 (string-prefix? "/" filename))
           filename
           (string-append (sys-dirname (db-path-of wiliki)) "/" filename)))))

;; NB: we assume write-log is always called during the main database
;; is locked, so we don't do any locking here.
(define (write-log wiliki pagename old new timestamp)
  (and-let* ((logfile (log-file-path wiliki)))
    (let ((content (wiliki-log-create
                    pagename new old
                    :timestamp timestamp
                    :remote-addr (or (sys-getenv "REMOTE_ADDR") "")
                    :remote-user (or (sys-getenv "REMOTE_USER") ""))
                   ))
      (call-with-output-file logfile
        (lambda (p) (display content p) (flush p))
        :if-exists :append)
      )))

;; CGI processing ---------------------------------

(define (html-page head-elements . body-elements)
  ;; NB: cgi-header should be able to handle extra header fields.
  ;; for now, I add extra headers manually.
  `("Content-Style-Type: text/css\n"
    ,(cgi-header
      :content-type #`"text/html; charset=,(output-charset)")
    ,(html-doctype :type :transitional)
    ,(html:html
      (html:head
       head-elements
       (or (and-let* ((w (wiliki))
                      (sv (server-name-of w))
                      (sc (script-name-of w)))
             (html:base :href #`"http://,|sv|,|sc|"))
           '())
       (or (and-let* ((w (wiliki)) (ss (style-sheet-of w)))
             (html:link :rel "stylesheet" :href ss :type "text/css"))
           ;; default
           "<style type=\"text/css\"> body { background-color: #eeeedd }</style>"))
      (html:body
       body-elements))))

(define (error-page e)
  (html-page
   (html:title #`",(title-of (wiliki)) : Error")
   (list (html:h1 "Error")
         (html:p (html-escape-string (ref e 'message)))
         (if (positive? (debug-level (wiliki)))
           (html:pre (html-escape-string
                      (call-with-output-string
                        (cut with-error-to-port <>
                             (cut report-error e)))))
           '())
         ))
  )

(define (redirect-page key)
  (cons "Status: 302 Moved\n"
        (cgi-header :location (url "~a" key))))

(define (conflict-page page pagename content donttouch)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Update Conflict"))
   `(,($$ "<p>It seems that somebody has updated this page while you're editing.  The most recent content is shown below.</p>")
     ,(html:hr)
     ,(format-colored-box (html:pre (html-escape-string (content-of page))))
     ,(html:hr)
     ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
     ,(edit-form #t pagename content (mtime-of page) donttouch)
     )
   :show-edit? #f :show-history? #f))

(define (cmd-view pagename)
  ;; NB: see the comment in format-wiki-name about the order of
  ;; wdb-get and virtual-page? check.
  (cond ((wdb-get (db) pagename) => (cut format-page pagename <>))
        ((virtual-page? pagename)
         (format-page pagename (handle-virtual-page pagename)
                      :show-edit? #f))
        ((equal? pagename (top-page-of (wiliki)))
         (let ((toppage (make <page> :key pagename :mtime (sys-time))))
           (wdb-put! (db) (top-page-of (wiliki)) toppage)
           (format-page (top-page-of (wiliki)) toppage)))
        ((or (string-index pagename #[\s\[\]])
             (string-prefix? "$" pagename))
         (error "Invalid page name" pagename))
        (else
         (format-page
          (string-append ($$ "Nonexistent page: ") pagename)
          `(,(html:p
              ($$ "Create a new page: ")
              (format-wiki-name pagename)))
          :show-edit? #f :show-history? #f))
        ))

(define (edit-form preview? pagename content mtime donttouch)
  (define (buttons)
    (if preview?
        `(,(html:input :type "submit" :name "preview" :value ($$ "Preview"))
          ,(html:input :type "submit" :name "commit" :value ($$ "Commit without preview")))
        `(,(html:input :type "submit" :name "preview" :value ($$ "Preview again"))
          ,(html:input :type "submit" :name "commit" :value ($$ "Commit")))))
  (define (donttouch-checkbox)
    `(,(apply html:input :type "checkbox" :name "donttouch" :value "on"
              (if donttouch '(:checked #t) '()))
      ,($$ "Don't update 'Recent Changes'")))
  
  (html:form
   :method "POST" :action (cgi-name-of (wiliki))
   (buttons) (donttouch-checkbox)
   (html:br)
   (html:input :type "hidden" :name "c" :value "c")
   (html:input :type "hidden" :name "p" :value pagename)
   (html:input :type "hidden" :name "l" :value (lang))
   (html:input :type "hidden" :name "mtime" :value mtime)
   (html:textarea :name "content"
                  :rows (textarea-rows-of (wiliki))
                  :cols (textarea-cols-of (wiliki))
                  (html-escape-string content))
   (html:br)
   (buttons)
   (html:br)
   ($$ "<h2>Text Formatting Rules</h2>
      <p>No HTML.</p>
      <p>A line begins with \";;\" doesn't appear in the output (comment).</p>
      <p>A line begins with \"~\" is treated as if it is continued
         from the previous line, except comments.  (line continuation).</p>
      <p>Empty line to separating paragraphs (&lt;p&gt;)
      <p>\"<tt>- </tt>\", \"<tt>-- </tt>\" and \"<tt>--- </tt>\" ... at the
         beginning of a line for an item of unordered list (&lt;ul&gt;).
         Put a space after dash(es).</p>
      <p>\"<tt># </tt>\", \"<tt>## </tt>\", \"<tt>### </tt>\" ... at the
         beginning of a line for an item of ordered list (&lt;ol&gt;).
         Put a space after <tt>#</tt>'s.</p>
      <p>A line with only \"<tt>----</tt>\" is &lt;hr&gt;.</p>
      <p>\"<tt>:item:description</tt>\" at the beginning of a line is &lt;dl&gt;.
         The item includes all colons but the last one.  If you want to include
         a colon in the description, put it in the next line.</p>
      <p><tt>[[Name]]</tt> to make \"Name\" a WikiName.  Note that
         a simple mixed-case word doesn't become a WikiName.
         \"Name\" beginning with \"$\" has special meanings (e.g. 
         \"[[$date]]\" is replaced for the time at the editing.)</p>
      <p>A URL-like string beginning with \"<tt>http:</tt>\" becomes
         a link.  \"<tt>[URL name]</tt>\" becomes a <tt>name</tt> that linked
         to <tt>URL</tt>.</p>
      <p>Surround words by two single quotes (<tt>''foo''</tt>)
         to emphasize.</p>
      <p>Surround words by three single quotes (<tt>'''foo'''</tt>)
         to emphasize more.</p>
      <p>\"<tt>*</tt>\", \"<tt>**</tt>\" and \"<tt>***</tt>\"' ... 
         at the beginning of a line is a header.  Put a space
         after the asterisk(s).</p>
      <p>Whitespace(s) at the beginning of line for preformatted text.</p>
      <p>A line of \"{{{\" starts verbatim text, which ends with
         a line of \"}}}\".
         No formatting is done in verbatim text.  Even comments and line
         continuation don't have effect.</p>
      <p>A line begins with \"||\" and also ends with \"||\" becomes a
         row of a table.  Consecutive rows forms a table.  Inside a row,
         \"||\" delimits columns.</p>
      <p>\"~%\" is replaced for \"&lt;br&gt;\".</p>
      <p>If you want to use special characters at the
         beginning of line, put six consecutive single quotes.
         It emphasizes a null string, so it's effectively nothing.</p>")
   ))

(define (cmd-edit pagename)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db) pagename #t)))
    (format-page pagename
                 (edit-form #t pagename
                            (content-of page) (mtime-of page) #f)
                 :show-edit? #f :show-lang? #f :show-history? #f)))

(define (cmd-preview pagename content mtime donttouch)
  (let ((page (wdb-get (db) pagename #t)))
    (if (or (not (mtime-of page)) (eqv? (mtime-of page) mtime))
        (format-page
         (format #f ($$ "Preview of ~a") pagename)
         `(,(format-colored-box (format-content (make <page>
                                                  :key pagename
                                                  :content content)))
           ,(html:hr)
           ,(edit-form #f pagename content mtime donttouch))
         :show-edit? #f :show-lang? #f :show-history? #f)
        (conflict-page page pagename content donttouch)
        )))

(define (cmd-commit-edit pagename content mtime donttouch)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((p   (wdb-get (db) pagename #t))
        (now (sys-time)))
    (if (or (not (mtime-of p)) (eqv? (mtime-of p) mtime))
        (if (string-every #[\s] content)
            (begin
              (write-log (wiliki) pagename (content-of p) "" now)
              (set! (content-of p) "")
              (wdb-delete! (db) pagename)
              (redirect-page (top-page-of (wiliki))))
            (let1 new-content (expand-writer-macros content)
              (write-log (wiliki) pagename (content-of p) new-content now)
              (set! (mtime-of p) now)
              (set! (content-of p) new-content)
              (wdb-put! (db) pagename p :donttouch donttouch)
              (redirect-page pagename)))
        (conflict-page p pagename content donttouch)
        )
    ))

(define (cmd-all)
  (format-page
   (string-append (title-of (wiliki))": "($$ "All Pages"))
   (html:ul
    (map (lambda (k) (html:li (format-wikiname-anchor k)))
         (sort (wdb-map (db) (lambda (k v) k)) string<?)))
   :page-id "c=a"
   :show-edit? #f
   :show-all? #f
   :show-history? #f))

(define (cmd-recent-changes)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Recent Changes"))
   (html:table
    (map (lambda (p)
           (html:tr
            (html:td (format-time (cdr p)))
            (html:td "(" (how-long-since (cdr p)) " ago)")
            (html:td (format-wikiname-anchor (car p)))))
         (wdb-recent-changes (db))))
   :page-id "c=r"
   :show-edit? #f
   :show-recent-changes? #f
   :show-history? #f))

(define (cmd-search key)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Search results"))
   (html:ul
    (map (lambda (p)
           (html:li
            (format-wikiname-anchor (car p))
            (or (and-let* ((mtime (get-keyword :mtime (cdr p) #f)))
                  #`"(,(how-long-since mtime))")
                "")))
         (wdb-search-content (db) key)))
   :page-id (format #f "c=s&key=~a" (html-escape-string key))
   :show-edit? #f
   :show-history? #f))

(define (cmd-lwp-view key)
  (let ((page (wdb-get (db) key #f)))
    `(,(cgi-header
        :content-type #`"text/plain; charset=,(output-charset)")
      ,#`"title: ,|key|\n"
      ,#`"wiliki-lwp-version: ,|*lwp-version*|\n"
      ,(if page
           `(,#`"mtime: ,(mtime-of page)\n"
             "\n"
             ,(content-of page))
           `(,#`"mtime: 0\n"
             "\n")))))

;; Retrieve requested page name.
;; The pagename can be specified in one of the following ways:
;;
;;  * Using request path
;;      http://foo.net/wiliki.cgi/PageName
;;  * Using cgi 'p' parameter
;;      http://foo.net/wiliki.cgi?l=jp&p=PageName
;;  * Using cgi parameter - in this case, PageName must be the
;;    first parameter before any other CGI parameters.
;;      http://foo.net/wiliki.cgi?PageName
;;
;; The url is tested in the order above.  So the following URL points
;; the page "Foo".
;;      http://foo.net/wiliki.cgi/Foo?Bar&p=Baz
;;
;; If no page is given, the top page of WiLiKi is used.
;; If the url main component ends with '/', it is regareded as a
;; top page, e.g. the following points to the toppage.
;;      http://foo.net/wiliki.cgi/?Bar&p=Baz


(define (get-page-name wiki param)

  ;; Extract the extra components of REQUEST_URI after the CGI name.
  ;; If the path components
  (define (get-path)
    (and-let* ((uri (sys-getenv "REQUEST_URI"))
               (script (script-name-of wiki))
               (path (string-scan uri #`",|script|/" 'after))
               (conv (cv-in (uri-decode-string
                             (or (string-scan path "?" 'before) path)))))
      (if (equal? conv "")
        (top-page-of wiki)
        conv)))

  (cond ((get-path))
        ((cgi-get-parameter "p" param :default #f :convert cv-in))
        ((and (pair? param) (pair? (car param)) (eq? (cadar param) #t))
         (cv-in (caar param)))
        (else (top-page-of wiki)))
  )

;; Entry ------------------------------------------

(define-method wiliki-main ((self <wiliki>))
  (cgi-main
   (lambda (param)
     (let ((pagename (get-page-name self param))
           (command  (cgi-get-parameter "c" param))
           (language (cgi-get-parameter "l" param :convert string->symbol)))
       (parameterize
           ((wiliki self)
            (lang   (or language (language-of self))))
        (cgi-output-character-encoding (output-charset))
        (textdomain (lang))
        (cond
         ;; command may #t if we're looking at the page named "c".
         ((or (not command) (eq? command #t))
          (with-db (cut cmd-view pagename)))
         ((equal? command "lv")
          (with-db (cut cmd-lwp-view pagename)))
         ((equal? command "e")
          (with-db (cut cmd-edit pagename)))
         ((equal? command "a")
          (with-db cmd-all))
         ((equal? command "r")
          (with-db cmd-recent-changes))
         ((equal? command "h")
          (with-db (cut cmd-history pagename)))
         ((equal? command "hd")
          (with-db (cut cmd-diff pagename
                        (cgi-get-parameter "t" param :convert x->integer))))
         ((equal? command "s")
          (with-db
           (cut cmd-search (cgi-get-parameter "key" param :convert cv-in))))
         ((equal? command "c")
          (with-db
           (cut
            (if (cgi-get-parameter "commit" param :default #f)
              cmd-commit-edit
              cmd-preview)
            pagename
            (cgi-get-parameter "content" param :convert cv-in)
            (cgi-get-parameter "mtime" param
                               :convert x->integer
                               :default 0)
            (cgi-get-parameter "donttouch" param :default #f))
           :write))
         ((equal? command "rss")
          (with-db (cut rss-page (db))))
         (else (error "Unknown command" command))
         ))))
   :merge-cookies #t
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
