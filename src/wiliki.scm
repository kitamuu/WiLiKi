;;;
;;; WiLiKi - Wiki in Scheme
;;;
;;;  Copyright (c) 2000-2009  Shiro Kawai  <shiro@acm.org>
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

(define-module wiliki
  (use dbm)
  (use gauche.charconv)
  (use gauche.parameter)
  (use gauche.sequence)
  (use gauche.version)
  (use rfc.uri)
  (use scheme.list)
  (use srfi.13)
  (use srfi.27)
  (use text.gettext)
  (use text.html-lite)
  (use text.tr)
  (use text.tree)
  (use util.match)
  (use wiliki.db)
  (use wiliki.format)
  (use wiliki.macro)
  (use wiliki.page)
  (use www.cgi)
  (extend wiliki.core)
  (export <wiliki> wiliki-main
          <wiliki-formatter>
          wiliki:language-link wiliki:make-navi-button
          wiliki:top-link wiliki:edit-link wiliki:history-link
          wiliki:all-link wiliki:recent-link wiliki:search-box
          wiliki:menu-links wiliki:page-title wiliki:breadcrumb-links
          wiliki:wikiname-anchor wiliki:wikiname-anchor-string
          wiliki:get-formatted-page-content
          wiliki:recent-changes-alist
          wiliki:page-lines-fold
          wiliki:lang
          wiliki:version
          wiliki:self-url
          )
  )
(select-module wiliki)
(random-source-randomize! default-random-source)

;; Load extra code only when needed.
(autoload wiliki.rss     rss-page)
(autoload wiliki.pasttime how-long-since)
(autoload wiliki.macro decompose-key entry-title)
(autoload wiliki.log     wiliki-log-create wiliki-log-pick
                         wiliki-log-pick-from-file
                         wiliki-log-parse-entry wiliki-log-entries-after
                         wiliki-log-diff wiliki-log-diff*
                         wiliki-log-revert wiliki-log-revert*
                         wiliki-log-recover-content
                         wiliki-log-merge)

;; Less frequently used commands are separated to subfiles.
(autoload "wiliki/history" cmd-history cmd-diff cmd-viewold)
(autoload wiliki.edit      cmd-edit cmd-preview cmd-commit-edit)
(autoload "wiliki/version" wiliki:version)

;; Some constants

(define *lwp-version* "1.0")            ;''lightweight protocol'' version
(define $$ gettext)

;; compatibility stuff

;; wiliki accessors.  They're now obsolete; using ref is recommended.
(define (db-path-of w)     (~ w'db-path))
(define (db-type-of w)     (~ w'db-type))
(define (title-of w)       (if w (~ w'title) ""))
(define (top-page-of w)    (~ w'top-page))
(define (language-of w)    (if w (~ w'language) 'en))
(define (charsets-of w)    (~ w'charsets))
(define (editable? w)      (~ w'editable?))
(define (style-sheet-of w) (~ w'style-sheet))
(define (image-urls-of w)  (~ w'image-urls))
(define (description-of w) (~ w'description))
(define (protocol-of w)    (~ w'protocol))
(define (server-name-of w) (~ w'server-name))
(define (server-port-of w) (~ w'server-port))
(define (script-name-of w) (~ w'script-name))
(define (debug-level w)    (if w (~ w'debug-level) 0))
(define (gettext-paths w)  (~ w'gettext-paths))
(define (textarea-rows-of w) (~ w'textarea-rows)) ;; obsoleted
(define (textarea-cols-of w) (~ w'textarea-cols)) ;; obsoleted

(define redirect-page      wiliki:redirect-page)

(define log-file-path      wiliki:log-file-path)

;; NB: compatibility kludge - this may return wrong answer
;; if W is not the current wiliki, but I bet switching two
;; wiliki instances are pretty rare.
(define (cgi-name-of w) (and w (wiliki:url)))
(define (full-script-path-of w) (and w (wiliki:url :full)))

(define (url fmt . args) (apply wiliki:url fmt args))
(define (url-full fmt . args) (apply wiliki:url :full fmt args))
(define wiliki:self-url  url)

;;;==================================================================
;;; Actions
;;;

;;
;; View page
;;
(define-wiliki-action v :read (pagename)
  ;; NB: see the comment in format-wikiname about the order of
  ;; wiliki-db-get and virtual-page? check.
  (cond [(wiliki:db-get pagename) => html-page]
        [(virtual-page? pagename) (html-page (handle-virtual-page pagename))]
        [(equal? pagename (top-page-of (wiliki)))
         (let1 toppage (make <wiliki-page>
                         :title pagename :key pagename :mtime (sys-time))
           ;; Top page is non-existent, or its name may be changed.
           ;; create it automatically.  We need to ensure db is writable.
           (if (editable? (wiliki))
             (wiliki:with-db (^[]
                               (wiliki:db-put! (~ (wiliki)'top-page) toppage)
                               (html-page toppage))
                             :rwmode :write)
             (errorf "Top-page #f (~a) doesn't exist, and the database \
                      is read-only" toppage)))]
        [(or (string-index pagename #[\[\]])
             (#/^\s|\s$/ pagename)
             (string-prefix? "$" pagename))
         (error "Invalid page name" pagename)]
        [else
         `(,(cgi-header :status 404)
           ,(html:html
             (html:head (html:title "Entry Not Found"))
             (html:body
               (html:p #"~|pagename| is not found"))))]
        ))

(define-wiliki-action lv :read (pagename)
  (let ((page (wiliki:db-get pagename #f)))
    `(,(cgi-header
        :content-type #"text/plain; charset=~(output-charset)")
      ,#"title: ~|pagename|\n"
      ,#"wiliki-lwp-version: ~|*lwp-version*|\n"
      ,(if page
         `(,#"mtime: ~(~ page 'mtime)\n"
           "\n"
           ,(~ page 'content))
         `("mtime: 0\n"
           "\n")))))

;;
;; All pages, recent changes, RSS
;;
(define-wiliki-action a :read (_)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "All Pages"))
     :command "c=a"
     :content `((ul ,@(map (^k `(li ,(wiliki:wikiname-anchor k)))
                           (sort (wiliki:db-map (^[k v] k)) string<?))))
     )))

(define-wiliki-action r :read (_)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "Recent Changes"))
     :command "c=r"
     :content
     `((table
        ,@(map (^p `(tr
                     (td ,(wiliki:format-time (cdr p)))
                     (td "(" ,(how-long-since (cdr p)) " ago)")
                     (td ,(wiliki:wikiname-anchor (car p)))))
               (wiliki:db-recent-changes))))
     )))

(define-wiliki-action rss :read (_
                                 (type :default #f))
  (rss-page :item-description (cond
                               [(member type '("html" "html-partial"
                                               "raw" "raw-partial" "none"))
                                (string->symbol type)]
                               [else #f])))

;;
;; Search
;;
(define-wiliki-action s :read (_ (key :convert cv-in))
  (if (equal? (cgi-get-metavariable "REQUEST_METHOD") "POST")
    (html-page
     (make <wiliki-page>
       :title (string-append (title-of (wiliki))": "
                             (format ($$ "Search results of \"~a\"") key))
       :command (format #f "c=s&key=~a" (html-escape-string key))
       :content
       `((h2 ,(format (gettext "~sの検索結果") key))
	 (ul
           ,@(map (^p
                  (let1 page (wiliki:db-get (car p))
                    `(li (a (@ (href ,#"~(wiliki:url)/~(car p)"))
                        ,(apply format "~4d/~2,'0d/~2,'0d" (decompose-key (car p))) " : "
                            ,(entry-title (car p) (~ page 'content))))
		    ))
                 (wiliki:db-search-content key))))
       ))
    (html-page
     (make <wiliki-page>
       :title (string-append (title-of (wiliki))": search")
       :command (format #f "c=s&key=~a" (html-escape-string key))
       :content (wiliki:search-box key)))))

;;
;; Edit and commit
;;   We redirect GET request to the edit action to the normal view,
;;   since it is bothering that search engines pick the link to the edit
;;   page.  (We allow GET with t parameter, since edit history page
;;   contains such links.)
;;   The 'n' action is only used from the link of creating a new page.
;;   It returns the normal view if the named page already exists.
(define-wiliki-action e :read (pagename
                               (t :convert x->integer :default #f))
  (if (or t
          (and-let* ([m (cgi-get-metavariable "REQUEST_METHOD")])
            (string-ci=? m "POST")))
    (cmd-edit pagename t)
    (wiliki:redirect-page pagename)))


(define-wiliki-action n :read (pagename)
  (if (wiliki:db-exists? pagename)
    (wiliki:redirect-page pagename)
    (cmd-edit pagename #f)))

(define-wiliki-action c :write (pagename
                                (commit :default #f)
                                (content :convert cv-in)
                                (mtime   :convert x->integer :default 0)
                                (logmsg  :convert cv-in)
                                (donttouch :default #f))
  ((if commit cmd-commit-edit cmd-preview)
   pagename content mtime logmsg donttouch #f))

;;
;; History
;;
(define-wiliki-action h :read (pagename
                               (s :convert x->integer :default 0))
  (cmd-history pagename s))

(define-wiliki-action hd :read (pagename
                                (t  :convert x->integer :default 0)
                                (t1 :convert x->integer :default 0))
  (cmd-diff pagename t t1))

(define-wiliki-action hv :read (pagename
                                (t  :convert x->integer :default 0))
  (cmd-viewold pagename t))

;;================================================================
;; WiLiKi-specific formatting routines
;;

;; Creates a link to switch language
(define (wiliki:language-link page)
  (and-let* ([target (or (~ page 'command) (~ page 'key))])
    (receive (language label)
        (case (wiliki:lang)
          [(jp) (values 'en "->English")]
          [(vi) (values 'vi "->Vietnamese")]
          [else (values 'jp "->Japanese")])
      `(a (@ (href ,(string-append (cgi-name-of (wiliki)) "?" target
                                   (lang-spec language '&))))
          "[" ,label "]"))))

;; Navigation buttons
(define (wiliki:make-navi-button params content)
  `(form (@ (method POST) (action ,(cgi-name-of (wiliki)))
            (style "margin:0pt; padding:0pt"))
         ,@(map (match-lambda
                  [(n v) `(input (@ (type hidden) (name ,n) (value ,v)))])
                params)
         (input (@ (type submit) (class "navi-button") (value ,content)))))

(define (wiliki:top-link page)
  (and (not (equal? (~ page 'title) (top-page-of (wiliki))))
       (wiliki:make-navi-button '() ($$ "Top"))))

(define (wiliki:edit-link page)
  (and (eq? (~ (wiliki) 'editable?) #t)
       (wiliki:persistent-page? page)
       (wiliki:make-navi-button `((p ,(~ page 'key)) (c e)) ($$ "Edit"))))

(define (wiliki:history-link page)
  (and (~ (wiliki) 'log-file)
       (wiliki:persistent-page? page)
       (wiliki:make-navi-button `((p ,(~ page 'key)) (c h)) ($$ "History"))))

(define (wiliki:back-link page)
  (and (wiliki:persistent-page? page)
       (wiliki:make-navi-button `((key ,#"[[~(~ page'key)]]") (c s))
                                ($$ "Links to here"))))

(define (wiliki:all-link page)
  (and (not (equal? (~ page 'command) "c=a"))
       (wiliki:make-navi-button '((c a)) ($$ "All"))))

(define (wiliki:recent-link page)
  (and (not (equal? (~ page 'command) "c=r"))
       (wiliki:make-navi-button '((c r)) ($$ "Recent Changes"))))

(define (wiliki:search-box :optional (key #f))
  `((form (@ (method POST) (action ,(cgi-name-of (wiliki)))
             (class "search-form"))
          (input (@ (type hidden) (name c) (value s)))
          (label (input (@ (type text) (name key) (size 18) (placeholder "キーワード検索")
                    ,@(cond-list
                       [key `(value ,key)])
                    (class "search-box"))))
          (button (@ (type submit) (name search)
                    (class "search-button")))
          )))

(define (wiliki:breadcrumb-links page delim)
  (define (make-link-comp rcomps acc)
    (if (null? acc)
      (list (car rcomps))
      (cons (wiliki:wikiname-anchor (string-join (reverse rcomps) delim)
                                    (car rcomps))
            acc)))
  (let1 combs (string-split (~ page 'title) delim)
    (if (pair? (cdr combs))
      `((span (@ (class "breadcrumb-links"))
              ,@(intersperse
                 delim
                 (pair-fold make-link-comp '() (reverse combs)))))
      '())))

(define (wiliki:menu-links page)
  (define (td x) (list 'td x))
  `((table
     (@ (border 0) (cellpadding 0))
     (tr ,@(cond-list
            [(wiliki:top-link page) => td]
            [(wiliki:edit-link page) => td]
            [(wiliki:history-link page) => td]
            [(wiliki:back-link page) => td]
            [(wiliki:all-link page) => td]
            [(wiliki:recent-link page) => td])
         (td ,@(wiliki:search-box))))))

(define (wiliki:page-title page)
  `((h1 ,(~ page 'title))))

(define (wiliki:default-page-header page opts)
  `(,@(wiliki:page-title page)
    (div (@ (align "right")) ,@(wiliki:breadcrumb-links page ":"))
    (div (@ (align "right")) ,@(wiliki:menu-links page))
    (hr)))

(define (wiliki:default-page-footer page opts)
  (if (~ page 'mtime)
    `((hr)
      (div (@ (align right))
           ,($$ "Last modified : ")
           ,(wiliki:format-time (~ page 'mtime))))
    '()))

(define (wiliki:default-head-elements page opts)
  (let1 w (wiliki)
    `((title ,(wiliki:format-head-title (the-formatter) page))
      ,@(cond-list
         [w `(base (@ (href ,(wiliki:url :top))))]
         [w `(link (@ (rel "alternate") (type "application/rss+xml")
                      (title "RSS") (href ,(wiliki:url :full "c=rss"))))]
         [w `(link (@ (rel "canonical")
                      (href ,(if (or (not (~ page'key)) (equal? (~ page'key) (~ w'top-page)))
                              (wiliki:url :top)
                              #"~(wiliki:url :full)/~(~ page'key)"))))]
         [w `(meta (@ (property "og:type") (content "website")))]
         [w `(meta (@ (property "og:url") (content ,(wiliki:url :top))))]
         [w `(meta (@ (property "og:image") (content ,#"~(wiliki:url :top)/assets/top.png")))]
         [w `(meta (@ (name "viewport") (content "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0")))]
         [w `(link (@ (rel "stylesheet") (href "https://cdnjs.cloudflare.com/ajax/libs/drawer/3.2.2/css/drawer.min.css")))]
         [w `(link (@ (rel "stylesheet") (href "https://use.fontawesome.com/releases/v5.15.4/css/all.css")))]
         [w `(script (@ (src "https://cdn.jsdelivr.net/npm/lazyload@2.0.0-rc.2/lazyload.js")))]
         [w `(script (@ (src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")))]
         [w `(script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/iScroll/5.2.0/iscroll.min.js")))]
         [w `(script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/drawer/3.2.2/js/drawer.min.js")))]
         [w `(script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/js/bootstrap.min.js")))]
         [w `(script (@ (async "async") (src "https://www.googletagmanager.com/gtag/js?id=G-H5PNCVLGVB")))]
         [w `(script (@ (async "async") (src "https://platform.twitter.com/widgets.js")))]
         [w `(script (@ (async "async") (src "//cdn.iframe.ly/embed.js")))]
         [w `(script (@ (src ,(string-append "assets/blog.js?" (substring (number->string (random-real)) 2 16)))))]
         [(and w (~ w'style-sheet))
          => @(^[ss] (map (^s `(link (@ (rel "stylesheet")
                                        (href ,(string-append s "?" (substring (number->string (random-real)) 2 16))) (type "text/css"))))
                          (if (list? ss) ss (list ss))))])
      )))

(define (default-format-time time)
  (if time
    (if (zero? time)
      ($$ "Epoch")
      (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time)))
    "-"))

(define (default-format-wikiname name)
  (define (inter-wikiname-prefix head)
    (and-let* ([page (wiliki:db-get "InterWikiName")]
               [rx   (string->regexp #"^:~(regexp-quote head):\\s*")])
      (call-with-input-string (~ page 'content)
        (^p (let loop ((line (read-line p)))
              (cond [(eof-object? line) #f]
                    [(rx line) =>
                     (^m (let1 prefix (m 'after)
                           (if (string-null? prefix)
                             (let1 prefix (read-line p)
                               (if (or (eof-object? prefix)
                                       (string-null? prefix))
                                 #f
                                 (string-trim-both prefix)))
                             (string-trim-both prefix))))]
                    [else (loop (read-line p))]))))))
  (define (reader-macro-wikiname? name)
    (cond [(string-prefix? "$$" name) (handle-reader-macro name)]
          [(or (string-prefix? "$" name)
               (#/^\s/ name)
               (#/\s$/ name))
           ;;invalid wiki name
           (list "[[" name "]]")]
          [else #f]))
  (define (inter-wikiname? name)
    (receive (head after) (string-scan name ":" 'both)
      (or (and head
               (and-let* ([inter-prefix (inter-wikiname-prefix head)])
                 (values inter-prefix after)))
          (values #f name))))
  (or (reader-macro-wikiname? name)
      (receive (inter-prefix real-name) (inter-wikiname? name)
        (cond [inter-prefix
               (let1 scheme
                   (if (#/^(https?|ftp|mailto):/ inter-prefix) "" "http://")
                 `((a (@ (href ,(format "~a~a~a" scheme inter-prefix
                                        (uri-encode-string
                                         (cv-out real-name)))))
                      ,name)))]
              ;; NB: the order of checks here is debatable.  Should a virtual
              ;; page shadow an existing page, or an existing page shadow a
              ;; virtual one?  Note also the order of this check must match
              ;; the order in cmd-view.
              [(or (wiliki:db-exists? real-name) (virtual-page? real-name))
               (list (wiliki:wikiname-anchor real-name))]
              [else
               `(,real-name
                 (a (@ (href ,(url "p=~a&c=n" (cv-out real-name)))) "?"))]))
      )
  )

;; Ideally, default-format-wikiname &c should be defined as a method
;; specialized for <wiliki-formatter>.  However we need to keep the
;; old protocol for the backward compatibility; existing wiliki app
;; may customize the formatter by setting slots.  Newly written scripts
;; should customize by subclassing <wiliki-formatter>, *not* by setting
;; slots.
(define-class <wiliki-formatter> (<wiliki-formatter-base>)
  (;; for backward compatibility.
   (bracket :init-value default-format-wikiname)
   (time    :init-value default-format-time)
   (header  :init-value wiliki:default-page-header)
   (footer  :init-value wiliki:default-page-footer)
   (head-elements :init-value wiliki:default-head-elements)))

(wiliki:formatter (make <wiliki-formatter>)) ;override the default

;; Character conv ---------------------------------

(define cv-in wiliki:cv-in)

(define cv-out wiliki:cv-out)

(define output-charset wiliki:output-charset)

;; CGI processing ---------------------------------

(define html-page wiliki:std-page) ; for backward compatibility

(provide "wiliki")

