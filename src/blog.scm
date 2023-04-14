;; -*- coding: utf-8 -*-

;; Simple blog app sample based on WiLiKi

#|
 Setup:

  - Put this file in gosh's load path.

  - Prepare a password file by wiliki-passwd command.  It is used for
    authentication.

  - Create a cgi script something like this.

    #!/path/to/gosh
    (use wiliki)
    (use blog)
    (define (main args)
      (wiliki-main
       (make <blog>
         ;; Options specific to <blog>
         :author "YourName"
         :login-command "something"
         :auth-db-path "/path/to/passwd-file"

         ;; Usual WiKiKi options.  Don't set :editable?, for it is set by
         ;; 'initialize' method of <blog>.
         :db-path "/path/to/blog-file.dbm"
         :log-file "blog-file.log"
         :event-log-file "blog-events.log"
         :top-page "My Blog"
         :title "My Blog"
         :description "My Blog"
         :style-sheet "blog.css"
         :language 'jp
         :charsets '((jp . utf-8) (en . utf-8))
         :image-urls '((#/^https?:\/\/[-\w.]*amazon.com\// allow)
                       (#/^\// allow))
         :debug-level 1
         )))

  - 'Login-command' value is used to access the login screen.  To add
     and edit blog contents, you have to log in first.  Use the
     following url, where substituting LOGIN-COMMAND to the word
     you give in this option, to get to the login form.

     http://your-path-to-cgi-script/?c=LOGIN-COMMAND

     This allows each blog script to have different url for login form,
     making automated login attack more difficult.

  - 'Auth-db-path' option points to the file created by wiliki-passwd
     command, which contains username and hashed password pairs.

 Caveat:
   Currently we assume <blog> instance is created for every request.
   See the 'initialize' method of <blog>.
|#


;; An blog entry is just a wiki page with the following name:
;;  YYYYMMDD[C]-TITLE
;; 'C' is #\a, #\b, #\c, ... just to make sorting easier when there's
;; more than one entry per day.

(define-module blog
  (use gauche.parameter)
  (use scheme.list)
  (use srfi.13)
  (use srfi.19)
  (use srfi.27)
  (use srfi.42)
  (use rfc.cookie)
  (use rfc.uri)
  (use rfc.http)
  (use www.cgi)
  (use text.tree)
  (use text.html-lite)
  (use util.match)
  (use file.util)
  (use wiliki)
  (use wiliki.auth :prefix auth:)
  (use wiliki.parse)
  (use wiliki.page)
  (use wiliki.edit)
  (use wiliki.rss)
  (use wiliki.log)
  (use wiliki.macro)
  (export <blog> blog-option-ref))
(select-module blog)
(random-source-randomize! default-random-source)
(autoload wiliki.macro decompose-key entry-title authenticated?)

;;;
;;; Authentication
;;;

;; Authentication is handled by a special command, whose name is
;; given to the initarg option of <blog> object.  This will make it
;; difficult for automated login attempt.

(define (register-login-action command)
  ($ (with-module wiliki.core wiliki-action-add!)
     (string->symbol command)
     (^[pagename params]
       (let ([user (cgi-get-parameter "user" params)]
             [pass (cgi-get-parameter "pass" params)])
         (if (auth:auth-valid-password? user pass)
           (let1 sess (auth:auth-new-session user)
             `(,(cgi-header :status 302
                            :location (cgi-get-metavariable "SCRIPT_NAME")
                            :cookies (construct-cookie-string
                                      `(("sess" ,sess
                                         :max-age 20000000
                                         :path "/"))))))
           `(,(cgi-header)
             ,(html:html
               (html:head (html:title "Login"))
               (html:body
                ($ html:form :action "" :method "POST"
                   (html:input :type "hidden" :name "c" :value command)
                   (html:table
                    (html:tr
                     (html:td "Name")
                     (html:td (html:input :type "text" :name "user")))
                    (html:tr
                     (html:td "Password")
                     (html:td (html:input :type "password" :name "pass")))
                    (html:tr
                     (html:td (html:input :type "submit" :name "submit"
                                          :value "login")))))))))))))

;;;
;;; New entry
;;;
(define (new-entry-button)
  `((form (@ (method POST) (action ,(wiliki:url))
             (style "margin:0pt; padding:0pt"))
          (input (@ (type hidden) (name p) (value "NewEntry")))
          (input (@ (type submit) (class navi-button) (value "New"))))))

(define-virtual-page (#/^NewEntry$/ (_))
  (let* ([now (date->string (current-date) "~Y~m~d")]
         [pgs (wiliki:db-search (^(k v) (string-prefix? now k))
                                (^(a b) (string>? (car a) (car b))))]
         [suffix (match pgs
                   [() ""]              ;first entry of the day
                   [((key . _) . _) (pick-next-suffix key)])])
    (if (authenticated?)
      `((h2 "Create new entry")
        (form (@ (method POST) (action ,(wiliki:url)))
              (input (@ (type hidden) (name c) (value n)))
              (input (@ (type text) (name p) (value ,#"~|now|~|suffix|-")
                        (size 50)))
              (input (@ (type submit) (name submit) (value "Create")))))
      '())))

(define (pick-next-suffix key)
  (rxmatch-case key
    [#/^\d{8}([a-z])/ (#f c)
     (integer->char (+ (char->integer (string-ref c 0)) 1))]
    [else #\a]))

;;;
;;; Index
;;;

;; index   : (tree next-link prev-link)
;; tree    : ((year yearly ...) ....)
;; yearly  : ((month monthly ...) ...)
;; monthly : ((day #(key title fwd back)) ...)

(define (make-index-entry key title) (vector key title #f #f))
(define (index-entry-key e)   (vector-ref e 0))
(define index-entry-title
  (getter-with-setter (^e (vector-ref e 1)) (^(e v) (vector-set! e 1 v))))
(define index-entry-next
  (getter-with-setter (^e (vector-ref e 2)) (^(e v) (vector-set! e 2 v))))
(define index-entry-prev
  (getter-with-setter (^e (vector-ref e 3)) (^(e v) (vector-set! e 3 v))))

(define *index-store* " blog-index")

(define (build-index)
  (define root (list #f))
  (define (ensure-year year)
    (or (assv year root) (rlet1 node (list year) (push! (cdr root) node))))
  (define (ensure-year-month year month)
    (let1 ynode (ensure-year year)
      (or (assv month ynode) (rlet1 node (list month) (push! (cdr ynode) node)))))
  (define (ensure-year-month-day year month day)
    (let1 mnode (ensure-year-month year month)
      (or (assv day mnode) (rlet1 node (list day) (push! (cdr mnode) node)))))
  (define (ensure-entry year month day key title)
    (let1 dnode (ensure-year-month-day year month day)
      (or (find (^e (equal? key (index-entry-key e))) (cdr dnode))
          (rlet1 entry (make-index-entry key title)
            (push! (cdr dnode) entry)))))
  (define (build-tree)
    (wiliki:db-for-each
     (^(key content)
       (match (decompose-key key)
         [(y m d)
          (let1 in (open-input-string content)
            (read in)     ;discard metadata
            (ensure-entry y m d key
                          (entry-title key (get-remaining-input-string in))))]
         [_ #f]))))
  (define (build-links)
    (let ([first #f]
          [prev #f])
      (let1 last (fold-ec prev
                          (:list ynode (sort-by (cdr root) car))
                          (:list mnode (sort-by (cdr ynode) car))
                          (:list dnode (sort-by (cdr mnode) car))
                          (:list entry (sort-by (cdr dnode) index-entry-key))
                          entry
                          (^(entry prev)
                            (if prev
                              (set! (index-entry-next prev) entry)
                              (set! first entry))
                            (set! (index-entry-prev entry) prev)
                            entry))
        (list first last))))

  (build-tree)
  (cons (cdr root) (build-links)))

(define (save-index)
  (wiliki:db-raw-put! *index-store* (write-to-string (blog-index) write/ss)))

(define (read-index)
  (read-from-string (wiliki:db-raw-get *index-store* "(() #f #f)")))

(define blog-index
  (let1 cached #f
    (case-lambda
      [() (or cached
              (rlet1 idx (read-index)
                (set! cached idx)))]
      [(new) (set! cached #f) (blog-index)])))
(define blog-index-root
  (getter-with-setter (^() (car (blog-index)))
                      (^v  (set-car! (blog-index) v))))
(define blog-index-first
  (getter-with-setter (^() (cadr (blog-index)))
                      (^v  (set! (cadr (blog-index)) v))))
(define blog-index-last
  (getter-with-setter (^() (caddr (blog-index)))
                      (^v  (set! (caddr (blog-index)) v))))

(define (get-index-entry key :optional (create #f))
  (match (and key (decompose-key key))
    [(y m d)
     (and-let* ([ynode (or (assv y (blog-index-root))
                           (and create (rlet1 z (list y)
                                         (set! (blog-index-root)
                                               (cons z (blog-index-root))))))]
                [mnode (or (assv m (cdr ynode))
                           (and create (rlet1 z (list m) (push! (cdr ynode) z))))]
                [dnode (or (assv d (cdr mnode))
                           (and create (rlet1 z (list d) (push! (cdr mnode) z))))])
       (or (find (^e (equal? key (index-entry-key e))) (cdr dnode))
           (and create
                (rlet1 e (make-index-entry key "")
                  (push! (cdr dnode) e)
                  (update-blog-index-links! key e)))))]
    [_ #f]))

(define (update-blog-index-links! key e)
  (let1 last (blog-index-last) ;adjust prev/next links
    (if (not last)
      (begin (set! (blog-index-first) e) (set! (blog-index-last) e))
      (let loop ([last last])
        (cond [(not last)
               (set! (index-entry-next e) (blog-index-first))
               (set! (blog-index-first) e)]
              [(string>=? key (index-entry-key last))
               (if-let1 n (index-entry-next last)
                 (begin (set! (index-entry-prev n) e)
                        (set! (index-entry-next e) n))
                 (set! (blog-index-last) e))
               (set! (index-entry-prev e) last)
               (set! (index-entry-next last) e)]
              [else (loop (index-entry-prev last))])))))

(define (rebuild-index-button)
  `((form (@ (method POST) (action ,(wiliki:url))
             (style "margin:0pt; padding:0pt"))
          (input (@ (type hidden) (name p) (value rebuild-index)))
          (input (@ (type submit) (class navi-button) (value "Rebuild Index"))))))

(define (show-index-button)
  `((form (@ (method POST) (action ,(wiliki:url))
             (style "margin:0pt; padding:0pt"))
          (input (@ (type hidden) (name p) (value show-index)))
          (input (@ (type submit) (class navi-button) (value "Show Index"))))))

(define (manage-comments-button)
  `((form (@ (method GET) (action ,(build-path (wiliki:url) "ManageComments:0"))
             (style "margin:0pt; padding:0pt"))
          (input (@ (type submit) (class navi-button) (value "Manage Comments"))))))

(define-wiliki-action rebuild-index :write (pagename)
  (when (authenticated?)
    (blog-index (build-index)) (save-index)
    (blog-recent-entries-update!)
    (blog-recent-comments-update!))
  (wiliki:redirect-page pagename))

(define-wiliki-action show-index :read (_)
  (wiliki:std-page
   (make <blog-page>
     :title "Index"
     :content
     (cond [(authenticated?)
            (match-let1 (root first last) (read-index)
              (define (ent entry)
                `(li (a (@ (href ,#"~(wiliki:url)/~(index-entry-key entry)"))
                        ,(index-entry-key entry)" : ",(index-entry-title entry))))
              (define (d-ent dnode)
                `(li ,(x->string (car dnode))
                     (ul ,@(map ent (sort-by (cdr dnode) index-entry-key)))))
              (define (m-ent mnode)
                `(li ,(x->string (car mnode))
                     (ul ,@(map d-ent (sort-by (cdr mnode) car)))))
              (define (y-ent ynode)
                `(li ,(x->string (car ynode))
                     (ul ,@(map m-ent (sort-by (cdr ynode) car)))))
              `((ul ,@(map y-ent (sort-by root car)))))]
           [else
            `(p "Only the blog author can do this action.")]))))

;;;
;;; Recent entries/comments
;;;

(define *recent-entries* " recent-entries")
(define *recent-comments* " recent-comments")

(define (blog-recent-entries)
  (read-from-string (wiliki:db-raw-get *recent-entries* "()")))

(define (blog-recent-entry-add! key title)
  (let1 es (remove (^(e) (equal? (caar e) key)) (blog-recent-entries))
    (wiliki:db-raw-put! *recent-entries*
                        (write-to-string
                         (take* (acons (cons key title) (sys-time) es) 50)))))

(define (blog-recent-entries-update!)
  (let1 es
      (wiliki:db-fold (lambda (k v es)
                        (rxmatch-case k
                         [#/^\d{8}/ ()
                          (let1 p (wiliki:db-record->page (wiliki) k v)
                            (cons (acons k (entry-title k (~ p'content))
                                         (~ p'mtime))
                                  es))]
                         [else es]))
                      '())
    (wiliki:db-raw-put! *recent-entries*
                        (write-to-string (take* (sort-by es caar string>?) 50)))))

(define-virtual-page (#/^RecentEntries$/ (_)) ;for check
  `((h2 "Recent Entries")
    (ul
     ,@(map (lambda (e) `(li ,(write-to-string e))) (blog-recent-entries)))))

(define (blog-recent-comments)
  (read-from-string (wiliki:db-raw-get *recent-comments* "()")))

(define (blog-recent-comment-add! key page)
  (if-let1 info (extract-comment-info key page)
    (wiliki:db-raw-put! *recent-comments*
                        (write-to-string (cons info (blog-recent-comments))))))

(define (blog-recent-comments-update!)
  (let1 es (wiliki:db-fold
            (lambda (k v es)
              (if-let1 info
                  (extract-comment-info k (wiliki:db-record->page (wiliki) k v))
                (cons info es)
                es))
            '())
    (wiliki:db-raw-put! *recent-comments*
                        (write-to-string (take* (sort-by es cadddr >) 50)))))

(define (extract-comment-info key page)
  (rxmatch-if (#/^\|comments:(.*)::\d{3,}$/ key) (_ main-key)
    (and-let* ([main  (wiliki:db-get main-key)]
               [title (entry-title main-key (~ main'content))]
               [poster (extract-comment-poster page)])
      (list main-key title poster (~ page'ctime)))
    #f))

(define (extract-comment-poster page)
  (rxmatch-case (with-input-from-string (~ page'content) read-line)
    [#/^\* (.*) \(\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2}\):/ (_ who) who]
    [else #f]))

(define-reader-macro (recent-comments)
  (define n 7)
  `((ul (@ (class recent-comments))
        ,@(map (^e (match-let1 (key title poster time) e
                     `(li ,poster " on "
                          (a (@ (href ,#"~(wiliki:url)/~key")) ,title)
                          " (" ,(sys-strftime "%Y/%m/%d" (sys-localtime time))
                          ")")))
               (take* (blog-recent-comments) n)))))

;;;
;;; Hooking DB
;;;

(define-class <blog> (<wiliki>)
  ((author               :init-keyword :author
                         :init-value "Anonymous")
   ;; Set a command name to show log in page (avoid conflicting existing
   ;; wiliki commands)
   (login-command        :init-keyword :login-command
                         :init-value #f)
   ;; Set path for wiliki password db
   (auth-db-path         :init-keyword :auth-db-path
                         :init-value #f)
   ;; Miscellaneous customization parameters (alist)
   ;; Would be used by macros, e.g. amazon-affiliate-id
   (blog-options         :init-keyword :options
                         :init-value '())
   ))
(define-class <blog-page> (<wiliki-page>) ())

(define-method initialize ((self <blog>) initargs)
  (next-method)
  ;; Authentication setup
  ;; NB: Currently we assume <blog> instance is created every time
  ;; script is called (like in cgi script), so that we can check session
  ;; cookie here to set the editable? slot.  This assumption breaks if
  ;; blog script is loaded once and serves multiple requests.  It should be
  ;; fixed in WiLiKi side to allow editable? slot value is recalculated
  ;; for each request.
  (auth:auth-db-path (~ self'auth-db-path))
  (and-let1 login-command (~ self'login-command)
    (register-login-action login-command))
  (set! (~ self'editable?) (if (authenticated?) #t 'limited))
  ;; Some other setup
  (rss-item-description 'html)
  (rss-url-format 'path)
  (rss-source (cut take* (blog-recent-entries) <>))
  (wiliki:formatter (make <blog-formatter>)))

(define-method wiliki:page-class ((self <blog>)) <blog-page>)

(define-method wiliki:db-put! (key (page <blog-page>) . opts)
  (begin0 (next-method)
    (match (decompose-key key)
      [(y m d) (let* ([e (get-index-entry key #t)]
                      [title (entry-title key (~ page'content))])
                 (set! (index-entry-title e) title)
                 (save-index)
                 (blog-recent-entry-add! key title))]
      [_ (blog-recent-comment-add! key page)])))

(define-method blog-option-ref ((self <blog>) key :optional (fallback #f))
  (assq-ref (~ self'blog-options) key fallback))

;;;
;;; Parts
;;;

;; Maybe Entry, Int, (Entry -> Entry) -> Maybe Entry
(define (drop-entries entry count advance)
  (cond [(not entry) #f]
        [(zero? count) entry]
        [else (drop-entries (advance entry) (- count 1) advance)]))

;; Maybe Entry, Int, (Entry -> Entry) -> [Entry]
(define (take-entries entry count advance)
  (let loop ([entry entry] [count count] [r '()])
    (if (or (not entry) (zero? count))
      (reverse r)
      (loop (advance entry) (- count 1) (cons entry r)))))

;; Maybe Entry, (Entry -> Bool), (Entry -> Entry) -> [Entry]
(define (span-entries entry pred advance)
  (define (scan-out entry)
    (cond [(not entry) '()]
          [(pred entry) (scan-in (advance entry) (list entry))]
          [else (scan-out (advance entry))]))
  (define (scan-in entry r)
    (cond [(not entry) (reverse r)]
          [(pred entry) (scan-in (advance entry) (cons entry r))]
          [else (reverse r)]))
  (scan-out entry))

(define (entry-header entry :key (show-navi #t))
  (define (entry-anchor e)
    `(a (@ (href ,#"~(wiliki:url)/~(index-entry-key e)"))
        ,(if (string-null? (index-entry-title e))
           (index-entry-key e)
           (index-entry-title e))))
  (define (entry-navi)
    (if show-navi
      `((div (@ (class entry-navi))
            ,@(cond-list
               [(index-entry-prev entry)=> @(^p `("< " ,(entry-anchor p)))]
               [#t " | "]
               [(index-entry-next entry)=> @(^n `(,(entry-anchor n) " >"))])))
      `((div (@ (class entry-navi)) "> " (a (@ (href ,#"~(wiliki:url)/Archive/l0")) "エントリ一覧") " | "))))
  (let1 key (index-entry-key entry)
    (let1 cl (number->string (exact (* (floor (/ (string-length (regexp-replace-all #/\[.+?\]|\|\||\s/ (~ (wiliki:db-get key #f)'content) "")) 100.0)) 100)))
      (rxmatch-let (#/^(\d\d\d\d)(\d\d)(\d\d)/ key)
          (_ y m d)
        `(,@(entry-navi)
          (header (@ (class "entry-header"))
            (div (@ (class entry-date)) ,#"~|y|/~|m|/~|d|")
            ,(if show-navi
              ;;`(div (@ (class tweetbutton))
              ;;     (a (@ (href "https://twitter.com/share?ref_src=twsrc%5Etfw") (class "twitter-share-button") (data-show-count "false")) "Tweet"))
              `(div (@ (class content-length)) ,#"文章量：約~|cl|字")
              `(div (@ (class permalink))
                   (a (@(href ,#"~(wiliki:url)/~key")) "#permalink")))))))))

(define (show-n-entries entries)
  (define dummy-page (make <blog-page>)) ;to make comment macro use compact form
  (map (^e `(article (@ (class n-entry-show))
                 ,@(entry-header e :show-navi #f)
                 ,@(let1 p (wiliki:db-get (index-entry-key e) #f)
                     (parameterize ([wiliki:page-stack
                                     (cons dummy-page
                                           (wiliki:page-stack))])
                       (wiliki:format-content p)))))
       entries))

(define (show-more-entry-link start)
  `(p (a (@ (href ,#"~(wiliki:url)/Browse/~start"))
         "More entries ...")))

(define (show-archive-links entries)
  `((ul ,@(map (^e `(li (a (@ (href ,#"~(wiliki:url)/~(index-entry-key e)"))
                           ,(let1 ymd (decompose-key (index-entry-key e))
                              (apply format "~4d/~2,'0d/~2,'0d" ymd))
                           " : " ,(index-entry-title e))))
               entries))))

(define (show-yearly-links hilite-year)
  `((p (@ (class "archive-year-links"))
       ,@(map (^y `(span (@ (class ,(if (eqv? y hilite-year)
                                      "archive-year-link-hi"
                                      "archive-year-link-lo")))
                         (a (@ (href ,#"~(wiliki:url)/Archive/~y"))
                            ,(x->string y))))
              (sort (map car (blog-index-root)) >)))))

(define-reader-macro (recent-entries)
  (define n 6)
  `((ul (@ (class recent-entries))
        ,@(map (^e `(li (a (@ (href ,#"~(wiliki:url)/~(index-entry-key e)"))
                           ,(index-entry-title e))))
               (take-entries (blog-index-last) n index-entry-prev)))
    (p (a (@ (href ,#"~(wiliki:url)/Archive/l0")) "More..."))))

(define *entries-in-page* 3)
(define *archive-links-in-page* 50)

(define-reader-macro (top-page-contents)
  (let1 es (take-entries (blog-index-last) *entries-in-page* index-entry-prev)
    `(,@(show-n-entries es)
      ,@(cond-list
         [(= (length es) *entries-in-page*)
          (show-more-entry-link *entries-in-page*)]))))

(define-virtual-page (#/^Browse\/(\d+)/ (_ start))
  (define s (x->integer start))
  (define n *entries-in-page*)
  (let1 es (take-entries (drop-entries (blog-index-last) s index-entry-prev)
                         n index-entry-prev)
    `(,@(show-n-entries es)
     ,@(cond-list [(= (length es) n) (show-more-entry-link (+ s n))]))))

(define-virtual-page (#/^Archive\/l(\d+)/ (_ start))
  (define s (x->integer start))
  (define n *archive-links-in-page*)
  (let1 es (take-entries (drop-entries (blog-index-last) s index-entry-prev)
                         n index-entry-prev)
    `(,@(show-yearly-links #f)
      ,@(show-archive-links es)
      ,@(cond-list
	 [(= (length es) n)
          `(a (@ (href ,#"~(wiliki:url)/Archive/l~(+ s n)")) "More...")]))))

(define-virtual-page (#/^Archive\/(\d{4})/ (_ year))
  (define rx (string->regexp #"^~year"))
  (let1 es (span-entries (blog-index-last)
                         (^e (rx (index-entry-key e)))
                         index-entry-prev)
    `(,@(show-yearly-links (x->integer year))
      ,@(show-archive-links es))))

;;;
;;; Miscellaneous (not specific for blog features)
;;;

(define-reader-macro (chaton-badge room-uri)
  `((script (@ (type "text/javascript") (src ,#"~|room-uri|/b")
               (charset "utf-8")))))

(define-reader-macro (amazon-affiliate asin . opts)
  (define aid (blog-option-ref (wiliki) 'amazon-affiliate-id))
  (define ikey (blog-option-ref (wiliki) 'iframely-key-hash))
    #"<div class=\"iframely-embed\"><div class=\"iframely-responsive\" style=\"height: 140px; width: 90%; margin: 0 auto; padding-bottom: 0;\">\
        <a href=\"https://www.amazon.co.jp/dp/~|asin|&linkCode=ll1&tag=~|aid|&language=ja_JP&ref_=as_li_ss_tl\" \
        data-iframely-url=\"//cdn.iframe.ly/api/iframe?url=https%3A%2F%2Fwww.amazon.co.jp%2Fdp%2F~|asin|%3F%26linkCode%3Dll1%26tag%3D~|aid|%26language%3Dja_JP%26ref_%3Das_li_ss_tl&key=~|ikey|\">\
        </a></div></div>")

(define-reader-macro (gist id)
  (if (#/^[\da-fA-F]+$/ id)
    #"<div style='font-size:75%'><a style=\"background-color: #ececec;\" href=\"https://gist.github.com/~|id|\">https://gist.github.com/~|id|</a><script src=\"https://gist.github.com/~|id|.js\"> </script><noscript><a href=\"https://gist.github.com/~|id|\">https://gist.github.com/~|id|</a></noscript></div>"
    ""))

(define-reader-macro (embedly url)
  #"<a class=\"embedly-card\" href=\"~|url|\"></a><script async src=\"//cdn.embedly.com/widgets/platform.js\" charset=\"UTF-8\"></script>")

(define-reader-macro (hatena url)
  #"<iframe class=\"hatenablogcard\" style=\"width:100%;height:155px;\" src=\"https://hatenablog-parts.com/embed?url=~|url|\" width=\"100%\" frameborder=\"0\" scrolling=\"no\"></iframe>")

(define-reader-macro (tweet url)
  #"<div class=\"twitter\"><blockquote class=\"twitter-tweet\"><a href=\"~|url|\"></a></blockquote></div>")

(define-reader-macro (youtube id)
  #"<div class=\"youtube\"><iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/~|id|\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe></div>")

(define-reader-macro (misskey-note host id)
  #"<iframe src=\"https://~|host|/embed/notes/~|id|\" data-misskey-embed-id=\"v1_~(number->string (random-real))\" loading=\"lazy\" referrerpolicy=\"strict-origin-when-cross-origin\" style=\"border: none; width: 100%; max-width: 500px; height: 300px; color-scheme: light dark;\"></iframe><script defer src=\"https://~|host|/embed.js\"></script>")

(define-reader-macro (misskey-clip host id)
  #"<iframe src=\"https://~|host|/embed/clips/~|id|\" data-misskey-embed-id=\"v1_~(number->string (random-real))\" loading=\"lazy\" referrerpolicy=\"strict-origin-when-cross-origin\" style=\"border: none; width: 100%; max-width: 500px; height: 300px; color-scheme: light dark;\"></iframe><script defer src=\"https://~|host|/embed.js\"></script>")

(define-reader-macro (google-map title . params)
  (let-macro-keywords* params ([width 640] [height 480]
                               [frameborder 0] [scrolling "no"]
                               [marginheight 0] [marginwidth 0]
                               [msa 0] [msid #f] [hl "en"] [ie "UFT-8"] [t "h"]
                               [ll #f] [spn #f] [z #f])
    (unless (and ll spn z)
      (error "You need ll, spn and z parameters"))
    (let* ([q `((msa ,msa) ,@(if msid `((msid ,msid)) '())
                (hl ,hl) (ie ,ie) (t ,t)
                (ll ,ll) (spn ,spn) (z ,z))]
           [prefix "https://maps.google.com/maps/ms"]
           [path1 (http-compose-query prefix `(,@q (output "embed")))]
           [path2 (http-compose-query prefix `(,@q (source "embed")))])
      `((iframe (@ (width ,width) (height ,height)
                   (frameborder ,frameborder) (scrolling ,scrolling)
                   (marginheight ,marginheight)
                   (marginwidth ,marginwidth) (src ,path1)))
        (br)
        (small "View "
               (a (@ (href ,path2) (style "color:#0000FF;text-align:left"))
                  ,title)
               " in a larger map")))))

;;;
;;; Page layout
;;;

(define-class <blog-formatter> (<wiliki-formatter>) ())

(define-method wiliki:format-head-title ((fmt <blog-formatter>) page . opts)
  (cond [(get-index-entry (~ page'key))
         => (^e #"~(~ (wiliki)'title) - ~(index-entry-title e)")]
        [else (~ page'title)]))

(define-method wiliki:format-page-header ((fmt <blog-formatter>) page . opts)
  (define (td x) (list 'td x))
  `((header (@ (class "site-header") (role "banner"))
      ,@(cond-list
         [(authenticated?)
          `(div (@ (class "admin-menu"))
                (table (tr (td ,@(new-entry-button))
                           ,@(cond-list
                              [(wiliki:edit-link page) => td]
                              [(wiliki:history-link page) => td]
                              [(wiliki:all-link page) => td])
                           (td ,@(rebuild-index-button))
                           (td ,@(show-index-button))
                           (td ,@(manage-comments-button)))))])
      (input (@ (type "checkbox") (id "menu-toggle") (class "menu-toggle") (style "display: none;")))
      (label (@ (for "menu-toggle") (class "drawer-overlay")))
      (label (@ (for "menu-toggle") (class "drawer-toggle drawer-hamburger"))
             (span (@ (class "drawer-hamburger-icon"))))
      (nav (@ (role "navigation") (class "drawer-nav"))
           (div ,@(wiliki:search-box))
           ,@(let loop ([in (wiliki:get-formatted-page-content "_SidePane")])
               (cond [(null? in) '()]
                     [(and (pair? (car in)) (eq? (caar in) 'h2))
                      (let-values ([(content rest)
                                    (span (lambda (x) (not (and (pair? x) (eq? (car x) 'h2)))) (cdr in))])
                        (cons `(details (@ (class "menu-accordion"))
                                        (summary (h2 ,@(let1 h2 (car in)
                                                         (if (and (pair? (cdr h2)) (pair? (cadr h2)) (eq? (caadr h2) '@))
                                                           (cddr h2)
                                                           (cdr h2)))))
                                        ,@content)
                              (loop rest)))]
                     [else (cons (car in) (loop (cdr in)))])))
      (div (@ (class "blog-title"))
           (a (@ (href ,(wiliki:url :top))) ,(~ (wiliki)'title))))))

(define-method wiliki:format-page-footer ((fmt <blog-formatter>) page . opts)
  `((footer (@ (class "site-footer"))
         (div (@ (class "footer-content"))
           (div (@ (class "license-info"))
             (a (@ (rel "license")
                   (href "https://creativecommons.org/licenses/by-nc/4.0/"))
                (img (@ (alt "Creative Commons License")
                        (style "width:initial;")
                        (src "https://licensebuttons.net/l/by-nc/4.0/88x31.png"))))
             (span "This work by "
                   (a (@ (href "https://misskey.osyakasyama.me/@pleasure666")
                         (rel "me"))
                      ,(~ (wiliki)'author))
                   " is licensed under a "
                   (a (@ (rel "license")
                         (href "https://creativecommons.org/licenses/by-nc/4.0/"))
                      "Creative Commons Attribution 4.0 Unported License")))
           (div (@ (class "meta-info"))
             (span "Last modified : " ,(wiliki:format-time (ref page 'mtime)))
             (span (a (@ (href "https://practical-scheme.net/wiliki/wiliki.cgi"))
                      "WiLiKi " ,(wiliki:version))
                   " running on "
                   (a (@ (href "https://practical-scheme.net/gauche/"))
                      "Gauche ",(gauche-version))))
           (div (@ (id "page_top")) (a (@ (href "#"))))))))

(define-method wiliki:format-page-content ((fmt <blog-formatter>) page . opts)
  (let1 index-entry (get-index-entry (~ page'key))
    `((div (@ (class "site-content"))
       (aside (@ (class "menu-strip") (role "complementary"))
         (div ,@(wiliki:search-box))
         ,@(wiliki:get-formatted-page-content "_SidePane"))
       (main (@ (class "main-pane") (role "main"))
         (article (@ (class "content"))
           ,@(cond-list
              [index-entry @ (entry-header index-entry)])
           ,@(wiliki:format-content page)))))))

;;;
;;; Comment management
;;;

(define-virtual-page (#/^ManageComments:(\d+)$/ (_ s))
  ;; Search log files and pick a new comment entries.
  ;; Returns  ((page . ip) ...).  The second value is a flag to indicate
  ;; there's more entries.
  (define (pick-comments start count)
    (let loop ([entries ($ filter-map
                           (^[ls] (and (string-prefix? "A" (cadr ls))
                                       (read-from-string #"(~(car ls))")))
                           $ wiliki-log-pick-from-file #/^\|comments:/
                           $ wiliki:log-file-path $ wiliki)]
               [r '()] [s 0] [c 0])
      (if (> c count)
        (values (reverse r) #t)
        (match entries
          [() (values (reverse r) #f)]
          [((_ pagename _ ip . _) . rest)
           (if-let1 page (wiliki:db-get pagename)
             (if (>= s start)
               (loop rest (acons page ip r) (+ s 1) (+ c 1))
               (loop rest r (+ s 1) c))
             (loop rest r s c))]))))
  ;; Take (page . ip) and renders one entry of the comment.
  (define (comment-entry page&ip)
    (rxmatch-let (#/^\|comments:(.*)::\d+$/ (~(car page&ip)'key)) (_ parent-key)
      `(div
        (@ (class "manage-comment-entry"))
        (div (@ (class "comment-past"))
             (p (@ (class "comment-entry-title"))
                (input (@ (type checkbox) (name ,(~(car page&ip)'key))))
                "Comment for " ,(parent-page-link parent-key)
                " from " ,(cdr page&ip))
             ,@(parameterize ([wiliki:reader-macros '()])
                 (wiliki:format-content (car page&ip)))))))
  (define (parent-page-link parent-key)
    (let1 parent-page (wiliki:db-get parent-key)
      (if parent-page
        `(a (@ (href ,(wiliki:url "~a" (~ parent-page'key))))
            ,(entry-title (~ parent-page'key) (~ parent-page'content)))
        (format "~s (deleted)" parent-key))))

  ;; body
  (let1 start (x->integer s)
    (if (authenticated?)
      (receive (page&ips more?) (pick-comments start 30)
        `((h2 "Manage comments")
          (form
           (@ (method "POST"))
           (input (@ (type hidden) (name c) (value "manage-comment-del")))
           (input (@ (type submit) (name s) (value "Delete marked comments")))
           ,@(map comment-entry page&ips)
           ,@(if more?
               `((p (a (@ (href ,(wiliki:url "ManageComments:~d" (+ start 30))))
                       "More...")))
               '()))))
      '())))

(define-wiliki-action manage-comment-del :write (pagename params
                                                          (confirm :default #f))
  (define (comment-entry key)
    `(div (@ (class manage-comment-entry))
          (div (@ (class comment-past))
               ,@(wiliki:get-formatted-page-content key))))
  (let1 page-keys (filter #/^\|comments:/ (map car params))
    (if confirm
      (begin
        (dolist [page page-keys]
          (cmd-commit-edit page "" #f "Batch deletion by the Comment Manager"
                           #t #t))
        (wiliki:redirect-page "ManageComments:0"))
      (wiliki:std-page
       (make <blog-page>
         :title "Manage Comments - Confirm"
         :content
         `((h2 "Delete these comments?")
           ,@(parameterize ([wiliki:reader-macros '()])
               (map comment-entry page-keys))
           (form
            (@ (method POST))
            (input (@ (type hidden) (name c) (value "manage-comment-del")))
            (input (@ (type hidden) (name confirm) (value "ok")))
            (input (@ (type submit) (name s) (value "Yes, delete them.")))
            ,@(map (^k `(input (@ (type hidden) (name ,k) (value "yes"))))
                   page-keys))
           ))))))

;; Local variables:
;; mode: scheme
;; end:
