#!/usr/bin/gosh

;; wiliki2 - sample of customizing page format

(use wiliki)
(use wiliki.format)
(use wiliki.db)

(define (my-page-header page opts)
  `((div (@ (style "font-size:80%;text-align:right"))
         ,@(wiliki:menu-links page))))

(define (my-page-footer page opts)
  `((hr)
    (div (@ (class "footer") (style "text-align:right"))
         "Last modified : " ,(wiliki:format-time (ref page 'mtime))
         (br)
         "Powered by "
         (a (@ (href "http://www.shiro.dreamhost.com/scheme/gauche"))
            "Gauche ")
         ,(gauche-version))))

(define (my-page-content page opts)
  `((table
     (@ (border 0) (cellspacing 8) (width "100%") (class "content-table"))
     (tr (td (@ (class "menu-strip")
                (valign "top") (style "font-size:80%;width:10em"))
             (div (@ (class "menu-title")) "Topics")
             ,@(wiliki:get-formatted-page-content "Topics")
             (div (@ (class "menu-title")) "Recent Updated")
             (ul (@ (class "menu-list"))
                 ,@(map (lambda (p)
                          `(li ,@(wiliki:format-wiki-name (car p))))
                        (wiliki:recent-changes-alist :length 20))))
         (td (@ (valign "top"))
             ,@(wiliki:page-title page)
             ,@(wiliki:format-content page))))))

(let ((formatter (wiliki:formatter)))
  (set! (ref formatter 'header) my-page-header)
  (set! (ref formatter 'footer) my-page-footer)
  (set! (ref formatter 'content) my-page-content))

(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "/home/shiro/data/wikidata.dbm"
     :top-page "WiLiKi"
     :title "MyWiliki2"
     :description "Shiro's Wiliki Site Sample 2"
     :style-sheet "wiliki2.css"
     :language 'jp
     :charsets '((jp . euc-jp) (en . euc-jp))
     :image-urls '((#/^http:\/\/sourceforge.net\/sflogo/ allow))
     :log-file "wikidata.log"
     :debug-level 0
     )))

;; Local variables:
;; mode: scheme
;; end:
