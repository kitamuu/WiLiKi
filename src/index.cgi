#!/usr/local/bin/gosh

(use wiliki)
(use dbm.fsdbm)
(use blog)
(use wiliki.rss)
(rss-item-description 'html)

(define (main args)
  (wiliki-main
   (make <blog>
     :author "kitamuu"
     :login-command "login"
     :auth-db-path "/usr/lib/cgi-bin/data/wiki-pass"
     :db-path "/usr/lib/cgi-bin/data/wiliki"
     :db-type <fsdbm>
     :log-file "/usr/lib/cgi-bin/data/wiliki-blog.log"
     :event-log-file "/usr/lib/cgi-bin/data/blog-events.log"
     :top-page "Something like blog"
     :title "Something like blog"
     :description "Something like blog"
     :style-sheet '("assets/blog.css")
     :language 'jp
     :charsets '((jp . utf-8) (en . utf-8))
     :image-urls '((#/^http:\/\/[-\w.]*amazon.com\// allow) (#/^https:\/\/[-\w.]*storage.googleapis.com\// allow)
                   (#/^\// allow))
     :debug-level 0
     :script-name "/wiliki"
     :options '((amazon-affiliate-id . your-id) (iframely-key-hash . your-key-hash))
     )))
