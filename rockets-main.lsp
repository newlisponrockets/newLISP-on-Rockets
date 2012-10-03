#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Main Page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; Version 0.01 (Rockets version shown on page)

(display-header)
(display-navbar "newLISP on Rockets" '(("Home" "rockets-main" "active") ("About" "rockets-about") ("Contact" "rockets-contact") ("Register" "rockets-register")))
(start-div "hero-unit")
	(display-image "rockets.png")
	(displayln "<h2>The newLISP on Rockets Blog</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

; THIS IS TEMPORARY TAKE OUT AS SOON AS WE HAVE VALIDATION
(define (author-name str-author-id)
	(case str-author-id
		("0" "Rocket Man")))+

(open-database "ROCKETS-BLOG")

; set Rockets cookie name (will be from a file later)
(set 'rocket-cookie-name "rockets-4dckq3-e4jcx-2wgxc")

(displayln "Checking for user cookie:" ($COOKIES rocket-cookie-name)) ; this is the Rockets signin cookie.  Will be in external file or db at some point.

; get all existing posts
(set 'posts-query-sql (string "SELECT * from Posts;"))
(set 'posts-result (reverse (query posts-query-sql))) ; reverse it so newest posts first
; print out all posts
(dolist (x posts-result)
	(displayln "<br><a href='rockets-delete.lsp?post=" (x 0) "'>Delete post</a>")
	(displayln "<h4>" (x 3) "</h4>")
	(displayln "<br><b>Post #:</b> " (x 0) )
	(displayln "<BR><B>Date:</b> " (x 2) "")
	(displayln "<br><B>Author:</b> " (author-name (x 1)) "")
	(displayln "<br><br><p>" (format-for-web (x 4)) "</p>")
	(displayln "<hr>")
)

; Twitter stuff
;(set 'twitter-term "salesforce")
;(displayln "<h3>Twitter searches containing: " twitter-term "</h3>")
;(twitter-search twitter-term 8)

; set a cookie
(set-cookie rocket-cookie-name "user=1" (date-value 2013 2 28))

; print post entry box
(display-post-box "Post something..." "postsomething" "rockets-post.lsp" "subjectline" "replybox" "Post Message")

; table structure:  ((0 "Id" "INTEGER" 0 nil 1) (1 "PosterId" "TEXT" 0 nil 0) (2 "PostDate" "DATE" 0 nil 0) (3 "PostSubject" "TEXT" 0 nil 0) (4 "PostContent" "TEXT" 0 nil 0))

(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!
(exit)
