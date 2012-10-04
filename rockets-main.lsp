#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Main Page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in

(display-navbar "newLISP on Rockets" '(("Home" "rockets-main" "active") ("About" "rockets-about") ("Contact" "rockets-contact") ("Register" "rockets-register")) "rockets-verify")
(start-div "hero-unit")
	(display-image "rockets.png")
	(displayln "<h2>The newLISP on Rockets Blog</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

; THIS IS TEMPORARY TAKE OUT AS SOON AS WE HAVE VALIDATION
(define (author-name str-author-id)
	(case str-author-id
		("0" "Rocket Man")))

;(set 'test-var "testy!")
;(display-partial "test-partial")

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

; print post entry box
(if (= Rockets:UserId 0) (begin
	(display-post-box "Post something..." "postsomething" "rockets-post.lsp" "subjectline" "replybox" "Post Message")
)) ; only the site administrator may make new blog posts at the moment

; table structure:  ((0 "Id" "INTEGER" 0 nil 1) (1 "PosterId" "TEXT" 0 nil 0) (2 "PostDate" "DATE" 0 nil 0) (3 "PostSubject" "TEXT" 0 nil 0) (4 "PostContent" "TEXT" 0 nil 0))

(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!
(exit)
