#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Main Page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(display-header "The newLISP on Rockets Blog")
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-main")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(start-div "hero-unit")
	(display-image "rockets.png" 317 180)
	(displayln "<h2>The newLISP on Rockets Blog</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

; get current page from URL (if there is one)
(set 'current-page ($GET "p"))
(if current-page (set 'current-page (int current-page)) (set 'current-page 1))

; get all existing posts
(set 'total-posts (int (first (first (query (string "SELECT Count(*) FROM Posts"))))))

(set 'total-pages (/ total-posts Blog:posts-per-page))
(if (>= (mod (float total-posts) (float Blog:posts-per-page)) 1) (inc total-pages)) ; fix number of pages if not evenly divisible

(display-paging-links 1 total-pages current-page active-page)

(set 'start-post-num (- (* current-page Blog:posts-per-page) Blog:posts-per-page))
(set 'posts-query-sql (string "SELECT * from Posts ORDER BY Id DESC LIMIT " start-post-num "," Blog:posts-per-page ";"))

(set 'posts-result (query posts-query-sql))
; print out all posts
(dolist (x posts-result)
	(display-individual-post x)
)

(display-paging-links 1 total-pages current-page active-page) ; display them again

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

