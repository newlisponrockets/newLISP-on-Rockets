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
(set 'active-page "rockets-forum")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(start-div "hero-unit")
	(display-image "rockets.png" 317 180)
	(displayln "<h2>The newLISP on Rockets Discussion Forum</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

; get current page from URL (if there is one)
(set 'current-page (force-parameters 1 ($GET "p"))) ; we only need the first part, ignore anything else
(if current-page (set 'current-page (int current-page)) (set 'current-page 1))

; get all existing posts
(set 'total-posts (int (first (first (query (string "SELECT Count(*) FROM Posts"))))))

(set 'total-pages (/ total-posts Blog:forum-posts-per-page))
(if (>= (mod (float total-posts) (float Blog:forum-posts-per-page)) 1) (inc total-pages)) ; fix number of pages if not evenly divisible

(display-paging-links 1 total-pages current-page active-page)

(set 'start-post-num (- (* current-page Blog:forum-posts-per-page) Blog:forum-posts-per-page))
(set 'posts-query-sql (string "SELECT * from Posts ORDER BY Id DESC LIMIT " start-post-num "," Blog:forum-posts-per-page ";"))

(set 'posts-result (query posts-query-sql))

; print out all posts
(dolist (x posts-result)
	(set 'post-num (x 0))
	(set 'post-author (author-name (x 1)))
	(set 'post-avatar (author-avatar (x 1)))
	(set 'post-date (x 2))
	(set 'post-subject (x 3))
	(set 'post-content (x 4))
	(set 'post-replies (x 5)) (if (nil? post-replies) (set 'post-replies "0"))
	(set 'post-type (x 6))
	(set 'post-views (x 7))
	(if (nil? post-views) (set 'post-views 0)) ; needed because views was a late addition
	(push (list post-subject post-type post-author post-views post-replies) forum-post-table -1)
	(push (list (string "rockets-item.lsp?p=" post-num "&f=true") nil nil nil nil) forum-links-table -1)
)

(display-table '("Topic Subject" "Post Type" "Post Author" "Views" "Replies") forum-post-table "striped" forum-links-table)

(display-paging-links 1 total-pages current-page active-page) ; display them again

; print post entry box
(if Rockets:UserId (begin
	(display-post-box "Post something..." "postsomething" "rockets-post.lsp" "subjectline" "replybox" "Post Message" nil nil nil "Forum")
)) ; any registered user may make a forum post

(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!

