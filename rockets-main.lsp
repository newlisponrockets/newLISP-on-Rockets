#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Main Page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header RocketsConfig:Name)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-main")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(start-div "hero-unit")
	(display-image "rockets.png" 317 180)
	(displayln "<h2>" RocketsConfig:Name "</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

; get current page from URL (if there is one)
(set 'current-page (force-parameters 1 ($GET "p"))) ; we only need the first part, ignore anything else
(if current-page (set 'current-page (int current-page)) (set 'current-page 1))

; get all existing posts
(set 'total-posts (int (first (first (query (string "SELECT Count(*) FROM Posts WHERE PostType='Blog post'"))))))

(set 'total-pages (/ total-posts Blog:posts-per-page))
(if (>= (mod (float total-posts) (float Blog:posts-per-page)) 1) (inc total-pages)) ; fix number of pages if not evenly divisible

(display-paging-links 1 total-pages current-page active-page)

(set 'start-post-num (- (* current-page Blog:posts-per-page) Blog:posts-per-page))
(set 'posts-query-sql (string "SELECT * from Posts WHERE PostType='Blog post' ORDER BY Id DESC LIMIT " start-post-num "," Blog:posts-per-page ";"))

(set 'posts-result (query posts-query-sql))
; print out all posts
(dolist (x posts-result)
	(display-individual-post x)
)

(display-paging-links 1 total-pages current-page active-page) ; display them again

; print post entry box
(if (= Rockets:UserId 0) (begin
	(display-post-box "Post something..." "postsomething" "rockets-post.lsp" "subjectline" "replybox" "Post Message")
)) ; only the site administrator may make new blog posts at the moment

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!

