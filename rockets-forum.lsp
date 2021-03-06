#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets-forum.lsp - Rockets forum page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - Forum"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-forum")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(start-div "hero-unit")
	(if RocketsConfig:DiscussionImage 
		(display-image RocketsConfig:DiscussionImage)
		(display-image "newlisp-rockets-picture-small" 480 270)
	)
	(displayln "<h2>The " RocketsConfig:ShortName " Discussion Forum</h2>")
	(if RocketsConfig:ForumSubtitle (displayln "<P>" RocketsConfig:ForumSubtitle "</p>"))
(end-div)

; If user has selected "Mark All As Read" then, well, mark all as read!
(set 'mark-all ($GET "markall"))
(if (and mark-all Rockets:UserId) (begin ; but you have to be logged in of course
	(set 'all-posts (query "SELECT Id from Posts")) ; put all posts ids into a list
	(set 'read-posts-line "")
	(dolist (r all-posts) (extend read-posts-line (string (first r) "-")))
	(set 'UserId Rockets:UserId)
	(set 'UserReadPosts read-posts-line)
	(update-record "Users" UserId UserReadPosts)
	(page-redirect "rockets-forum") ; we have to do a redirect to refresh the page
))

; get current page from URL (if there is one)
(set 'current-page (force-parameters 1 ($GET "p"))) ; we only need the first part, ignore anything else
(if current-page (set 'current-page (int current-page)) (set 'current-page 1))

; get all existing posts
(set 'total-posts (int (first (first (query (string "SELECT Count(*) FROM Posts"))))))

(set 'total-pages (/ total-posts Blog:forum-posts-per-page))
(if (>= (mod (float total-posts) (float Blog:forum-posts-per-page)) 1) (inc total-pages)) ; fix number of pages if not evenly divisible

(display-paging-links 1 total-pages current-page active-page)

(set 'start-post-num (- (* current-page Blog:forum-posts-per-page) Blog:forum-posts-per-page))
; get all Forum Notices
(set 'posts-query-notices-sql (string "SELECT * from Posts WHERE PostType='Forum notice' ORDER BY PostLastDate DESC;"))
; get all posts of all types EXCEPT Forum Notices
(set 'posts-query-sql (string "SELECT * from Posts WHERE PostType!='Forum notice' ORDER BY PostLastDate DESC LIMIT " start-post-num "," Blog:forum-posts-per-page ";"))

(set 'posts-result-notices (query posts-query-notices-sql))
(set 'posts-result (query posts-query-sql))
(if posts-result-notices (set 'posts-result (append posts-result-notices posts-result)))

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
	(set 'post-lastauthor (x 10))
	(set 'post-lastdate (x 11))
	(if (nil? post-views) (set 'post-views 0)) ; needed because views was a late addition
    ; check to see if the user has read this post or not
    (if Rockets:UserReadPosts (begin
	  (if (or (find (string post-num "-") Rockets:UserReadPosts) (nil? Rockets:UserId)) ; if you're not logged in OR if you are, and you've read the post
		(set 'post-read (string " <img src=images/read-msg.png> "))
		(set 'post-read (string " <img src=images/unread-msg.png> "))
      ))
        (set 'post-read (string " <img src=images/read-msg.png> "))
    )
    ; if the post is a forum notice, then make sure the icon is always an exclamation point
    (if (= post-type "Forum notice") (set 'post-read (string " <img src=images/forum-notice.png> ")))
	(push (list (string "<h4>" post-read post-subject "</h4>") 
		        (string "<h5>" post-type "</h5>")
		        (string "<h5>" post-author "</h5>")
		        (string "<h5>" post-views " views / " post-replies " replies</h5>")
		        (string "<h5>" post-lastdate " by " post-lastauthor "</h5>")) forum-post-table -1)
	(push (list (string "rockets-item.lsp?p=" post-num "&f=true") nil nil nil (string "rockets-item.lsp?p=" post-num "&f=true#reply")) forum-links-table -1)
)

(display-responsive '("Topic Subject" "Post Type" "Post Author" "Views / Replies" "Last Post") forum-post-table "striped" forum-links-table '(5 1 2 2 2))

(display-paging-links 1 total-pages current-page active-page) ; display them again

; add "Mark all posts as read" button
(if Rockets:UserId
	(displayln "<a class='btn btn-primary' href='rockets-forum.lsp?markall=1'>Mark all posts as read</a>"))

; print post entry box
(if Rockets:UserId (begin
	(display-post-box "Post something..." "postsomething" "rockets-post.lsp" "subjectline" "replybox" "Post Message" nil nil nil "Forum" true)
)) ; any registered user may make a forum post

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!

