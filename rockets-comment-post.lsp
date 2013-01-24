#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Posting Page rockets-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions")
; only registered users should ever be on this page
(if Rockets:UserId (begin

(set 'max-comments (first (first (query "SELECT max(Id) from Comments"))))
(displayln "<P>Max comment id: " max-comments)

(displayln "<P>$POST data: " ($POST))

(if (and ($POST "post") ($POST "subjectline") ($POST "linkbackid"))
	(begin
		(if (nil? max-comments) (set 'Id 1) (set 'Id (+ max-comments 1)))
		(set 'CommenterId Rockets:UserId) ; any registered user may add a comment
		(set 'PostId (int ($POST "linkbackid")))
		(set 'CommentSubject ($POST "subjectline"))
		(set 'CommentContent ($POST "post"))
		(set 'CommentDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
		(set 'forum-view-post ($POST "optionalhidden")) ; this indicates whether we were in forum or blog mode
		(displayln "<Br>Post: " CommentId " from: " CommenterId " : " CommentSubject " " CommentContent " " CommentDate)
		(create-record "Comments" Id PostId CommenterId CommentDate CommentSubject CommentContent) ; It's the C in CRUD!
		(displayln "<B>Posting disabled for the moment...</b>")
		; we also have to update the posts table with the new # of comments
		(set 'comment-count (first (first (query (string "SELECT Count(*) FROM Comments WHERE PostId=" PostId ";")))))
		(if comment-count (displayln "a comment") (displayln "no comment"))
		(display "Current comment count: " comment-count)
		(displayln (query (string "UPDATE Posts SET PostComments=" comment-count " WHERE Id=" PostId ";")))
		; now update the user's postcount! postcount++!!
		(set 'UserId Rockets:UserId)
		(set 'UserPosts (++ Rockets:UserPosts))
		(update-record "Users" UserId UserPosts)
		; now we have to loop through all users read lists to remove the post from that list, since a new comment was added
		(set 'readlist-all (query "SELECT UserID, UserReadPosts FROM Users")) ; everyone's read list!  EVERYONE'S!
		(dolist (q readlist-all)
			(set 'user-tmp (q 0))
			(set 'readlist-tmp (q 1))
			(if (find (string PostId "-") readlist-tmp)
			(and (replace (string PostId "-") readlist-tmp "")
				  (query (append "UPDATE Users SET UserReadPosts='" readlist-tmp "' WHERE UserID=" (string user-tmp) ";"))))
		)
	)
)

;	(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>")
	(if forum-view-post
		(page-redirect "rockets-item.lsp" (string "p=" PostId "&f=true#reply")) ; go back to forum view if came from forum
		(page-redirect "rockets-item.lsp" (string "p=" PostId "#reply")))
)
(displayln "<p>Why are you here?")
)

(display-page)