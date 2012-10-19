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

(displayln "<p>It's coming... be patient. :P")

(displayln "<P>$POST data: " ($POST))

(displayln "<br>Posts table: " (query "pragma table_info('Posts');"))
;(displayln "<br>Create table: "(query "CREATE TABLE Comments (Id INTEGER PRIMARY KEY, PostId INTEGER, CommenterId INTEGER, CommentDate DATE, CommentSubject TEXT, CommentContent TEXT);"))
;(displayln "<br> Drop table: " (query "DROP TABLE Comments;"))
(displayln "<br>Comments table: " (query "pragma table_info('Comments');"))

(if (and ($POST "post") ($POST "subjectline") ($POST "linkbackid"))
	(begin
		(if (nil? max-comments) (set 'Id 1) (set 'Id (+ max-comments 1)))
		(set 'CommenterId Rockets:UserId) ; ONLY I MAY POST FOR NOW
		(set 'PostId (int ($POST "linkbackid")))
		(set 'CommentSubject ($POST "subjectline"))
		(set 'CommentContent ($POST "post"))
		(set 'CommentDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
		(displayln "<Br>Post: " CommentId " from: " CommenterId " : " CommentSubject " " CommentContent " " CommentDate)
		(create-record "Comments" Id PostId CommenterId CommentDate CommentSubject CommentContent) ; It's the C in CRUD!
		(displayln "<B>Posting disabled for the moment...</b>")
		; we also have to update the posts table with the new # of comments
		(set 'comment-count (first (first (query (string "SELECT Count(*) FROM Comments WHERE PostId=" PostId ";")))))
		(if comment-count (displayln "a comment") (displayln "no comment"))
		(display "Current comment count: " comment-count)
		(displayln (query (string "UPDATE Posts SET PostComments=" comment-count " WHERE Id=" PostId ";")))
	)
)

;(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>")
(page-redirect "rockets-item.lsp" (string "p=" PostId))
)
	(displayln "<p>Why are you here?")
)

(display-page)