#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Posting Page rockets-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in

(if Rockets:UserId (begin ; must be a registered user to post anything

	(set 'max-posts (first (first (query "SELECT max(Id) from Posts"))))
	(displayln "<P>Max post id: " max-posts)
	
	(displayln "<P>$POST data: " ($POST))
	
	; if we have a post in $POST, post it to the Posts table.  Tee hee!
	(set 'continue true) ; debugging
	(if continue (begin
	(if (and ($POST "post") ($POST "subjectline"))
		(begin
			(set 'post-type-trigger ($POST "optionalhidden")) ; this is a hidden value to make forum posts, not blog posts
			(set 'Id (+ max-posts 1))
			(set 'PosterId Rockets:UserId) ; Any registered user may post, but only Admin may post blog posts
			(set 'PostSubject ($POST "subjectline"))
			(set 'PostContent ($POST "post"))
			(set 'PostDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
			(if (= post-type-trigger "Forum")
				(set 'PostType "Forum post")
				(set 'PostType "Blog post"))
			(create-record "Posts" Id PosterId PostDate PostSubject PostContent PostType) ; It's the C in CRUD!
			;(displayln "<B>Posting disabled for the moment...</b>")
		)
	)
	))

)) ; end check to see if user is signed in
;(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>")
(if (= PostType "Blog post")
	(page-redirect "rockets-main.lsp")
	(page-redirect "rockets-forum.lsp"))
(display-page)