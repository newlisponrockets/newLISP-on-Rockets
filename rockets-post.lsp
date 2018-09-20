#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

(load "Rockets-config.lisp") ; load configuration information

; Rockets - Posting Page rockets-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(open-database RocketsConfig:Database)
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
			(set 'PostPoll ($POST "polltopic")) ; for polls, adds text to post and info to database
			(set 'PostPollValues ($POST "pollvalues"))
			(displayln "Post Poll Values: " PostPoll ">>>")
			(if (not (= PostPoll "")) (begin
				(set 'poll-prepend-text (string "[h4]" PostPoll "[/h4]\n[poll]"))
				(set 'PostPollSubject PostSubject) ; we need a copy of this variable  
				(replace " " PostPollSubject "_") ; this is to generate unique form names for each poll
				(set 'PostPollValues (parse PostPollValues "\n"))
				(dolist (p PostPollValues)
					(extend poll-prepend-text (string "\n" "[radio]" PostPollSubject " value=" $idx))
					(if (= $idx 0) (extend poll-prepend-text " checked=yes"))	
					(extend poll-prepend-text (string "[/radio] " p ))
				)
				(set 'PostContent (string poll-prepend-text " [/poll]\n\n\n " PostContent))
			))
			(if (= post-type-trigger "Forum")
				(set 'PostType "Forum post")
				(set 'PostType "Blog post"))
			(create-record "Posts" Id PosterId PostDate PostSubject PostContent PostType) ; It's the C in CRUD!
			; now update the user's postcount! postcount++!!
			(set 'UserId Rockets:UserId)
			(set 'UserPosts (++ Rockets:UserPosts))
			(update-record "Users" UserId UserPosts)
		)
	)
	))

)) ; end check to see if user is signed in

;(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>")
(if (= PostType "Blog post")
	(page-redirect "rockets-main.lsp")
	(page-redirect "rockets-forum.lsp"))
(display-page)