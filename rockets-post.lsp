#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Posting Page rockets-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(open-database "ROCKETS-BLOG")
(set 'max-posts (first (first (query "SELECT max(Id) from Posts"))))
(displayln "<P>Max post id: " max-posts)

(displayln "<P>$POST data: " ($POST))

; if we have a post in $POST, post it to the Posts table.  Tee hee!
(set 'continue true)
(if continue (begin
(if (and ($POST "post") ($POST "subjectline"))
	(begin
		(set 'Id (+ max-posts 1))
		(set 'PosterId "0") ; ONLY I MAY POST FOR NOW
		(set 'PostSubject ($POST "subjectline"))
		(set 'PostContent ($POST "post"))
		(set 'PostDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
		(create-record "Posts" Id PosterId PostDate PostSubject PostContent) ; It's the C in CRUD!
		;(displayln "<B>Posting disabled for the moment...</b>")
	)
)
))
(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>")
(display-page)