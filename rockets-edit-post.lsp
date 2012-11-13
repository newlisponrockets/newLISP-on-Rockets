#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Edit Posting Page rockets-edit-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(open-database "ROCKETS-BLOG")

(displayln "<P>$POST data: " ($POST))

; update post 
(display-partial "rockets-checksignin") ; see if the user is signed in
(if (= Rockets:UserId 0) (set 'continue true) (set 'continue nil)) ; Only admin user can edit posts, since only admin can make them
(if continue (begin
(if (and ($POST "post") ($POST "subjectline"))
	(begin
		(set 'Id (int ($POST "linkbackid")))
		(set 'PosterId Rockets:UserId) ; ONLY I MAY POST FOR NOW
		(set 'PostSubject ($POST "subjectline"))
		(set 'PostContent ($POST "post"))
		(set 'PostDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
		(update-record "Posts" Id PosterId PostDate PostSubject PostContent) ; It's the U in CRUD!
		;(displayln "<B>Posting disabled for the moment...</b>")
	)
)
))
(displayln "Id: " Id)
;(displayln "<a href='rockets-item.lsp?p=" Id "'>Click here to return to the main page.</a>")
(page-redirect "rockets-item" (string "p=" Id))
(display-page)