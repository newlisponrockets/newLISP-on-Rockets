#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Delete page (rockets-delete.lsp)
; 
; This just deletes a page
; Version 0.01 (Rockets version shown on page)

(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'Id (integer ($GET "post")))		; security will come from authorization.. if user isn't logged in and isn't admin
													; no amount of entering this URL will work.
(if (= Rockets:UserId 0) 					; only an admin can delete posts
	(delete-record "Posts" Id) ; this actually deletes the query.
	(displayln "<p>Sorry, only users with admin access may delete posts.</p>"))

; this is temporary, and we also have to make a redirect.  Have to figure out how to do headers and stuff.
(displayln "<a href='rockets-main.lsp'>Post deleted. Click here to return to the main page.</a>")
(page-redirect "rockets-main.lsp")
(display-page)
