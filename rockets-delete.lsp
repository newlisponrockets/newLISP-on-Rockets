#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Delete page (rockets-delete.lsp)
; 
; This just deletes a page
; Version 0.01 (Rockets version shown on page)

(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'Id (integer ($GET "post")))		
													
(if (= Rockets:UserId 0) 			; only an admin can delete posts
	(delete-record "Posts" Id) 	; this actually deletes the query.
	(displayln "")) 					; if the user tried to enter the URL manually it will just do nothing.

; Regardless, redirect back to main page.  
(page-redirect "rockets-main.lsp")

