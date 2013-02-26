#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Delete page (rockets-delete.lsp)
; 
; This just deletes a page
; Version 0.01 (Rockets version shown on page)

(load "Rockets-config.lisp") ; load configuration information
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'Id (integer ($GET "post")))	
(set 'PostId Id) ; for deleting comments	
													
(if (= Rockets:UserId 0) (begin	; only an admin can delete posts
	(delete-record "Posts" Id) 	; this actually deletes the post.
	(delete-record "Comments" PostId) ; we need to delete all associated comments as well.
	)
	(displayln "")) 					; if the user tried to enter the URL manually it will just do nothing.

; Regardless, redirect back to main page.  
(page-redirect "rockets-main.lsp")

