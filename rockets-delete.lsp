#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Delete page (rockets-delete.lsp)
; 
; This page shows a confirmation for the user before deleting a post

(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - Confirm delete"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog
(display-partial "rockets-navbar")
(set 'Id (integer ($GET "post")))	
(set 'PostId Id) ; for deleting comments	
													
(if (= Rockets:UserId 0) (begin	; only an admin can delete posts
	(displayln "<br>")
	(display-h1 "Confirm post deletion")
	(displayln "Are you sure you want to delete this post?")
	(displayln "<br><br>")
	(display-button-green "No - don't delete" "rockets-main")
	(display-button-red "Yes - delete post" (string "rockets-delete-confirm.lsp?post=" Id))
	(displayln "<br>")
	)
	(displayln "")
) 					; if the user tried to enter the URL manually it will just do nothing.

; Regardless, redirect back to main page.  
;(page-redirect "rockets-main.lsp")
(display-footer RocketsConfig:Owner)
(display-page)
