#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-signout.lsp) - Rockets - User sign out page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-navbar "newLISP on Rockets" '(("Home" "rockets-main") ("About" "rockets-about") ("Why Rockets?" "rockets-why") ("Register" "rockets-register" "active")) "rockets-verify")

(start-div "hero-unit")
	(displayln "<h2>The newLISP on Rockets Blog</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

(if Rockets:UserId
	(begin
		(delete-cookie rocket-cookie-name)
		(displayln "<P>You are now signed out of the newLISP on Rockets Blog.</p>")
		(displayln "<br><br>Click <a href='rockets-main.lsp'>here</a> to return to the main page.")
	)
	(displayln "<P>You were not signed in.</p>")
)

(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!
(exit)