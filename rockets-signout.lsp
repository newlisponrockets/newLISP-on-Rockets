#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-signout.lsp) - Rockets - User sign out page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - Sign Out"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-main")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(start-div "hero-unit")
	(displayln "<h2>" RocketsConfig:Name "</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

(if Rockets:UserId
	(begin
		(delete-cookie rocket-cookie-name)
		(page-redirect "rockets-signout")
	)
	(begin
		(displayln "<P>You are now signed out of " RocketsConfig:Name ".</p>")
		(displayln "<br><br>Click <a href='rockets-main.lsp'>here</a> to return to the main page.")
	)
)

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
