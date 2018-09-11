#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-profile.lsp) - Rockets - Site admin / upload page 
; 
; This page allow the site owner to upload files directly, manipulate 
; the source control system, change theme, etc. 
;
; Written 2013 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information

(display-header (string RocketsConfig:Name " - Admin Page"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-profile")
(display-partial "rockets-navbar")

(displayln "<h2>Admin Page</h2>")

(if (= Rockets:UserId 0) (begin ; admin-only section
        (displayln "<p>Site name: " RocketsConfig:ShortName "</p>")
        )
	(displayln "<p>Sorry, you must be signed in to an administer account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
