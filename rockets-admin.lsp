#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-profile.lsp) - Rockets - Site admin / upload page 
; 
; This page allow the site owner to change the configuration of 
; their newLISP on Rockets system.
;
; Written 2018 by Jeremy Reimer

(load "Rockets-config.lisp") ; load configuration information

(display-header (string RocketsConfig:Name " - Admin Page"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-profile")
(display-partial "rockets-navbar")

(displayln "<h2>Admin Page</h2>")

(if (= Rockets:UserId 0) (begin ; admin-only section
        (displayln "<form name='admin'>")
        (displayln "<h2>Site configuration</h2>")
        (displayln "<p>Site name: <input type='text' name='shortname' value='" RocketsConfig:ShortName "'></p>")
        (displayln "<h2>Top menu navigation</h2>")
        ; display all navigation

        (dolist (n RocketsNavigation:navbar-list)
            (displayln "<p>Menu item: " $idx ": <input type='text' name='menuname" $idx "' value='" (n 0) "'>")
            (displayln "Page destination: <input type='text' name='menuvalue" $idx "' value='" (n 1) "'>")
        )
        (displayln "<p><input type='submit' value='Save changes'>")
        (displayln "</form>")
        )
	(displayln "<p>Sorry, you must be signed in to an admin account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
