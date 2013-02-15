#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - 404 page not found page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in

(display-partial "rockets-navbar")

(start-div "hero-unit")
	(displayln "<h2>Sorry, I couldn't find that page...</h2>")
	(displayln "<p>I tried my best, I really did!  But at the end of the day, I'm just a script running on a web server.</p>")
	(displayln "<p>I can only do so much.  Try starting back at the <a href='" $BASE_PATH "rockets-main.lsp'>home page</a>.")
(end-div)

;(displayln "<p>Debug stuff here...</p>")
; this stuff is subject to change!
;(set 'userlist (get-record "Users"))
;(dolist (z userlist) (displayln "<br>" (z 0) " " (z 1) " " (z 7)))
;(displayln "extend table: " (query "ALTER TABLE Posts ADD COLUMN PostComments INTEGER;"))
;(displayln "Posts table: " (query "pragma table_info('Posts');"))

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!