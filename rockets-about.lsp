#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - User verification page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-navbar "newLISP on Rockets" '(("Home" "rockets-main") ("About" "rockets-about" "active") ("Contact" "rockets-contact") ("Register" "rockets-register")) "rockets-verify")

(start-div "hero-unit")
	(displayln "<h2>The newLISP on Rockets Blog</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
	(displayln "<h3>What is newLISP on Rockets?</h3>")
	(displayln "<P><a href='http://newlisp.org'>newLISP</a> is a fast and flexible scripting language that uses a LISP syntax.")
	(displayln "It's very easy to learn and use.</p>")
	(displayln "<p>Rockets is a framework written in newLISP that is designed for rapid prototyping of web-based applications.")
	(displayln "The framework is open-source, licensed under the <a href='http://www.gnu.org/licenses/old-licenses/gpl-2.0.html'>GPL</a>.")
(end-div)

(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!