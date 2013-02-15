#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - User verification page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - About"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-about")
(display-partial "rockets-navbar")

(start-div "hero-unit")
	(displayln "<h2>" RocketsConfig:Name "</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
	(displayln "<h3>What is newLISP on Rockets?</h3>")
	(displayln "<P><a href='http://newlisp.org'>newLISP</a> is a fast and flexible scripting language that uses a LISP syntax.")
	(displayln "It's very easy to learn and use.</p>")
	(displayln "<p>Rockets is a framework written in newLISP that is designed for rapid prototyping of web-based applications.")
	(displayln "It uses <a href='http://twitter.github.com/bootstrap/'>Bootstrap</a> for its user interface as well as <a href='http://jquery.com/'>jQuery</a>.  Database functions use <a href='http://sqlite.org'>SQLite</a>.")
	(displayln "<p>The framework is open-source, licensed under the <a href='http://www.gnu.org/licenses/old-licenses/gpl-2.0.html'>GPL</a>.")
	(displayln "The source code for both the framework and this blog are available <a href='https://github.com/newlisponrockets/newLISP-on-Rockets'>here</a>.</p>")
	(displayln "<h3>How about a Hello World?</h3>")
	(displayln "<p>Sure!  Here's one:</p>")
	(displayln "<br>#!/usr/bin/env newlisp")
	(displayln "<br>(load \"newlisp-rockets.lisp\")")
	(displayln "<br>(display-header)")
	(displayln "<br>(display-navbar \"Hello World\")")
	(displayln "<br>(start-div \"hero-unit\")")
	(displayln "<br>&nbsp;&nbsp;(displayln \"Hello, World!\")")
	(displayln "<br>(end-div)")
	(displayln "<br>(display-footer)")
	(displayln "<br>(display-page)")
	(displayln "<h3>Is that it?</h3>")
	(displayln "<p>newLISP on Rockets is currently in the early stages of development.  You are welcome to peruse the <a href='rockets-main.lsp'>blog</a> for updates on the project.")
(end-div)

;(displayln "<p>Debug stuff here...</p>")
; this stuff is subject to change!
;(set 'userlist (get-record "Users"))
;(dolist (z userlist) (displayln "<br>" (z 0) " " (z 1) " " (z 7)))
;(send-mail "jreimeris@shaw.ca" "newlisponrockets@newlisponrockets.com" "Rocket Man" "Test email!" "Will this work? This is a new email btw")
;(displayln "extend table: " (query "ALTER TABLE Posts ADD COLUMN PostComments INTEGER;"))
;(displayln "Posts table: " (query "pragma table_info('Posts');"))

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!