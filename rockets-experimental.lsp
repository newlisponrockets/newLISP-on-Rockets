#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-experimental.lsp) - Rockets - Experimental HTML5 Canvas page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

# Canvas functions (will be moved to a partial file later)
(define (init-canvas canvas-id canvas-width canvas-height)

  (displayln "<canvas id=\"" canvas-id "\" width=\"" canvas-width "\" height=\"" canvas-height "\"></canvas>")
)

(load "Rockets-config.lisp") ; load configuration information
(display-header)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in

(display-partial "rockets-navbar")

(start-div "hero-unit")
	(displayln "<h2>Experimental Rockets 3.0 Test page</h2>")
	(displayln "<p>Please proceed with caution!</p>")
(end-div)

;(displayln "<p>Debug stuff here...</p>")
; this stuff is subject to change!

(setq canvas-width 800)
(setq canvas-height 600)
(setq canvas-id "test1")
(init-canvas canvas-id canvas-width canvas-height)


(displayln "<p>STUFF GOES HERE...</p>")

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
