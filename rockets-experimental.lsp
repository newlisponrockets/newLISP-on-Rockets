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

  (displayln "<canvas id=\"" canvas-id "\" width=\"" canvas-width "\" height=\"" canvas-height "\">")
  (displayln "<p>Sorry, we're unable to render this content. Please navigate back to the <a href='/'>home page</a> to continue.</p>")
  (displayln "</canvas>")
  (displayln "<script>")
  (displayln "function draw() {")
  (displayln "  const canvas = document.getElementById(\"" canvas-id "\");")
  (displayln "  if (canvas.getContext) {")
  (displayln "    const ctx = canvas.getContext(\"2d\");")
)

(define (close-canvas)
  (displayln "  }")
  (displayln "}")
  (displayln "draw();")
  (displayln "</script>")
)

(define (draw-image x y imagename width height tiled)
    (setq imagenum (+ imagenum 1))
    (displayln "    const img" imagenum " = new Image();")
    (displayln "    img" imagenum ".onload = () => {")
    (if tiled (begin
      (let ((numx (/ canvas-width width))
           (numy (/ canvas-height height)))
           (displayln "      for (let i = 0; i < " numy "; i++) {")
           (displayln "        for (let j = 0; j < " numx "; j++) {")
           (displayln "          ctx.drawImage(img" imagenum ", j * " width ", i * " height ", " width ", " height ");")
           (displayln "        }")
           (displayln "      }")
           (displayln "    };"))
      )
      (begin 
        (display "      ctx.drawImage(img" imagenum ", " x ", " y )
        (if (or width height) (display ", " width ", " height))
        (displayln ");")
        (displayln "    };")
      )
    )
    (displayln "    img" imagenum".src = \"" imagename "\";");
)

(define (draw-rectangle x y width height filled)
  (if filled
    (displayln "    ctx.fillRect(" x ", " y ", " width ", " height ");")
    (displayln "    ctx.strokeRect(" x ", " y ", " width ", " height ");")
  )
)

(define (draw-text x y text font size serif filled)
  (if filled
    (begin
       (displayln "    ctx.font = \"" size "px " font "\";")
       (displayln "    ctx.fillText(\"" text "\", " x ", " y ");")
    )
    (begin
       (displayln "    ctx.font = \"" size "px " font "\";")
       (displayln "    ctx.strokeText(\"" text "\", " x ", " y ");")
    )
  )
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

(setq imagenum 0)

(setq canvas-width 800)
(setq canvas-height 600)
(setq canvas-id "test1")
(init-canvas canvas-id canvas-width canvas-height)
(draw-rectangle 25 25 300 100)
(draw-text 50 75 "Hello, World!" "serif" 48 false)
(draw-image 0 0 "images/poweredby.png" 100 20 true)
(draw-image 50 175 "images/sun_halloween.jpg" 200 200)
(draw-image 450 175 "images/poweredby.png")
(draw-image 500 175 "images/poweredby.png" 400 100)	
(close-canvas)

(displayln "<p>STUFF GOES HERE...</p>")

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
