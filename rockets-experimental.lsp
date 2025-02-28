#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-experimental.lsp) - Rockets - Experimental HTML5 Canvas page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

# Canvas functions (will be moved to a partial file later)
(define (init-canvas canvas-id canvas-width canvas-height image-list)

  (displayln "<canvas id=\"" canvas-id "\" width=\"" canvas-width "\" height=\"" canvas-height "\">")
  (displayln "<p>Sorry, we're unable to render this content. Please navigate back to the <a href='/'>home page</a> to continue.</p>")
  (displayln "</canvas>")
  (displayln "<script>")
  (if image-list (begin
    (dolist (img image-list)
      (displayln "const img" $idx " = new Image();")
    )
    (displayln "function init() {")
    (dolist (img image-list)
      (displayln "  img" $idx ".src  = \"" img "\";")
    )
  ))
  (displayln "  window.requestAnimationFrame(draw);")
  (displayln "}")
  (displayln "function draw() {")
  (displayln "  const ctx = document.getElementById(\"" canvas-id "\").getContext(\"2d\");")
  (displayln "  ctx.globalCompositeOperation = \"source-over\";")
)

(define (close-canvas)
  # TEMPORARY add rotation animation
  #(displayln "  const time = new Date();")
  #(displayln "  ctx.rotate( ((2 * Math.PI) / 60) * time.getSeconds() );")
  (displayln "  ctx.rotate(0.01);")
  (displayln "}")
  (displayln "init();")
  (displayln "</script>")
)

(define (save-state)
  (displayln "    ctx.save();")
)

(define (restore-state)
  (displayln "  ctx.restore();")
)

(define (clear-rect x y width height)
  (displayln "  ctx.clearRect(" x ", " y ", " width ", " height ");")
)

(define (req-anim-frame)
  (displayln "  window.requestAnimationFrame(draw);")
)

(define (draw-image imagename x y width height tiled)
    (setq imagenum (find imagename image-list 1))
    (if tiled (begin
      (let ((numx (/ canvas-width width))
           (numy (/ canvas-height height)))
           (displayln "  for (let i = 0; i < " numy "; i++) {")
           (displayln "    for (let j = 0; j < " numx "; j++) {")
           (displayln "      ctx.drawImage(img" imagenum ", j * " width ", i * " height ", " width ", " height ");")
           (displayln "    }")
           (displayln "  };")
      ))
      (begin 
        (display "  ctx.drawImage(img" imagenum ", " x ", " y )
        (if (or width height) (display ", " width ", " height))
        (displayln ");")
      )
    )
)

(define (draw-rectangle x y width height filled)
  (if filled
    (displayln "  ctx.fillRect(" x ", " y ", " width ", " height ");")
    (displayln "  ctx.strokeRect(" x ", " y ", " width ", " height ");")
  )
)

(define (draw-text x y text font size serif filled)
  (if filled
    (begin
       (displayln "  ctx.font = \"" size "px " font "\";")
       (displayln "  ctx.fillText(\"" text "\", " x ", " y ");")
    )
    (begin
       (displayln "  ctx.font = \"" size "px " font "\";")
       (displayln "  ctx.strokeText(\"" text "\", " x ", " y ");")
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
(setq image-list '(
  "images/sun_halloween.jpg"
  "images/poweredby.png"
  "images/16pxdither3.png"
))
(init-canvas canvas-id canvas-width canvas-height image-list)
(clear-rect 0 0 canvas-width canvas-height)
(draw-image "16pxdither3" 0 0 16 16 true)
(draw-rectangle 25 25 300 100)
(draw-text 50 75 "Hello, World!" "serif" 48 true true)
(draw-image "poweredby" 50 175  200 200)
(req-anim-frame)
(draw-image "poweredby" 150 175)
(draw-image "sun_halloween" 50 275 400 400)	
(close-canvas)

(displayln "<p>STUFF GOES HERE...</p>")

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
