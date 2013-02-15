#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - Registration page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - Register"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-register")
(display-partial "rockets-navbar")

(displayln "<h1>Register for the Rockets Blog!</h1>")
(displayln "<p>Tired of having to squint at distorted letters just to register for a new website?  So are we.  So let's try something more fun.</p>")

(load "rocket-list.lisp")
(set 'total-critter-list-num (append RocketReg:rocket-list RocketReg:not-rocket-list))
(dolist (q total-critter-list-num)
	(set 'temp-critter (string q))
	(push temp-critter total-critter-list -1))

; note: "cats" are rockets.  "not-cats" are other vehicles.
(seed (time-of-day))
(do-until (= (length (unique new-cat-list)) 3) (set 'new-cat-list (rand 9 3))) ; keep doing it until you get 3 distinct cats
(set 'new-not-cat-list-temp (rand (- 91 9) 9))
(dolist (y new-not-cat-list-temp) (push (+ y 9) new-not-cat-list -1))

(set 'total-animal-list (append new-cat-list new-not-cat-list))
(set 'total-animal-list (randomize total-animal-list))

(displayln "<h3>Can you find the rockets?</h3>")
(displayln "			<p class='p1'><span>Please click the checkboxes <i>below</i> all <b>three</b> rockets, to prove you are a human.</span></p> ")
(displayln "			<p class='p2'><span>Then, enter a user name, password, and email address below and click 'Register'.</span></p> ")
(displayln "			<p class='p3'><span>You will be registered and signed in automatically.</span></p> ")

(displayln "<form name='register' action='rockets-register-confirm.lsp'>")
	
(displayln "<table border=0>")
(set 'cat-counter 0)
(dolist (z total-animal-list)
	(if (= cat-counter 0) (displayln "<tr>")) 
	(++ cat-counter)
	(displayln "<td>")
	(displayln "<img src='images/rockets/r" (total-critter-list z) ".jpg'>")
	(displayln "<br><input type='checkbox' name='" (total-critter-list z) "' id='" (total-critter-list z) "'>")
	(displayln "</td>")
	(if (> cat-counter 5) (begin (displayln "</tr>") (set 'cat-counter 0)))
)
(displayln "</table>")

(if (= error-messages "few") (begin (display "<br>")
	(display-error "<strong>Warning! Danger Will Robinson!</strong> Not enough rockets! You need to select three of them.")))
(if (= error-messages "many") (begin (display "<br>")
	(display-error "<strong>Warning! Danger Will Robinson!</strong> You selected something that wasn't a rocket!")))

(displayln "<table><tr>")
(displayln "<br><br><br><td width=100>User name:</td><td><input type='text' name='u'><td>")
(if (= error-messages "samename") 
	(display-error "Somebody with the same name or email address is already registered here."))
(if (= error-messages "noname") 
	(display-error "You must enter a user name."))
(displayln "</td></tr>")
(displayln "<td width=100>Password:</td><td><input type='password' name='p'><td>")
(if (= error-messages "nopw") 
	(display-error "You must enter a password."))
(displayln "</td></tr>")
(displayln "<td width=100>Confirm password:</td><td><input type='password' name='c'><td>")
(if (= error-messages "pwmatch") 
	(display-error "Passwords did not match!"))
(displayln "</td></tr>")
(displayln "<td width=100>Email:</td><td><input type='text' name='e'><td>")
(if (= error-messages "noemail") 
	(display-error "You must enter an email address."))
(displayln "</td></tr>")
(displayln "<td width=100><input type='submit' value='Register'></td></tr></form>")
(displayln "</table><br><br><br>")


(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!