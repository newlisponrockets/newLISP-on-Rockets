#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - User verification page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; This page just verifies that a user entered the correct credentials
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header)

(module "crypto.lsp")

(open-database RocketsConfig:Database)
(display-partial "rockets-common-functions")

; set Rockets cookie name (from common functions)
(set 'rocket-cookie-name Blog:rocket-cookie-name)

(set 'UserEmail ($POST "email"))
(set 'UserPassword ($POST "password"))
(set 'page-to-redirect ($POST "activepage"))
; we might not get a redirect page value from the form, so set a default one if it doesn't exit
(if (or (nil? page-to-redirect) (= page-to-redirect "nil")) (set 'page-to-redirect "rockets-main"))

; a lot of this stuff is temporary until we figure out how to make this part of the framework
(set 'sql-result (get-record "Users" UserEmail))
(if sql-result (begin
	(set 'sql-result (first sql-result))
	(set 'sql-user-id (sql-result 0))
	(set 'sql-password-hash (sql-result 2))
	(set 'sql-password-salt (sql-result 3))
	(set 'sql-cookie-salt (sql-result 8))
	(set 'hash-combination (crypto:sha1 (string sql-password-salt UserPassword)))

	(if (= sql-password-hash hash-combination) (begin
		(displayln "<BR><B>Password correct!</B>")
		(set 'temp-cookie-hash (string "user=" (string sql-user-id "|" sql-cookie-salt)))
		(displayln "<BR>Cookie set: " temp-cookie-hash)
		; set a cookie
		(set-cookie rocket-cookie-name temp-cookie-hash (+ (date-value) (* 60 60 24 365))) ; set cookie expiry to one year from now
		(displayln "<br>You have succesfully logged in! Click <a href='rockets-main.lsp'>here</a> to continue.")
		(page-redirect page-to-redirect) 
	)
		(begin 
		(page-redirect page-to-redirect "e=signin") ; tell the application that the username was not recognized.
		)
	)
)
(begin
	(page-redirect page-to-redirect "e=signin") ; tell the application that the username was not recognized.
))

(close-database)
(display-footer RocketConfig:Owner)
(display-page) ; this is needed to actually display the page!
