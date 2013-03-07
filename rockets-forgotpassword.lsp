#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-forgotpassword.lsp) - Rockets - Forgot password page 
; 

(load "Rockets-config.lisp") ; load configuration information
(module "crypto.lsp") ; for setting new passwords
(display-header (string RocketsConfig:Name " - Forgot Password"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-navbar")
(display-partial "rockets-common-functions")

(start-div "hero-unit")
	(displayln "<h2>Forgot Password</h2>")
	;(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
	;(displayln "<p>Writing Holmes is an experimental writing site.  Feel free to look around!</p>")
(end-div)

(define (expire-old-tokens)
 (dolist (x RocketsTokens:Tokens)
	;(displayln "<br> " (x 0))
	(if (< (- (date-value) (x 0)) (* 60 60)) 
		(push x remaining-tokens -1)
		;(displayln " over an hour minute old")
	)
 )
	(set 'RocketsTokens:Tokens remaining-tokens)
	;(displayln "<p> New list: " RocketsTokens:Tokens)
)

; start executing page-----------------------

(if (nil? RocketsConfig:AdminEmail) 
	(set 'RocketsConfig:AdminEmail "newlisponrockets@newlisponrockets.com"))

(if (nil? RocketsConfig:SiteURL)
	(set 'RocketsConfig:SiteURL "pleaseconfigureyoursiteurl.com"))

(set 'email-to-send ($POST "email"))

(set 'uuid-to-verify ($GET "u"))

(if (file-info "reset-tokens.lisp") (load "reset-tokens.lisp"))

(if uuid-to-verify (begin
	(set 'uuid-expired-message (string "<p>You have attempted to reset your password, but the one-hour reset window has expired.  Please return to the <a href='rockets-forgotpassword.lsp'>Forgot Password</a> page and try again."))
	; first verify that uuid exists
	(if RocketsTokens:Tokens 
		(begin
			;(displayln "<p>Token file exists")
			(expire-old-tokens)
			(save "reset-tokens.lisp" 'RocketsTokens)
			;(displayln "<p>new tokens: " RocketsTokens:Tokens)
			(if (and RocketsTokens:Tokens (ref uuid-to-verify RocketsTokens:Tokens))
				(begin 
					(set 'new-password ($POST "pass"))
					(set 'confirm-password ($POST "conf"))
					(displayln "<h4>Reset your password</h4>")
					(displayln "<p><form name='forgotpassword' method='POST'>")
					(displayln "<p>Enter new password: &nbsp; <input type=password class=span3 name=pass>")
					(displayln "<p>Type again to confirm: <input type=password class=span3 name=conf>")
					(if (and new-password (= (trim new-password) ""))
						(display-error "You must enter a password"))
					(if (and new-password (!= new-password confirm-password))
						(display-error "Passwords do not match."))
					(displayln "<input type=hidden name=uuid value='" uuid-to-verify "'>")
					(displayln "<p><input type=submit value='Set New Password'></form>") 
					(if (and new-password (= new-password confirm-password)) (begin
						(displayln "<p>Passwords match! Now changing and logging you in.")
						; first find the appropriate email address
						(set 'ref-uuid (ref uuid-to-verify RocketsTokens:Tokens))
						(if ref-uuid (set 'email-to-reset (RocketsTokens:Tokens (first ref-uuid) 2)))
						(displayln "<p>email to change: " email-to-reset)
						(set 'UserEmail email-to-reset)
						(if UserEmail (set 'user-data (first (get-record "Users" UserEmail))))
						(displayln "<P>User data: " user-data)
						(if user-data (begin
							(set 'UserSalt (user-data 3))
							(set 'CookieSalt (user-data 8))
							(set 'UserPasswordHash (crypto:sha1 (string UserSalt new-password)))
							;(displayln "<p>New password: " UserPasswordHash)
							(update-record "Users" UserEmail UserPasswordHash)
							(displayln "<p>Password updated!")
							; now set the cookie
							(set 'UserId (user-data 0))
							(set 'new-cookie (string UserId "|" CookieSalt))
							(set-cookie Blog:rocket-cookie-name new-cookie (date-value 2053 2 28))
							(page-redirect "rockets-main" "e=resetpassword")
						))			
					))
				)
				(displayln uuid-expired-message)
			)
		)
		(displayln uuid-expired-message)
	)
))

(if email-to-send (begin 
	(set 'date-of-email (date-value))
	(set 'uuid-of-email (uuid))
	(push (list date-of-email uuid-of-email email-to-send) RocketsTokens:Tokens -1)
	(expire-old-tokens)
	(save "reset-tokens.lisp" 'RocketsTokens)
	(set 'link-text (string "http://" RocketsConfig:SiteURL "/rockets-forgotpassword.lsp?u=" uuid-of-email ))
	(set 'email-text (string "Either you, or someone pretending to be you, has sent a Reset Password request for the site " RocketsConfig:Name ". To reset your password, click on this link: <a href=" link-text ">" link-text "</a>.  If you did not request a password reset, please ignore this message."))
	(set 'subject-text (string "Reset password request for " RocketsConfig:Name))
	; check if it was a valid email address
	(set 'check-if-valid-email (query (string "SELECT * From Users WHERE UserEmail='" email-to-send "';")))
	(if check-if-valid-email
		(begin
			(displayln "<p>We are sending a message to your email address.  If you don't receive it right away, check your Spam folder.  Click on the link in the email to reset your password.")
			(send-mail email-to-send RocketsConfig:AdminEmail RocketsConfig:Owner subject-text email-text)
		)
		(displayln "<p>Sorry, that email address was not found in our user database.  <a href=rockets-forgotpassword.lsp>Try again.</a>")
	)
	;(displayln "<P>Sending email to: " email-to-send " from " RocketsConfig:AdminEmail " on " date-of-email " code: " uuid-of-email)
	
	)
	(if (not uuid-to-verify) (begin
			(displayln "<form name='forgotpassword' method='POST'>")
			(displayln "<p>Email address: <input type='text' name='email' class='span5'>")
			(displayln "<input type=submit value='Reset password'>")
	))
)

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
