#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - User verification page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(display-header)
(display-navbar "newLISP on Rockets" '(("Home" "rockets-main") ("About" "rockets-about") ("Contact" "rockets-contact") ("Register" "rockets-register" "active")) "rockets-verify")

(start-div "hero-unit")
	(displayln "<h2>The newLISP on Rockets Blog</h2>")
	(displayln "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")
(end-div)

(module "crypto.lsp")

(displayln "POST: " ($POST))

(open-database "ROCKETS-BLOG")

; set Rockets cookie name (will be from a file later)
(set 'rocket-cookie-name "rockets-4dckq3-e4jcx-2wgxc")

(displayln "Checking for user cookie:" ($COOKIES rocket-cookie-name)) 

(set 'temp-pw "rocket-horse-5")
(seed (time-of-day))
(set 'temp-salt (crypto:md5 (string (random))))
(set 'temp-crypto (crypto:sha1 (string temp-salt temp-pw)))
(set 'user-check ($POST "email"))
(set 'user-password ($POST "password"))
;(displayln "<br>Some sort of thing: " temp-pw)
(displayln "<br>Some sort of salt: " temp-salt)
(displayln "<br>Some sort of hash: " temp-crypto)

; let's set up the user database temporarily
;(set 'sql-create-user-table "CREATE TABLE Users (UserId INTEGER PRIMARY KEY, UserEmail TEXT, UserPasswordHash TEXT, UserSalt TEXT, UserPosts INTEGER, UserAchievements TEXT, UserReadPosts TEXT)")
;(set 'sql-display-table "Pragma table_info('Users');")
;(set 'sql-drop-table "DROP TABLE Users;")
;(set 'sql-create-user (string "INSERT INTO Users (UserID, UserEmail, UserPasswordHash, UserSalt, UserPosts) VALUES (0, 'newlisponrockets@newlisponrockets.com', '" temp-crypto  "', '" temp-salt "', 11);"))
(set 'sql-show-users (string "SELECT * FROM Users WHERE UserEmail='" user-check "';"))
; a lot of this stuff is temporary until we figure out how to make this part of the framework
(set 'sql-result (first (query sql-show-users)))
(displayln "<BR>SQL result: " sql-result)
(set 'sql-user-id (sql-result 0))
(set 'sql-password-hash (sql-result 2))
(set 'sql-password-salt (sql-result 3))
(set 'hash-combination (crypto:sha1 (string sql-password-salt user-password)))

(displayln "<BR>User entered: " user-password)
(displayln "<BR>Hashed pw: " sql-password-hash)
(displayln "<BR>Combine with salt: " (string sql-password-salt user-password))
(displayln "<BR>Hash that combination: " hash-combination)

(if (= sql-password-hash hash-combination) (begin
	(displayln "<BR><B>Password correct!</B>")
	(set 'temp-uuid "32E933AC-3225-414D-9041-3328A919A66E") ; this will be stored somewhere in the DB 
	(set 'temp-cookie-hash (string "user=" (string sql-user-id "|" temp-uuid)))
	(displayln "<BR>Cookie set: " temp-cookie-hash)
	; set a cookie
	(set-cookie rocket-cookie-name temp-cookie-hash (date-value 2013 2 28))
	(displayln "<br>You have succesfully logged in! Click <a href='rockets-main.lsp'>here</a> to continue.")
	(page-redirect "rockets-main.lsp") 
)
	(displayln "<p><b>Sorry, your user email or your password were not recognized.  Please try again.</b></p>")
)

;(displayln (env))

(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!
