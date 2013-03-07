#!/usr/bin/env newlisp 
(load "/var/www/newlisp-rockets.lisp") 
(load "Rockets-config.lisp") ; load configuration information
(display-header)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-register-confirm")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

; set Rockets cookie name (will be from a file later)
(set 'rocket-cookie-name "rockets-4dckq3-e4jcx-2wgxc")

(module "crypto.lsp")

(set 'UserPassword ($GET "p"))
(set 'UserConfirmPassword ($GET "c"))
(set 'UserName ($GET "u"))
(set 'UserEmail ($GET "e"))

(load "rocket-list.lisp") ; load up RocketReg:rocket-list and RocketReg:not-rocket-list
(set 'found-rockets 0)
(dolist (z RocketReg:rocket-list)
	(display "<br>**" (string "r" z) " GET: " ($GET (string z)))
	(if ($GET (string z)) (++ found-rockets))
	(set (sym (string "r" z)) ($GET (string z)))
	;(print (sym (string "k" z)))
)
(set 'found-not-rockets 0)
(dolist (q RocketReg:not-rocket-list)
	(if ($GET (string q)) (++ found-not-rockets))
)

(displayln "<P>>>TOTAL FOUND ROCKETS: " found-rockets)
(displayln "<P>>>TOTAL FOUND NOTROCKETS: " found-not-rockets)

(if (< found-rockets 3) (page-redirect "rockets-register" "e=few"))
(if (> found-not-rockets 0) (page-redirect "rockets-register" "e=many"))

; now check these things to see if they are acceptable

; first check to see if the user name or email exists in the database already
(if (query (string "SELECT * FROM Users WHERE UserName='" (trim UserName) "'")) (page-redirect "rockets-register" "e=samename"))
(if (query (string "SELECT * FROM Users WHERE UserEmail='" (trim UserEmail) "'")) (page-redirect "rockets-register" "e=samename"))
(if (= (trim UserName) "") (page-redirect "rockets-register" "e=noname"))
(if (= (trim UserPassword) "") (page-redirect "rockets-register" "e=nopw"))
(if (!= UserPassword UserConfirmPassword) (page-redirect "rockets-register" "e=pwmatch"))
(if (= (trim UserEmail) "") (page-redirect "rockets-register" "e=noemail"))

; everything checked out, let's register this person!
(displayln (query "pragma table_info('Users')"))
(set 'UserId (+ (int (first (first (query "select count(*) from Users")))) 1))
(displayln "<br>UserId: " UserId)
(displayln "<br>UserEmail: " UserEmail)
(displayln "<br>UserPassword: " UserPassword)
(set 'UserSalt (uuid))
(set 'UserPasswordHash (crypto:sha1 (string UserSalt UserPassword)))
(displayln "<br>UserSalt: " UserSalt)
(displayln "<br>UserPasswordHash: " UserPasswordHash)
(set 'CookieSalt (uuid))
(displayln "<br>UserSalt: " CookieSalt)
(set 'UserPosts 0)
(displayln "<br>UserPosts: " UserPosts)
(displayln "<br>UserName: " UserName)

(create-record "Users" UserId UserEmail UserPasswordHash UserSalt UserPosts CookieSalt UserName)
(displayln (query (string "select * from Users")))
; set the cookie
(set 'new-cookie (string UserId "|" CookieSalt))
(set-cookie rocket-cookie-name new-cookie (date-value 2053 2 28))

; one last thing, send a nice email welcoming the new user!
(set 'welcome-email "Thank you for registering.  If you have any questions, please don't hesitate to email me or post a comment on the blog.\n\nSincerely,\n\nRocket Man")
(send-mail UserEmail "newlisponrockets@newlisponrockets.com" "Rocket Man" "Welcome to the newLISP on Rockets blog!" welcome-email)
; and send a mail to me so that I know that a new user registered!
(set 'new-user-registered-mail (string "A new user by the name of: " UserName " just registered with the email address " UserEmail "."))
(send-mail "newlisponrockets@newlisponrockets.com" "newlisponrockets@newlisponrockets.com" "Rocket Man" "A new user has registered on the newLISP on Rockets blog" new-user-registered-mail)


(page-redirect "rockets-main" "e=newuser")

(display-footer RocketsConfig:Owner) 
(display-page)