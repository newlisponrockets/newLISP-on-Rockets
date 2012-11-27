; (rockets-checksignin.lsp)
;
; This is a partial file... all it does is check the validity of the user's sign-in cookie
; and if it is a valid cookie, retrieves the user data.  If it's not a valid cookie or the cookie
; isn't there, it simply doesn't set the appropriate variables
;
; set Rockets cookie name (will be from a file later)
(set 'rocket-cookie-name "rockets-4dckq3-e4jcx-2wgxc")

(set 'cookie-check ($COOKIES rocket-cookie-name))

;(displayln "Checking for user cookie:" cookie-check) 
; okay first we have to find the appropriate cookie salt based on the user number
(if cookie-check (begin 
	(set 'cookie-user-number-full (first (parse cookie-check "|")))
	(set 'UserId (last (parse cookie-user-number-full "=")))
	;(set 'rocket-cookie-salt (get-record "Users" UserId "CookieSalt")) ; have to modify (get-record) first
	(set 'rocket-cookie-salt (query (string "SELECT CookieSalt from Users WHERE UserId=" UserId)))
	(if rocket-cookie-salt (set 'rocket-cookie-salt (first (first rocket-cookie-salt)))) ; fixes weird bug when you block cookies
	; now that we have the salt, check if it exists in the cookie
	(set 'correct-salt (find (string "|" rocket-cookie-salt) cookie-check))
	(if correct-salt (begin
		;(displayln "<BR>FOUND THE RIGHT SALT! MMmmmm")
		(set 'UserId (last (parse (first (parse cookie-check "|")) "=")))
		;(displayln "<BR> User: " UserId)
		(set 'load-user-sql-data (first (get-record "Users" UserId)))
		;(set 'load-user-sql-data (first (query (string "SELECT * From Users WHERE UserId=" user-part))))
		;(displayln "<BR> User data: " load-user-sql-data)
		(set 'Rockets:UserId (load-user-sql-data 0))
		(set 'Rockets:UserEmail (load-user-sql-data 1))
		(set 'Rockets:UserSalt (load-user-sql-data 3))
		(set 'Rockets:UserPosts (load-user-sql-data 4))
		(set 'Rockets:UserName (load-user-sql-data 7))
		(set 'Rockets:CookieSalt (load-user-sql-data 8))
		(set 'Rockets:UserAvatar (load-user-sql-data 9))
		(set 'Rockets:UserBirthDate (load-user-sql-data 10))
		(if (and Rockets:UserBirthDate (= (length Rockets:UserBirthDate) 23)) ; change SQLite format to MM-DD-YYYY format
			(set 'Rockets:UserBirthDate (string (slice Rockets:UserBirthDate 8 2) "-" (slice Rockets:UserBirthDate 5 2) "-" (slice Rockets:UserBirthDate 0 4))))
		(if (nil? Rockets:UserAvatar) (set 'Rockets:UserAvatar "unknown.png"))
	))
))