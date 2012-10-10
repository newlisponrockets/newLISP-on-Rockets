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
	(set 'cookie-user-number (last (parse cookie-user-number-full "=")))
	(set 'rocket-cookie-salt (first (first (query (string "SELECT CookieSalt from Users WHERE UserId=" cookie-user-number)))))
	
	; now that we have the salt
	(set 'correct-salt (find (string "|" rocket-cookie-salt) cookie-check))
	(if correct-salt (begin
		;(displayln "<BR>FOUND THE RIGHT SALT! MMmmmm")
		(set 'user-part (last (parse (first (parse cookie-check "|")) "=")))
		;(displayln "<BR> User: " user-part)
		(set 'load-user-sql-data (first (query (string "SELECT * From Users WHERE UserId=" user-part))))
		;(displayln "<BR> User data: " load-user-sql-data)
		(set 'Rockets:UserId (load-user-sql-data 0))
		(set 'Rockets:UserEmail (load-user-sql-data 1))
		(set 'Rockets:UserPosts (load-user-sql-data 4))
		(set 'Rockets:UserName (load-user-sql-data 7))
		(set 'Rockets:CookieSalt (load-user-sql-data 8))
	
	))
))