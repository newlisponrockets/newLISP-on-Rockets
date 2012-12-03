#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-profile.lsp) - Rockets - User profile page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-profile")
(display-partial "rockets-navbar")

(displayln "<h2>User Profile</h2>")

(if Rockets:UserId (begin
	(displayln "<p>User Name: " Rockets:UserName)
	(displayln "<p>User Email: " Rockets:UserEmail)
	(displayln "<p>Total Posts: " Rockets:UserPosts)
	; update birth date if user hit the Save button
	(if ($POST "date") (begin 
		(set 'temp-date ($POST "date"))
		(set 'UserBirthDate (string (slice temp-date 6 4) "-" (slice temp-date 3 2) "-" (slice temp-date 0 2) " 00:00:00.000"))
		(set 'UserId Rockets:UserId)
		(if (update-record "Users" UserId UserBirthDate)
			(begin (display-success "Birth date saved!") (set 'Rockets:UserBirthDate temp-date))
			(display-error "Error updating birth date. :("))
	))
	; get birthdate from database
	(if Rockets:UserBirthDate 
		(set 'show-birthdate Rockets:UserBirthDate)
		(set 'show-birthdate "01-01-1980")) ; default if not set
	(displayln "<form name='test' method='POST'>")
	(form-datepicker "Enter your birth date" "date" show-birthdate "dp1")
	(displayln "<input type='submit' value='Save'>")
	(displayln "</form>")
	(displayln "<p>Avatar: <img src='images/avatars/" Rockets:UserAvatar "' width=64 height=64")
	(displayln "<hr>")
	(displayln "<p>Upload new avatar (all avatars scaled to 64x64 pixels): <form name='FileUpload' action='rockets-avatarupload.lsp' method='POST' enctype='multipart/form-data'><input type='file' id='uploadName' name='uploaded_data' onChange='this.form.textname.value = this.value'><input type='hidden' name='textname'><input type='submit' value='Upload' name='submit'></form>")
	(if (= Rockets:UserId 0) (begin ; admin-only section
		(displayln "<p>User list:")
		(set 'userlist (get-record "Users"))
		(display-table '("User Id" "User Email" "Password Hash" "Salt" "Posts" "Achievements" "Read Posts" "User Name" "Cookie Salt" "User Avatar") userlist "hover")			
	))
	) ; -- end section that shows if the user is signed in
	(displayln "<p>You must be signed in to view your user profile.</p>")
)



(close-database)
(display-footer "Rocket Man")
(display-page) ; this is needed to actually display the page!