; partial file rockets-navbar.lsp
;
; displays the navigation menu and highlights the current page

(set 'navbar-list '(("About" "rockets-about") ("Docs" "rockets-documentation") ("Forum" "rockets-forum") ("Why Rockets?" "rockets-why") ("Register" "rockets-register")))
; go through the list and set the currently active page
(dolist (r navbar-list)
	(if (= active-page (r 1)) (setf (navbar-list $idx) (push "active" (navbar-list $idx) -1))))

(display-navbar RocketsConfig:ShortName navbar-list "rockets-verify")

; display any error messages, welcome messsages, etc.
(set 'error-messages ($GET "e"))
(if (= error-messages "signin") 
	(display-warning "<strong>Warning!</strong> Username or password not found.  Please try signing in again."))
(if (= error-messages "newuser")
	(display-success (string "Thank you for registering on " RocketsConfig:Name "!  You are now signed in.")))
