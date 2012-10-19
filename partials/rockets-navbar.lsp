; partial file rockets-navbar.lsp
;
; displays the navigation menu and highlights the current page

(set 'navbar-list '(("Home" "rockets-main") ("About" "rockets-about") ("Documentation" "rockets-documentation") ("Why Rockets?" "rockets-why") ("Register" "rockets-register")))
; go through the list and set the currently active page
(dolist (r navbar-list)
	(if (= active-page (r 1)) (setf (navbar-list $idx) (push "active" (navbar-list $idx) -1))))

(display-navbar "newLISP on Rockets" navbar-list "rockets-verify")

; display any error messages
(set 'error-messages ($GET "e"))
(if (= error-messages "signin") 
	(display-warning "<strong>Warning!</strong> Username or password not found.  Please try signing in again.")
)
