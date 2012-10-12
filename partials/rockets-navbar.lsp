; partial file rockets-navbar.lsp
;
; displays the navigation menu and highlights the current page

(set 'navbar-list '(("Home" "rockets-main") ("About" "rockets-about") ("Why Rockets?" "rockets-why") ("Register" "rockets-register")))

(dolist (r navbar-list)
	(if (= active-page (r 1)) (setf (navbar-list $idx) (push "active" (navbar-list $idx) -1))))

(display-navbar "newLISP on Rockets" navbar-list "rockets-verify")