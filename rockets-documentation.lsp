#!/usr/bin/env newlisp 
(load "/var/www/newlisp-rockets.lisp") 
(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-documentation")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(displayln "<h1>Documentation</h1>")
(displayln "<p>Documentation page is currently under construction.  Please check back for updates.</p>")
(set 'documentation-list (parse (read-file "newlisp-rockets.lisp") "\n"))
(dolist (d documentation-list)
	(if (= (first d) ";") (displayln "<br>" d "")))

(display-footer) 
(display-page)