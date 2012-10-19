#!/usr/bin/env newlisp 
(load "/var/www/newlisp-rockets.lisp") 
(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-item")
(set 'Id (integer ($GET "p")))
(if Id (extend active-page (string ".lsp?p=" Id))) ; in case user logs in and wants to return to this exact page
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(if Id (begin
	(display-individual-post (first (get-record "Posts" Id)) true Id); true= display comments, Id=link to page
)
	(displayln "<p>Sorry! No post was requested.")
)

(display-footer) 
(display-page)