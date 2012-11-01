#!/usr/bin/env newlisp 
(load "/var/www/newlisp-rockets.lisp") 
(display-header)
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-item")
(set 'Id (integer (force-parameters 1 ($GET "p"))))
(set 'edit-post (force-parameters 1 ($GET "edit")))
(if Id (extend active-page (string ".lsp?p=" Id))) ; in case user logs in and wants to return to this exact page
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(if Id (begin
	(set 'post-content (first (get-record "Posts" Id)))
	(display-individual-post post-content true Id); true= display comments, Id=link to page
)
	(displayln "<p>Sorry! No post was requested.")
)

(if edit-post (begin
	(displayln "<a name='edit'></a>")
	(display-post-box "Edit post..." "postsomething" "rockets-edit-post" "subjectline" "replybox" "Update Message" Id (post-content 3) (post-content 4))
))

(display-footer) 
(display-page)