#!/usr/bin/env newlisp 
(load "newlisp-rockets.lisp") 
(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - Post"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-item")
(set 'Id (integer (force-parameters 1 ($GET "p"))))
(set 'edit-post (force-parameters 1 ($GET "edit")))
(set 'forum-view-post (force-parameters 1 ($GET "f")))

(if Id (extend active-page (string ".lsp?p=" Id))) ; in case user logs in and wants to return to this exact page
(if forum-view-post (extend active-page (string "&f=true")))
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(define (display-item)

	(if Id (begin
		(set 'post-content (get-record "Posts" Id))
		(if post-content (begin
			(display-individual-post (first post-content) true Id) ; true= display comments, Id=link to page
			(if Rockets:UserId (begin ; check to see if user is signed in
				(if (not (find (string Id "-") Rockets:UserReadPosts)) ; check to see if post is not in user's read list
					(begin
						(push (string Id "-") Rockets:UserReadPosts -1) ; add it to read list
						(set 'UserId Rockets:UserId) ; get variables ready to write to database
						(set 'UserReadPosts Rockets:UserReadPosts)
						(update-record "Users" UserId UserReadPosts)
					))
			)))
			(displayln "<p>Sorry! We couldn't find that post."))
		)
		(displayln "<p>Sorry! We couldn't process your request, probably due to a timeout.  Please refresh the page and try again.")
	)

	(if edit-post (begin
		(displayln "<a name='edit'></a>")
		(set 'post-content (first post-content))
		(display-post-box "Edit post..." "postsomething" "rockets-edit-post" "subjectline" "replybox" "Update Message" Id (post-content 3) (post-content 4))
	))
)

(case RocketsConfig:IndividualPageType
     (0 (display-item))
     (1 (start-div "row-fluid") (start-div "span4") (display-partial "rockets-leftpanel") (end-div) 
        (start-div "span8") (display-item) (end-div) (end-div) )
     (2 (start-div "row-fluid") (start-div "span3") (display-partial "rockets-leftpanel") (end-div)
        (start-div "span6") (display-item) (end-div)
        (start-div "span3") (display-partial "rockets-rightpanel") (end-div) (end-div))
)

(display-footer RocketsConfig:Owner) 
(display-page)