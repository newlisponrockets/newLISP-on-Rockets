; rockets-common-functions.lsp
;
; partial file with global variables and functions that are common to the blog but not to Rockets itself
;

; GLOBAL VARIABLES - under the context "Blog" to distinguish from framework context "Rockets"
(set 'Blog:posts-per-page 10) ; number of posts per page

; this function displays an individual post with headers and the post itself
; also shows comments if bool-show-comments is true, and allows a logged-in user to reply
(define (display-individual-post list-post-data bool-show-comments str-linkback-id)
	(displayln "<h4><a href='rockets-item.lsp?p=" (list-post-data 0) "'>" (list-post-data 3) "</a></h4>")
	(displayln "<br><b>Post #:</b> " (list-post-data 0) )
	(displayln "<BR><B>Date:</b> " (list-post-data 2) "")
	(displayln "<br><B>Author:</b> " (author-name (list-post-data 1)) "")
	(displayln "<br><br><p>" (format-for-web (list-post-data 4)) "</p>")
	(if (= Rockets:UserId 0) (displayln "<br><a class='btn btn-danger' href='rockets-delete.lsp?post=" (list-post-data 0) "'>Delete post</a>"))
	; print reply button if we're not on main page and if valid account is logged in
	(if (and Rockets:UserId (not bool-show-comments)) (begin
		(displayln "<a class='btn btn-primary' href='rockets-item.lsp?p=" (list-post-data 0) "&r=1#reply'>Reply to post</a>")))
	(if (list-post-data 5) (begin				; show number of comments if any, and show actual comments in a list if requested
		(displayln "<p><i>Comments:</i> " (list-post-data 5) "</p>")))
	(if bool-show-comments (begin
		(displayln "<a name='reply'></a>") 	; display an html anchor so we can jump to it
		(if (list-post-data 5) (begin 			; display all comments here if they exist
			(set 'PostId (int (list-post-data 0)))
			(set 'post-comments (get-record "Comments" PostId))
			(dolist (p post-comments)
				(displayln "<br><i>" (format-for-web (p 5)) "</i>")
				(displayln "<br>&nbsp;&nbsp;&mdash; " (author-name (p 2)) " on " (p 3))
				(displayln "<hr>"))
		))		
		(if Rockets:UserId (begin					; show the comment reply box if a user is logged in
			(display-post-box "Post a reply..." "postsomething" "rockets-comment-post.lsp" "subjectline" "replybox" "Post Message" str-linkback-id)
		))
	))
	(displayln "<hr>")
)

; Get the user name given the user Id
(define (author-name str-author-id)
	(set 'get-user-name-query (query (string "SELECT UserName FROM Users WHERE UserId=" str-author-id ";")))
	(if get-user-name-query 
		(set 'result-name (first (first get-user-name-query)))
		(set 'result-name "Unknown User"))
)