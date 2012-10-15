; rockets-common-functions.lsp
;
; partial file with global variables and functions that are common to the blog but not to Rockets itself
;

; GLOBAL VARIABLES - under the context "Blog" to distinguish from framework context "Rockets"
(set 'Blog:posts-per-page 10) ; number of posts per page

; this function displays an individual post with headers and the post itself
(define (display-individual-post list-post-data)
	(displayln "<h4><a href='rockets-item.lsp?p=" (list-post-data 0) "'>" (list-post-data 3) "</a></h4>")
	(displayln "<br><b>Post #:</b> " (list-post-data 0) )
	(displayln "<BR><B>Date:</b> " (list-post-data 2) "")
	(displayln "<br><B>Author:</b> " (author-name (list-post-data 1)) "")
	(displayln "<br><br><p>" (format-for-web (list-post-data 4)) "</p>")
	(if (= Rockets:UserId 0) (displayln "<br><a class='btn btn-danger' href='rockets-delete.lsp?post=" (list-post-data 0) "'>Delete post</a>"))
	(displayln "<hr>")
)

; THIS IS TEMPORARY TAKE OUT AS SOON AS WE HAVE VALIDATION
(define (author-name str-author-id)
	(case str-author-id
		("0" "Rocket Man")))