#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Main Page
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; Version 0.01 (Rockets version shown on page)

(print-header)
(print-image "rockets.png")
(println "<h2>The newLISP on Rockets Blog</h2>")
(println "<P>Currently running newLISP on Rockets version: " $ROCKETS_VERSION "</p>")

; THIS IS TEMPORARY TAKE OUT AS SOON AS WE HAVE VALIDATION
(define (author-name str-author-id)
	; temporary -- I am the only valid poster for the moment
	(case str-author-id
		("0" "Rocket Man")))

(open-database "ROCKETS-BLOG")

; get all existing posts
(set 'posts-query-sql (string "SELECT * from Posts;"))
(set 'posts-result (reverse (query posts-query-sql))) ; reverse it so newest posts first
; print out all posts
(dolist (x posts-result)
	(println "<br><a href='rockets-delete.lsp?post=" (x 0) "'>Delete post</a>")
	(println "<BR><B>Date:</b> " (x 2) "")
	(println "<BR><B>Subject:</b> " (x 3) "")
	(println "<br><B>Author:</b> " (author-name (x 1)) "")
	(println "<p><b>Post:</b> " (format-for-web (x 4)) "")
	(println "<hr>")
)

(println "<h3>Post something...</h3>")
(println "<form name='postsomething' METHOD='POST' action='rockets-post.lsp'>")
(println "<input type='text' name='subjectline'>")
(println "<p><textarea name='post' id='replybox' cols='50' rows='10'></textarea>")
(println "<input type='submit' value='Post Message'>")

(println "</form>")

; table structure:  ((0 "Id" "INTEGER" 0 nil 1) (1 "PosterId" "TEXT" 0 nil 0) (2 "PostDate" "DATE" 0 nil 0) (3 "PostSubject" "TEXT" 0 nil 0) (4 "PostContent" "TEXT" 0 nil 0))

(close-database)
(print-footer)
(exit)
