#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Delete page (rockets-delete.lsp)
; 
; This just deletes a page
; Version 0.01 (Rockets version shown on page)

(open-database "ROCKETS-BLOG")
(set 'Id (integer ($GET "post")))		; security will come from authorization.. if user isn't logged in and isn't admin
													; no amount of entering this URL will work.
; (delete-record "Posts" Id) ; this is the format we want it to work
;(delete-record "Posts" Id) ; this actually deletes the query.

;(set 'delete-query (string "DELETE FROM Posts WHERE Id=" (safe-for-sql post-to-delete) ";"))
;(println "QUERY: " delete-query)
(displayln "<b>Sorry, post deleting is disabled for the moment until we get user logins working.</b>")
;(query delete-query)

(displayln "<P>Okay.</p>")

; this is temporary, and we also have to make a redirect.  Have to figure out how to do headers and stuff.
(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>")
(display-page)
