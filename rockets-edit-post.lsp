#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Edit Posting Page rockets-edit-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(load "Rockets-config.lisp") ; load configuration information
(open-database RocketsConfig:Database)

(displayln "<P>$POST data: " ($POST))

; update post 
(display-partial "rockets-checksignin") ; see if the user is signed in
(if Rockets:IsUserAdmin (set 'continue true) (set 'continue nil)) ; Only admin user can edit posts, since only admin can make them
(if continue (begin
    (if (and (= ($POST "optionalhidden") "comment") ($POST "post")) ; editing a COMMENT
        (begin 
            (set 'Id (int ($POST "linkbackid")))
            (set 'CommentSubject "")
            (set 'CommentContent ($POST "post"))
            (displayln "Editing comment: " Id " " CommenterId " " CommentDate) 
            (update-record "Comments" Id CommentContent) ; we don't update the date or author or any other field
            ; now we have to change the Id because we need to redirect to the original post Id, not the Comment Id
            (set 'temp-record (get-record "Comments" Id))
            (display temp-record)
            (displayln "PostID: " ((first temp-record) 1))
            (set 'Id ((first temp-record) 1))
        )
        (begin 
            (if (and ($POST "post") ($POST "subjectline")) ; editing a POST
            	(begin
            		(set 'Id (int ($POST "linkbackid")))
            		(set 'PosterId Rockets:UserId) ; ONLY I MAY POST FOR NOW
            		(set 'PostSubject ($POST "subjectline"))
            		(set 'PostContent ($POST "post"))
            		(set 'PostDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
            		(update-record "Posts" Id PosterId PostDate PostSubject PostContent) ; It's the U in CRUD!
            		;(displayln "<B>Posting disabled for the moment...</b>")
            	)
            )
        )
    )
    ; update RSS feed
    (display-partial "rockets-generate-rss")
))
(displayln "Id: " Id)
;(displayln "<a href='rockets-item.lsp?p=" Id "'>Click here to return to the main page.</a>")
(page-redirect "rockets-item" (string "p=" Id))
(display-page)
