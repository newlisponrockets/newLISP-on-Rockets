#!/usr/bin/env newlisp

; Rockets Blog - Image/audio upload page (fileupload.lsp)
;
; This uploads an image to the server -- should be accessible to admins ONLY!


(load "newlisp-rockets.lisp") ; this is where the magic happens!
(load "Rockets-config.lisp") ; load configuration information
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in

(if Rockets:IsUserAdmin (begin ; admin-only section

    (set 'file-name ($POST "filename"))
    (set 'file-binary ($POST "binary-data"))

    (write-file (string "images/" file-name) file-binary)

    (displayln "<P>Uploading file... complete.  File is at <a href=images/" file-name ">" file-name "</a>")

    ; if we've updated the header image, change the image in the config file
    (if ($GET "updateheader") (begin
        (displayln "<p>Updating header image with: " file-name)
        (setq RocketsConfig:HeaderImage file-name)
        (save "Rockets-config.lisp" 'RocketsConfig)
    ))

    (displayln "<p>==================</p>")
    ; if we've updated a blog post with an image, update the body with an <img> link to that new image
    (if ($GET "updateblogpost") (begin 
        (set 'Id (int ($GET "updateblogpost")))
        (display "Updating blog post..." Id)
        (set 'post-record (first (get-record "Posts" Id)))
        (display post-record)
        (set 'PostContent (post-record 4))
        (set 'text-to-replace (string "[image]"))
        (set 'replace-text (string "[img]images/" file-name "[/img]"))
        (replace text-to-replace PostContent replace-text)
        # audio section for podcasts
        (set 'text-to-replace (string "[audio]"))
        (set 'replace-text (string "[mp3]images/" file-name "[/mp3]"))
        (replace text-to-replace PostContent replace-text)
        (displayln "New text: " PostContent)
        (update-record "Posts" Id PostContent)
    ))
)) ; end admin-only section

(if ($GET "updateblogpost")
    (page-redirect (string "rockets-item.lsp?p=" ($GET "updateblogpost")))
)

(if ($GET "media")
    (page-redirect "rockets-admin.lsp?tab=media")
    (page-redirect "rockets-admin")
)
 
 

(display-page)