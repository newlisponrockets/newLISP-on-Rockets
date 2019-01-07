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
)) ; end admin-only section

(if ($GET "media")
    (page-redirect "rockets-admin.lsp?tab=media")
    (page-redirect "rockets-admin")
)
 
 

(display-page)