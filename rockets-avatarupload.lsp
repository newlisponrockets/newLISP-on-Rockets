#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Avatar upload page (rockets-avatarupload.lsp)
; 
; This saves an uploaded avatar 
; Version 0.01 (Rockets version shown on page)

(load "Rockets-config.lisp") ; load configuration information
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(if Rockets:UserId (begin

(set 'file-name ($POST "filename"))
(set 'file-binary ($POST "binary-data"))
(write-file (string "images/avatars/" file-name) file-binary)
(set 'UserAvatar file-name)
(set 'UserId Rockets:UserId)
(update-record "Users" UserId UserAvatar) ; update the user's avatar

(displayln "<P>Uploading file... complete.  File is at <a href=images/avatars/" file-name ">" file-name "</a>")
(page-redirect "rockets-profile") ; go back to the profile page, we're done here
))
(display-page)
