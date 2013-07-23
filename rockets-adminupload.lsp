#!/usr/bin/env newlisp

(load "/var/www/newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets Blog - Avatar upload page (rockets-adminupload.lsp)
; 
; This saves an uploaded file to be saved to the system. 
; Version 0.01 (Rockets version shown on page)

(load "Rockets-config.lisp") ; load configuration information
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(if Rockets:UserId (begin

(set 'file-name ($POST "filename"))
(set 'file-binary ($POST "binary-data"))
(write-file (string "uploads/" file-name) file-binary)

; we have to do something here to update the file, run git commit, etc.

(displayln "<P>Uploading file... complete.  File is at <a href=uploads/" file-name ">" file-name "</a>")
;(page-redirect "rockets-admin") ; go back to the profile page, we're done here
))
(display-page)