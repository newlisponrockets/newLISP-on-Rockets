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
(set 'check-name ($GET "checkname"))
(replace "</td>" check-name "")
(replace "<td>" check-name "")
(replace "partials/" check-name "") ; we'll have to deal with partials later
(set 'file-binary ($POST "binary-data"))

(if (= file-name check-name) (begin
	(displayln "<p>File names match.")
	(write-file (string "uploads/" file-name) file-binary)
	; chown the target file so we can copy on this account
	(exec (string "chown www-data:www-write /var/www/" file-name))
	; now copy file into root directory (right now /var/www, will be configurable
	(exec (string "cp /var/www/uploads/" file-name " /var/www/" file-name))
	; now chmod it 755
	(exec (string "chmod 755 /var/www/" file-name))
	)
	(displayln "<p>File names do not match!")
)



; we have to do something here to update the file, run git commit, etc.
(displayln "<p>File name: " file-name)
(displayln "<p>check name: " check-name)
(displayln "<P>Uploading file... complete.  File is at <a href=uploads/" file-name ">" file-name "</a>")
(displayln "<p>Go back to <a href=rockets-admin.lsp>Admin page</a>")
(page-redirect "rockets-admin") ; go back to the admin page, we're done here
))
(display-page)