#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-profile.lsp) - Rockets - Site admin / upload page 
; 
; This page allow the site owner to upload files directly, manipulate 
; the source control system, change theme, etc. 
;
; Written 2013 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information

(display-header (string RocketsConfig:Name " - Admin Page"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-profile")
(display-partial "rockets-navbar")

(displayln "<h2>Admin Page</h2>")

(if (= Rockets:UserId 0) (begin ; admin-only section
	(displayln "<h3>List of files in repository</h3>")
	(set 'git-data (exec "git ls-tree --full-tree -r HEAD"))
	(dolist (g git-data)
		(push (last (parse g " ")) git-data-parsed -1))
	(dolist (g git-data-parsed)
		(push (last (parse g "\t")) git-names -1))
	(dolist (g git-names)
		(push (list g "<form name='FileUpload' action='rockets-adminupload.lsp' method='POST' enctype='multipart/form-data'><input type='file' id='uploadName' name='uploaded_data' onChange='this.form.textname.value = this.value'><input type='hidden' name='textname'><input type='submit' value='Upload' name='submit'></form>") git-table -1)
	)	
	(display-table '("File name" "Upload new file") git-table "hover")
	)
	(displayln "<p>Sorry, you must be signed in to an administer account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!