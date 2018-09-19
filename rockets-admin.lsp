#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-profile.lsp) - Rockets - Site admin / upload page 
; 
; This page allow the site owner to change the configuration of 
; their newLISP on Rockets system.
;
; Written 2018 by Jeremy Reimer

(load "Rockets-config.lisp") ; load configuration information

(display-header (string RocketsConfig:Name " - Admin Page"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-profile")
(display-partial "rockets-navbar")

(define (update-page)
    (save "Rockets-config.lisp" 'RocketsConfig)
    (save "Rockets-navigation.lisp" 'RocketsNavigation)
    (page-redirect "rockets-admin.lsp?updated=true")
)

(displayln "<h2>Admin Page</h2>")

(if (= Rockets:UserId 0) (begin ; admin-only section
        ; if we made changes and updated the page, show success
        (if ($GET "updated") (display-success "Settings updated."))
        ; display the form to make changes
        (displayln "<form name='admin' method='POST'>")
        (displayln "<h3>Site configuration</h3>")
        (displayln "<p>Site name: <input type='text' name='shortname' value='" RocketsConfig:ShortName "'></p>")
        (displayln "<h3>Top menu navigation</h3>")
        ; display all navigation

        (dolist (n RocketsNavigation:navbar-list)            
            (displayln "<p>Menu item: " $idx ": <input type='text' name='menuname" $idx "' value='" (n 0) "'>")
            (displayln "Page destination: <input type='text' name='menuvalue" $idx "' value='" (n 1) "'>")
            (if (> $idx 0) (display-button-red "Delete" (string "rockets-admin.lsp?del=" $idx)))
        )
        (displayln "</p><p>")
        (display-button-green "Add menu item" (string "rockets-admin.lsp?add=true"))
        (displayln "</p><hr>")
        (displayln "<h3>Main Page configuration</h3>")
        ; this sets the default front page type if none was configured before
        (if (nil? RocketsConfig:FrontPageType) (setq RocketsConfig:FrontPageType 1))
        (setq page-choices '("Single page with custom content" "Single page with blog posts" "Two columns with custom left hand navbar content" "Three columns with custom left and right hand navbar content" ))
        (displayln "<select name='mainpage' style='width: auto'>")
        (dolist (c page-choices)
            (display "<option value='" $idx "'")
            (if (= $idx RocketsConfig:FrontPageType) (display " selected"))
            (displayln ">" c "</option>")
        )
        (displayln "</select>")        

        (displayln "<hr><p><input type='submit' value='Save changes'></p>")
        (displayln "</form>")    

        ; add form for uploading a main image for the blog
        (displayln "<p>Blog header image:")
        (if RocketsConfig:HeaderImage 
            (displayln "<img src='images/" RocketsConfig:HeaderImage "' width=300 height=200>") 
            (displayln "Default image"))
        (displayln "<form name='FileUpload' action='fileupload.lsp?updateheader=yes' method='POST' enctype='multipart/form-data'><input type='file' id='uploadName' name='uploaded_data' onChange='this.form.textname.value = this.value'><input type='hidden' name='textname'><input type='hidden' name='updateheaderimage' value='yes'><input type='submit' value='Upload' name='submit'></form>")    
    
        ; if we've made changes to any items, save them.
        (if ($POST) (begin
             ; check to see if name has changed
            (setq get-name ($POST "shortname"))
            (if get-name (begin
                (setq RocketsConfig:ShortName get-name)
            ))
            ; check to see if links have changed
            (dolist (m RocketsNavigation:navbar-list)
                (setq item ($POST (string "menuname" $idx)))
                (setq value ($POST (string "menuvalue" $idx)))
                (setq (RocketsNavigation:navbar-list $idx 0) item)
                (setq (RocketsNavigation:navbar-list $idx 1) value)
            )            
            (if ($POST "mainpage") (setq RocketsConfig:FrontPageType (int ($POST "mainpage"))))
            (update-page)
        ))
        ; if we've added or deleted items, adjust list and save them
        (if ($GET "add") (begin 
            (extend RocketsNavigation:navbar-list '(("New item" "filename-of-page")))
            (update-page)

        ))
        (if ($GET "del") (begin 
            (pop RocketsNavigation:navbar-list (int ($GET "del")))
            (update-page)
        ))

    )
	(displayln "<p>Sorry, you must be signed in to an admin account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
