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
    ;(displayln "POST: " ($POST))
    (case ($GET "tab")
        (nil (page-redirect "rockets-admin.lsp?updated=true"))
        ("custom" (page-redirect "rockets-admin.lsp?tab=custom&updated=true"))
        ("users" (page-redirect "rockets-admin.lsp?tab=users&updated=true"))
        ("media" (page-redirect "rockets-admin.lsp?tab=media&updated=true"))
    )
)

(define (checked str-panel str-item)
    (setq checkedvalue "")
    (if (= str-panel "left") (if (find str-item RocketsConfig:LeftPanel) (setq checkedvalue " checked='checked'")))
    (if (= str-panel "right") (if (find str-item RocketsConfig:RightPanel) (setq checkedvalue " checked='checked'")))
    (setq return-value checkedvalue)
)

(displayln "<h2>Admin Page</h2>")

(if Rockets:IsUserAdmin (begin ; admin-only section
        (if (nil? ($GET "tab")) (display-button-blue "General Configuration" "rockets-admin.lsp") (display-button "General Configuration" "rockets-admin.lsp"))
        (if (= ($GET "tab") "custom") (display-button-blue "Custom Configuration" "rockets-admin.lsp?tab=custom") (display-button "Custom Configuration" "rockets-admin.lsp?tab=custom"))
        (if (= ($GET "tab") "media") (display-button-blue "Media Configuration" "rockets-admin.lsp?tab=media") (display-button "Media Configuration" "rockets-admin.lsp?tab=media"))
        (if (= ($GET "tab") "users") (display-button-blue "User Configuration" "rockets-admin.lsp?tab=users") (display-button "User Configuration" "rockets-admin.lsp?tab=users"))
        (displayln "<p></p>")
        ; GENERAL CONFIGURATION ------------------------------------------------------------------------
        (if (nil? ($GET "tab")) (begin 
            ; if we made changes and updated the page, show success
            (if ($GET "updated") (display-success "Settings updated."))
            ; display the form to make changes
            (displayln "<form name='admin' method='POST'>")
            (displayln "<h3>Site configuration</h3>")
            (displayln "<p>Site short name: <input type='text' name='shortname' value='" RocketsConfig:ShortName "'></p>")
            (displayln "<p>Site full name: <input type='text' name='longname' value='" RocketsConfig:Name "'></p>")
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

            ; Left hand navigation options (only if you've enabled left-hand panel display)
            (if (or (= RocketsConfig:FrontPageType 2) (= RocketsConfig:FrontPageType 3)) (begin 
                (displayln "<h3>Left-hand panel configuration</h3>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='box1'" (checked "left" "box1") ">Custom HTML display box 1<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='popposts'" (checked "left" "popposts") ">Most popular blog posts<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='recentposts'" (checked "left" "recentposts") ">Recent forum posts<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='forumlink'" (checked "left" "forumlink") ">Forum link<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='box2'" (checked "left" "box2") ">Custom HTML display box 2<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='box3'" (checked "left" "box3") ">Custom HTML display box 3<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='blogtopics'" (checked "left" "blogtopics") ">Blog topics<br>")
                (displayln "<input type='checkbox' name='leftpanel[]' value='box4'" (checked "left" "box4") ">Custom HTML display box 4<br>")
            ))
            (if (= RocketsConfig:FrontPageType 3) (begin 
                (displayln "<h3>Right-hand panel configuration</h3>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='box1'" (checked "right" "box1") ">Custom HTML display box 1<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='popposts'" (checked "right" "popposts") ">Most popular blog posts<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='recentposts'" (checked "right" "recentposts") ">Recent forum posts<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='forumlink'" (checked "right" "forumlink") ">Forum link<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='box2'" (checked "right" "box2") ">Custom HTML display box 2<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='box3'" (checked "right" "box3") ">Custom HTML display box 3<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='blogtopics'" (checked "right" "blogtopics") ">Blog topics<br>")
                (displayln "<input type='checkbox' name='rightpanel[]' value='box4'" (checked "right" "box4") ">Custom HTML display box 4<br>")
            ))


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
                (setq get-long-name ($POST "longname"))
                (if get-long-name (begin 
                    (setq RocketsConfig:Name get-long-name)))
                ; check to see if links have changed
                (dolist (m RocketsNavigation:navbar-list)
                    (setq item ($POST (string "menuname" $idx)))
                    (setq value ($POST (string "menuvalue" $idx)))
                    (setq (RocketsNavigation:navbar-list $idx 0) item)
                    (setq (RocketsNavigation:navbar-list $idx 1) value)
                )            
                ; check if main page layout has changed
                (if ($POST "mainpage") (setq RocketsConfig:FrontPageType (int ($POST "mainpage"))))
                ; check if left and/or right hand panel configuration has changed            
                (if ($POST "leftpanel%5B%5D") (setq RocketsConfig:LeftPanel ($POST "leftpanel%5B%5D")))
                (if ($POST "rightpanel%5B%5D") (setq RocketsConfig:RightPanel ($POST "rightpanel%5B%5D")))
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
        )) ; end General Configuration section
        ; CUSTOM CONFIGURATION ------------------------------------------------------------------------
        (if (= ($GET "tab") "custom") (begin 
        (displayln "<form name='admin' method='POST'>")
        (displayln "<h3>Custom HTML box 1</h3>")
        (displayln "<textarea name='post1' id='html1' class='field span9' rows='10'>")
        (displayln "</textarea>")
        (displayln "<h3>Custom HTML box 2</h3>")
        (displayln "<textarea name='post2' id='html2' class='field span9' rows='10'>")
        (displayln "</textarea>")
        (displayln "<h3>Custom HTML box 3</h3>")
        (displayln "<textarea name='post3' id='html3' class='field span9' rows='10'>")
        (displayln "</textarea>")
        (displayln "<h3>Custom HTML box 4</h3>")
        (displayln "<textarea name='post4' id='html4' class='field span9' rows='10'>")
        (displayln "</textarea>")
        (displayln "<hr><p><input type='submit' value='Save changes'></p>")

        )) ; end General Configuration section

        ; MEDIA CONFIGURATION ------------------------------------------------------------------------
        (if (= ($GET "tab") "media") (begin 

        )) ; end General Configuration section

        ; USERS CONFIGURATION ------------------------------------------------------------------------
        (if (= ($GET "tab") "users") (begin 

        )) ; end General Configuration section


    )
	(displayln "<p>Sorry, you must be signed in to an admin account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
