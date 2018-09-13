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

(displayln "<h2>Admin Page</h2>")

(if (= Rockets:UserId 0) (begin ; admin-only section
        (displayln "<form name='admin' method='POST'>")
        (displayln "<h3>Site configuration</h3>")
        (displayln "<p>Site name: <input type='text' name='shortname' value='" RocketsConfig:ShortName "'></p>")
        (displayln "<h3>Top menu navigation</h3>")
        ; display all navigation

        (dolist (n RocketsNavigation:navbar-list)
            (displayln "<p>Menu item: " $idx ": <input type='text' name='menuname" $idx "' value='" (n 0) "'>")
            (displayln "Page destination: <input type='text' name='menuvalue" $idx "' value='" (n 1) "'>")
        )
        (displayln "<p><input type='submit' value='Save changes'>")
        (displayln "</form>")        
    
        ; if we've made changes, save them.
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
            (save "Rockets-config.lisp" 'RocketsConfig)
            (save "Rockets-navigation.lisp" 'RocketsNavigation)
            (page-redirect "rockets-admin.lsp?updated=true")
        ))
        ; if we made changes and updated page, show success
        (if ($GET "updated") (display-success "Settings updated."))
    )
	(displayln "<p>Sorry, you must be signed in to an admin account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
