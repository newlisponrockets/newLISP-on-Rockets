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
        ("podcast" (page-redirect "rockets-admin.lsp?tab=podcast&updated=true"))
    )
)

(define (checked str-panel str-item)
    (setq checkedvalue "")
    (if RocketsConfig:LeftPanel
       (if (= str-panel "left") (if (find str-item RocketsConfig:LeftPanel) (setq checkedvalue " checked='checked'"))))
    (if RocketsConfig:RightPanel
       (if (= str-panel "right") (if (find str-item RocketsConfig:RightPanel) (setq checkedvalue " checked='checked'"))))
    (setq return-value checkedvalue)
)

(displayln "<h2>Admin Page</h2>")

(if Rockets:IsUserAdmin (begin ; admin-only section
        (if (nil? ($GET "tab")) (display-button-blue "General Configuration" "rockets-admin.lsp") (display-button "General Configuration" "rockets-admin.lsp"))
        (if (= ($GET "tab") "custom") (display-button-blue "Custom Configuration" "rockets-admin.lsp?tab=custom") (display-button "Custom Configuration" "rockets-admin.lsp?tab=custom"))
        (if (= ($GET "tab") "media") (display-button-blue "Media Configuration" "rockets-admin.lsp?tab=media") (display-button "Media Configuration" "rockets-admin.lsp?tab=media"))
        (if (= ($GET "tab") "users") (display-button-blue "User Configuration" "rockets-admin.lsp?tab=users") (display-button "User Configuration" "rockets-admin.lsp?tab=users"))
        (if (= ($GET "tab") "podcast") (display-button-blue "Podcast Configuration" "rockets-admin.lsp?tab=podcast") (display-button "Podcast Configuration" "rockets-admin.lsp?tab=podcast"))        
        (displayln "<p></p>")
        ; GENERAL CONFIGURATION ------------------------------------------------------------------------
        (if (nil? ($GET "tab")) (begin 

            ; add form for uploading a main image for the blog
            (displayln "<p>Blog header image:")
            (if RocketsConfig:HeaderImage 
                (displayln "<img src='images/" RocketsConfig:HeaderImage "' width=300 height=200>") 
                (displayln "Default image"))
            (displayln "<form name='FileUpload' action='fileupload.lsp?updateheader=yes' method='POST' enctype='multipart/form-data'><input type='file' id='uploadName' name='uploaded_data' onChange='this.form.textname.value = this.value'><input type='hidden' name='textname'><input type='hidden' name='updateheaderimage' value='yes'><input type='submit' value='Upload' name='submit'></form>")    
 
             ; add form for uploading a discussion image for the blog
            (displayln "<p>Discussion header image:")
            (if RocketsConfig:DiscussionImage 
                (displayln "<img src='images/" RocketsConfig:DiscussionImage "' width=300 height=200>") 
                (displayln "Default image"))
            (displayln "<form name='FileUpload2' action='fileupload.lsp?updatediscussion=yes' method='POST' enctype='multipart/form-data'><input type='file' id='uploadName' name='uploaded_data' onChange='this.form.textname.value = this.value'><input type='hidden' name='textname'><input type='hidden' name='updatediscussionimage' value='yes'><input type='submit' value='Upload' name='submit'></form>")    
 
            ; if we made changes and updated the page, show success
            (if ($GET "updated") (display-success "Settings updated."))
            ; display the form to make changes
            (displayln "<form name='admin' method='POST'>")
            (displayln "<h3>Site configuration</h3>")
            (displayln "<p>Site short name: <input type='text' name='shortname' value='" RocketsConfig:ShortName "'></p>")
            (displayln "<p>Site full name: <input type='text' name='longname' value='" RocketsConfig:Name "'></p>")
            (if (nil? RocketsConfig:ForumSubtitle) (setq RocketsConfig:ForumSubtitle "Discussions on all topics."))
            (displayln "<p>Forum subtitle: <input type='text' name='forumsubtitle' value='" RocketsConfig:ForumSubtitle "'></p>")
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

            (displayln "<h3>Individual Page configuration</h3>")    
            ; this sets the default front page type if none was configured before
            (if (nil? RocketsConfig:IndividualPageType) (setq RocketsConfig:IndividualPageType 1)) 
            (setq page-choices '("Individual post by itself " "Two columns with custom left hand navbar content" "Three columns with custom left and right hand navbar content" ))
            (displayln "<select name='individualpage' style='width: auto'>")
            (dolist (c page-choices)
                (display "<option value='" $idx "'")
                (if (= $idx RocketsConfig:IndividualPageType) (display " selected"))
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
                (setq get-forumsubtitle ($POST "forumsubtitle"))
                (if get-forumsubtitle (begin 
                    (setq RocketsConfig:ForumSubtitle get-forumsubtitle)
                ))
                ; check to see if links have changed
                (dolist (m RocketsNavigation:navbar-list)
                    (setq item ($POST (string "menuname" $idx)))
                    (setq value ($POST (string "menuvalue" $idx)))
                    (setq (RocketsNavigation:navbar-list $idx 0) item)
                    (setq (RocketsNavigation:navbar-list $idx 1) value)
                )            
                ; check if main page layout has changed
                (if ($POST "mainpage") (setq RocketsConfig:FrontPageType (int ($POST "mainpage"))))
                ; check if individual page layout has changed
                (if ($POST "individualpage") (setq RocketsConfig:IndividualPageType (int ($POST "individualpage"))))
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
            (if ($GET "updated") (display-success "Settings updated."))
            (displayln "<form name='admin' method='POST'>")
            (displayln "<h3>Custom HTML box 1</h3>")
            (displayln "<textarea name='post1' id='html1' class='field span9' rows='10'>")
            (if (read-file "partials/panel1.html") (displayln (read-file "partials/panel1.html")))
            (displayln "</textarea>")
            (displayln "<h3>Custom HTML box 2</h3>")
            (displayln "<textarea name='post2' id='html2' class='field span9' rows='10'>")
            (if (read-file "partials/panel2.html") (displayln (read-file "partials/panel2.html")))
            (displayln "</textarea>")
            (displayln "<h3>Custom HTML box 3</h3>")
            (displayln "<textarea name='post3' id='html3' class='field span9' rows='10'>")
            (if (read-file "partials/panel3.html") (displayln (read-file "partials/panel3.html")))
            (displayln "</textarea>")
            (displayln "<h3>Custom HTML box 4</h3>")
            (displayln "<textarea name='post4' id='html4' class='field span9' rows='10'>")
            (if (read-file "partials/panel4.html") (displayln (read-file "partials/panel4.html")))
            (displayln "</textarea>")
            (displayln "<hr><p><input type='submit' value='Save changes'></p>")
            (displayln "</form>")
            ; note that we aren't doing any sanity checks on the HTML, because this page is only accessible to Admins
            ; if the admin wants to do a script injection attack on their own site, that's up to them!
    
            (if ($POST) (begin
                (if ($POST "post1") (begin 
                    (write-file "partials/panel1.html" ($POST "post1"))
                ))
                (if ($POST "post2") (begin 
                    (write-file "partials/panel2.html" ($POST "post2"))
                    (displayln "POST2!!!!!!!!!!!!!!!!")
                ))
                (if ($POST "post3") (begin 
                    (write-file "partials/panel3.html" ($POST "post3"))
                ))
                (if ($POST "post4") (begin 
                    (write-file "partials/panel4.html" ($POST "post4"))
                ))
                (update-page)
            ))
        )) ; end Custom Configuration section

        ; MEDIA CONFIGURATION ------------------------------------------------------------------------
        (if (= ($GET "tab") "media") (begin 
        (displayln "<h3>Media configuration</h3>")
            (if ($GET "image") 
                (begin (displayln "<img src='images/" ($GET "image") "'>")
                    (displayln "<p></p><br><br>") 
                    (if ($GET "delete-confirm") (begin 
                        (if (= ($GET "delete-confirm") "yes") (begin
                                (displayln "deleting file...")
                                (delete-file (string "images/" ($GET "image")))
                                (page-redirect "rockets-admin.lsp?tab=media") 
                            ) 
                            (begin
                                (display-button "Cancel deletion" (string "rockets-admin.lsp?tab=media&image=" ($GET "image")))
                                (display-button-red "Confirm deletion" (string "rockets-admin.lsp?tab=media&delete-confirm=yes&image=" ($GET "image")))
                            ))
                    )
                    (begin 
                        (display-button-red "Delete image" (string "rockets-admin.lsp?tab=media&delete-confirm=confirm&image=" ($GET "image")))
                        (displayln "<br><br>")
                        (display-button "Return to image directory" "rockets-admin.lsp?tab=media")
                    ))
                )
                (begin
                    (setq image-files (directory "images"))
                    (start-div "span12")
                    (dolist (file image-files)
                        (if (or (ends-with file ".jpg") (ends-with file ".png") (ends-with file ".gif") (ends-with file ".jpeg")) (begin 
                            (start-div "span2")
                            (displayln "<p><a href=rockets-admin.lsp?tab=media&image=" file "><img src='images/" file "' width=200></a></p>")
                            (end-div)
                        ))
                    )
                    (end-div)
                    (displayln "<p><br><br>")
                    (displayln "<h3>Add new image</h3>")
                    (displayln "<form name='FileUpload' action='fileupload.lsp?media='yes' method='POST' enctype='multipart/form-data'><input type='file' id='uploadName' name='uploaded_data' onChange='this.form.textname.value = this.value'><input type='hidden' name='textname'><input type='submit' value='Upload' name='submit'></form>")                    
            ))
            

        )) ; end Media Configuration section

        ; USERS CONFIGURATION ------------------------------------------------------------------------
        (if (= ($GET "tab") "users") (begin 
            (displayln "<h3>User configuration</h3>")
            (if ($GET "delete") (begin 
                (if (= ($GET "delete") "confirm") (begin 
                    (set 'UserId (force-parameters 1 ($GET "user")))
                    (displayln "<h3>Deleting user #" UserId "</h3>")
                    (set 'user-info (get-record "Users" UserId))  
                    (displayln "<p>User name: " (user-info 0 7))
                    (displayln "<p>User email: " (user-info 0 1)) 
                    (displayln "<p>User postcount: " (user-info 0 4))
                    (displayln "<p>User joinedate: " (user-info 0 11))
                    (displayln "<p>User avatar: " (user-info 0 9))
                    (displayln "<p>User achievements: " (user-info 0 5))
                    (displayln "</p><br>")
                    (display-button "Cancel deletion" (string "rockets-admin.lsp?tab=users"))
                    (display-button-red "Confirm deletion" (string "rockets-admin.lsp?tab=users&delete=yes&user=" ($GET "user")))  
                )
                    (begin
                        (set 'UserId (force-parameters 1 ($GET "user")))
                        (displayln "Actually deleting user " UserId)
                        (displayln "Here's the result:")
                        (displayln (delete-record "Users" UserId))
                        (page-redirect "rockets-admin.lsp?tab=users")
                    )
                )
            )
            (begin
                (set 'user-list (query "SELECT * FROM Users"))

                (dolist (x user-list)
                        (displayln "<p>User #: " (x 0))  
                        (if (not (= (x 0) 0)) (displayln "&nbsp<a href='rockets-admin.lsp?tab=users&delete=confirm&user=" (x 0) "'>Delete user</a>"))
                        (displayln "<p>User name: " (x 7))
                        (displayln "<p>User email: " (x 1)) 
                        (displayln "<p>User postcount: " (x 4))
                        (displayln "<p>User joinedate: " (x 11))
                        (displayln "<p>User avatar: " (x 9))
                        (displayln "<p>User achievements: " (x 5))
                        (displayln "<hr>"))

                        (displayln "<br><br><br>")
            ))

        )) ; end User Configuration section

        ; PODCAST CONFIGURATION ------------------------------------------------------------------------
        (if (= ($GET "tab") "podcast") (begin 

            (displayln "<h3>Podcast configuration</h3>")
            ; if we made changes and updated the page, show success
            (if ($GET "updated") (display-success "Settings updated."))
            (displayln "<p><i>Note: To create a podcast feed, you must first configure the options here, then add podcast posts to your blog with the same tag and with the 'Podcast' post type.</i></p>")
            (setq default-podcast-settings '("podcast" "Podcast Title" "Podcast Copyright" "Podcast Subtitle" "Podcast Author" "Podcast Summary" "Podcast Owner" "Podcast Email" "Podcast Image" "Podcast Category" "Podcast Subcategory"))
            (if (nil? RocketsConfig:PodcastList) (setq RocketsConfig:PodcastList (list default-podcast-settings))) ; default options
            (displayln "<p><form name='podcastadmin' method='POST'>")
            (dolist (x RocketsConfig:PodcastList)
                (displayln "<hr><p>Podcast #: " $idx "</p>")
                (displayln "<p>Tag: <input type='text' name='podcasttag" $idx "' value='" ((RocketsConfig:PodcastList $idx) 0) "'></p>")
                (displayln "<p>Title: <input type='text' name='podcasttitle" $idx "' value='" ((RocketsConfig:PodcastList $idx) 1) "'>")
                (displayln "<p>Copyright: <input type='text' name='podcastcopyright" $idx "' value='" ((RocketsConfig:PodcastList $idx) 2) "'>")
                (displayln "<p>Subtitle: <input type='text' name='podcastsubtitle" $idx "' value='" ((RocketsConfig:PodcastList $idx) 3) "'>")
                (displayln "<p>Author: <input type='text' name='podcastauthor" $idx "' value='" ((RocketsConfig:PodcastList $idx) 4) "'>")
                (displayln "<p>Summary: <input type='text' name='podcastsummary" $idx "' value='" ((RocketsConfig:PodcastList $idx) 5) "'>")
                (displayln "<p>Owner: <input type='text' name='podcastowner" $idx "' value='" ((RocketsConfig:PodcastList $idx) 6) "'>")
                (displayln "<p>Email: <input type='text' name='podcastemail" $idx "' value='" ((RocketsConfig:PodcastList $idx) 7) "'>")
                (displayln "<p>Image: <input type='text' name='podcastimage" $idx "' value='" ((RocketsConfig:PodcastList $idx) 8) "'>")
                (displayln " <img src='images/" ((RocketsConfig:PodcastList $idx) 8) "'>")
                (displayln "<p>Category: <input type='text' name='podcastcategory" $idx "' value='" ((RocketsConfig:PodcastList $idx) 9) "'>")
                (displayln "<p>Subcategory: <input type='text' name='podcastsubcategory" $idx "' value='" ((RocketsConfig:PodcastList $idx) 10) "'>")                
                (displayln "<p><b>Podcast feed URL:</b> " RocketsConfig:SiteURL "/podcast/" ((RocketsConfig:PodcastList $idx) 0) ".xml</p>")
                (if (> $idx 0) (display-button-red "Delete" (string "rockets-admin.lsp?tab=podcast&del=" $idx)))

            )
            (displayln "<hr><p><input type='submit' value='Save changes'></p>")
            (displayln "</form>")
            (display-button-green "Add new podcast" (string "rockets-admin.lsp?tab=podcast&add=true"))
            ; if we've changed any of the podcast parameters, save them 
            (if ($POST) (begin
                (setq podcast-length (length RocketsConfig:PodcastList))
                (displayln "Podcast length: " podcast-length)  
                (dolist (p RocketsConfig:PodcastList)
                    (if ($POST (string "podcasttag" $idx)) (setf ((RocketsConfig:PodcastList $idx) 0) ($POST (string "podcasttag" $idx))))
                    (if ($POST (string "podcasttitle" $idx)) (setf ((RocketsConfig:PodcastList $idx) 1) ($POST (string "podcasttitle" $idx))))
                    (if ($POST (string "podcastcopyright" $idx)) (setf ((RocketsConfig:PodcastList $idx) 2) ($POST (string "podcastcopyright" $idx))))
                    (if ($POST (string "podcastsubtitle" $idx)) (setf ((RocketsConfig:PodcastList $idx) 3) ($POST (string "podcastsubtitle" $idx))))
                    (if ($POST (string "podcastauthor" $idx)) (setf ((RocketsConfig:PodcastList $idx) 4) ($POST (string "podcastauthor" $idx))))
                    (if ($POST (string "podcastsummary" $idx)) (setf ((RocketsConfig:PodcastList $idx) 5) ($POST (string "podcastsummary" $idx))))
                    (if ($POST (string "podcastowner" $idx)) (setf ((RocketsConfig:PodcastList $idx) 6) ($POST (string "podcastowner" $idx))))
                    (if ($POST (string "podcastemail" $idx)) (setf ((RocketsConfig:PodcastList $idx) 7) ($POST (string "podcastemail" $idx))))
                    (if ($POST (string "podcastimage" $idx)) (setf ((RocketsConfig:PodcastList $idx) 8) ($POST (string "podcastimage" $idx))))
                    (if ($POST (string "podcastcategory" $idx)) (setf ((RocketsConfig:PodcastList $idx) 9) ($POST (string "podcastcategory" $idx))))
                    (if ($POST (string "podcastsubcategory" $idx)) (setf ((RocketsConfig:PodcastList $idx) 10) ($POST (string "podcastsubcategory" $idx))))
                )           
                (update-page)
            ))
            ; if we've added or deleted a podcast, extend or trim the list
            (if ($GET "add") (begin 
                (extend RocketsConfig:PodcastList (list default-podcast-settings))
                (update-page)
            ))
            (if ($GET "del") (begin 
                (pop RocketsConfig:PodcastList (int ($GET "del")))
                (update-page)                
            ))

        ))

    )
	(displayln "<p>Sorry, you must be signed in to an admin account to access this page.</p><p><a href='rockets-main.lsp'>Return to main page.</a></p>")
)
	



(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!
