; partial file rockets-sidepanels.lsp
; Actually display the panels configured in either RocketsConfig:LeftPanel or RocketsConfig:RightPanel
; this variable is called "panel-type" and is set in either rockets-leftpanel.lsp or rockets-rightpanel.lsp partials that call this one

(if panel-type (begin 

    (if (find "box1" panel-type) (begin ; Custom HTML Box 1
        (display-partial "rockets-panel1")
        (displayln "<br>")
    ))

    (if (find "popposts" panel-type) (begin ; Popular posts
        (display "<h3>Popular blog posts</h3>")
        (set 'popular-posts (query "SELECT ID,PostSubject from Posts WHERE PostType='Blog post' ORDER BY PostViews DESC LIMIT 5"))        
        (dolist (p popular-posts)
                (displayln "<li><a href=rockets-item.lsp?p=" (p 0) ">" (p 1) "</a></li>"))
    ))

    (if (find "recentposts" panel-type) (begin ; Recent posts

        (display "<h3>Recent forum posts</h3>")
        (set 'new-posts (query "SELECT ID,PostSubject from Posts ORDER BY PostDate DESC LIMIT 5"))
        (dolist (n new-posts)
                (displayln "<li><a href=rockets-item.lsp?p=" (n 0) ">" (n 1) "</a></li>"))

    ))    

    (if (find "forumlink" panel-type) (begin ; Discussion forum
        (display "<h3>Discussion Forum</h3>")
        (displayln "<a href=rockets-forum.lsp>Discussion forum</a>")
    ))   

    (if (find "box2" panel-type) (begin ; Custom HTML box 2
        (display-partial "rockets-panel2")
        (displayln "<br>")
    ))   

    (if (find "box3" panel-type) (begin ; Custom HTML box 3
        (display-partial "rockets-panel3")
        (displayln "<br>")
    ))   

    (if (find "blogtopics" panel-type) (begin ; Popular posts

        (display "<h3>Topics</h3>")
        (set 'blog-keywords (flat (query "SELECT DISTINCT PostTags from Posts;")))
        ;(displayln blog-keywords)
        ;(displayln (nil? (first (flat blog-keywords))))
        (if (not (nil? (last (flat blog-keywords)))) (begin
            (dolist (k blog-keywords)
                (if (not (nil? k)) (begin
                    ;(displayln "***>>>" k)
                    (set 'comma-delim (parse k ","))
                    (dolist (cd comma-delim)
                        (push (title-case (trim cd)) blog-keywords-parsed -1))
                ))
            )
            (set 'blog-keywords-parsed (sort (unique blog-keywords-parsed)))
            (dolist (b blog-keywords-parsed) (displayln "<br><a href='rockets-main.lsp?tags=" b "'>" b "</a>"))
            (displayln "<p></p>")
        )
            (displayln "<p>No post topics defined. Add some!</p>")
        )

    ))               

    (if (find "box4" panel-type) (begin ; Custom HTML Box 4
        (display-partial "rockets-panel4")
        (displayln "<br>")
    ))                   
)
(begin (displayln "<h2>Side navigation panel</h2>") (displayln "<p>(Please configure this in the Admin control panel)</p>"))
)