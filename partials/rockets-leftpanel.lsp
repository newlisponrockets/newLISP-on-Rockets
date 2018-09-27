; partial file rockets-leftpanel.lsp
;
; displays the optional left hand column for the main page

(if RocketsConfig:LeftPanel (begin 
    (if (find "box1" RocketsConfig:LeftPanel) (begin ; Custom HTML Box 1

        (display "<p>Custom HTML Box 1</p>")
    ))
    (if (find "popposts" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<h2>Popular blog posts</h2>")
    ))
    (if (find "recentposts" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<h2>Recent forum posts</h2>")
    ))    
    (if (find "forumlink" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<h2>Discussion Forum</h2>")
        (displayln "<a href=rockets-forum>Discussion forum</a>")
    ))   
    (if (find "box2" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<p>Custom HTML Box 2</p>")
    ))     
    (if (find "box3" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<p>Custom HTML Box 3</p>")
    ))   
    (if (find "blogtopics" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<h2>Topics</h2>")
    ))               
    (if (find "box4" RocketsConfig:LeftPanel) (begin ; Popular posts

        (display "<p>Custom HTML Box 4</p>")
    ))                   
)
(begin (displayln "<h1>Left-hand navigation panel</h1>") (displayln "<p>(Please configure this in the Admin control panel)</p>"))
)