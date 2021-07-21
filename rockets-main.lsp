#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets 2.0 - Main Page
; 
; This is the second  version of the self-hosted blog for newLISP on Rockets.
; The blog is minimalist but functional, and can be customized in the Admin control panel
; 
; Copyright 2012-2018 by Jeremy Reimer (aka Rocket Man)

(load "Rockets-config.lisp") ; load configuration information
(display-header RocketsConfig:Name)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-main")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

; get current page from URL (if there is one)
(set 'current-page (force-parameters 1 ($GET "p"))) ; we only need the first part, ignore anything else
(if current-page (set 'current-page (int current-page)) (set 'current-page 1))

(define (display-hero-unit)
    (start-div "hero-unit")
        (if RocketsConfig:HeaderImage
            (display-image RocketsConfig:HeaderImage)
            (display-image "newlisp-rockets-picture.jpg" 960 540)
        )        
        (displayln "<h2>" RocketsConfig:Name "</h2>")
    (end-div)
    ; add an RSS feed link
    (displayln "<p><a href=blog-feed.xml><img src=images/rss.png>&nbsp;RSS Feed for this blog</a></p>")
)

(define (display-custom-content)
    (display-partial "rockets-custom")
)

(define (display-blog-posts)
    ; admin anchor link for posting
    (if Rockets:IsUserAdmin (displayln "<p><a href='#postbox'>Jump to post box</a></p>"))
    ; get optional limiting tag
    (set 'tag-name (force-parameters 1 ($GET "tags")))
    (if tag-name (displayln "<h3> Showing topics tagged as: " tag-name "</h3>"))
    ; get all existing posts
    (if tag-name 
        (set 'total-posts (int (first (first (query (string "SELECT Count(*) FROM Posts WHERE PostType='Blog post' AND PostTags LIKE '%" tag-name "%'"))))))
        (set 'total-posts (int (first (first (query (string "SELECT Count(*) FROM Posts WHERE PostType='Blog post' OR PostType='Podcast'"))))))
    )
    (set 'total-pages (/ total-posts Blog:posts-per-page))
    (if (>= (mod (float total-posts) (float Blog:posts-per-page)) 1) (inc total-pages)) ; fix number of pages if not evenly divisible

    (display-paging-links 1 total-pages current-page active-page)

    (set 'start-post-num (- (* current-page Blog:posts-per-page) Blog:posts-per-page))
    (if tag-name
        (set 'posts-query-sql (string "SELECT * from Posts WHERE PostType='Blog post' AND PostTags LIKE '%" tag-name "%' ORDER BY Id DESC LIMIT " start-post-num "," Blog:posts-per-page ";"))
        (set 'posts-query-sql (string "SELECT * from Posts WHERE PostType='Blog post' OR PostType='Podcast' ORDER BY Id DESC LIMIT " start-post-num "," Blog:posts-per-page ";"))
    )
    (set 'posts-result (query posts-query-sql))
    ; print out all posts
    (dolist (x posts-result)
    	(display-individual-post x)
    )

    (display-paging-links 1 total-pages current-page active-page) ; display them again

    ; print post entry box
    (if Rockets:IsUserAdmin (begin
        (displayln "<a id='postbox'></a>") ; anchor link for post box
    	(display-post-box "Post something..." "postsomething" "rockets-post.lsp" "subjectline" "replybox" "Post Message" nil nil nil nil true "tags" '("Blog post" "Page" "Podcast" "Comic" "Forum notice"))
    )) ; only an administrator may make new blog posts 
)

(case RocketsConfig:FrontPageType
     (0 (display-custom-content))
     (1 (display-hero-unit) (display-blog-posts))
     (2 (display-hero-unit) (start-div "row-fluid") (start-div "span4") (display-partial "rockets-leftpanel") (end-div) 
        (start-div "span8") (display-blog-posts) (end-div) (end-div) )
     (3 (display-hero-unit) (start-div "row-fluid") (start-div "span3") (display-partial "rockets-leftpanel") (end-div)
        (start-div "span6") (display-blog-posts) (end-div)
        (start-div "span3") (display-partial "rockets-rightpanel") (end-div) (end-div))
)

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!

