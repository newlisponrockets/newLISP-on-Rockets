#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

(load "Rockets-config.lisp") ; load configuration information

; Rockets - Posting Page rockets-post.lsp
; 
; This page takes a post in $POST and posts it to the Posts table, post-haste.  Posts!
; Posts!
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; add functions common to the blog but not the Rockets framework itself

(if Rockets:UserId (begin ; must be a registered user to post anything

	(set 'max-posts (first (first (query "SELECT max(Id) from Posts"))))
	(displayln "<P>Max post id: " max-posts)
	
	(displayln "<P>$POST data: " ($POST))
	
	; if we have a post in $POST, post it to the Posts table.  Tee hee!
	(set 'continue true) ; debugging
	(if continue (begin
	(if (and ($POST "post") ($POST "subjectline"))
		(begin
			(set 'post-type-trigger ($POST "optionalhidden")) ; this is a hidden value to make forum posts, not blog posts
			(set 'Id (+ max-posts 1))
			(set 'PosterId Rockets:UserId) ; Any registered user may post, but only Admin may post blog posts
			(set 'PostType ($POST "posttype"))
			(set 'PostLastAuthor (author-name PosterId true))
			(set 'PostSubject ($POST "subjectline"))
			(set 'PostContent ($POST "post"))
			(set 'PostDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
			(set 'PostLastDate PostDate)
			(set 'PostTags ($POST "tags"))
			(if (and (= PostType "Podcast") (or (nil? PostTags) (= PostTags ""))) (setq PostType "podcast")) ; if you don't set a tag for a podcast, set it to "podcast"
			(set 'PostPoll ($POST "polltopic")) ; for polls, adds text to post and info to database
			(set 'PostPollValues ($POST "pollvalues"))
			(displayln "Post Poll Values: " PostPoll ">>>")
			(if (not (= PostPoll "")) (begin
				(set 'poll-prepend-text (string "[h4]" PostPoll "[/h4]\n[poll]"))
				(set 'PostPollSubject PostSubject) ; we need a copy of this variable  
				(replace " " PostPollSubject "_") ; this is to generate unique form names for each poll
				(set 'PostPollValues (parse PostPollValues "\n"))
				(dolist (p PostPollValues)
					(extend poll-prepend-text (string "\n" "[radio]" PostPollSubject " value=" $idx))
					(if (= $idx 0) (extend poll-prepend-text " checked=yes"))	
					(extend poll-prepend-text (string "[/radio] " p ))
				)
				(set 'PostContent (string poll-prepend-text " [/poll]\n\n\n " PostContent))
			))
			(displayln "Post Type (before): " PostType)
			(if (nil? PostType) (begin ; regular users can make Forum Posts but nothing else, admins can make multiple PostTypes
				(if (= post-type-trigger "Forum")
					(set 'PostType "Forum post")
					(set 'PostType "Blog post"))
			))
			(displayln "Post Type (after): " PostType)
			(create-record "Posts" Id PosterId PostDate PostSubject PostContent PostType PostTags PostLastAuthor PostLastDate) ; It's the C in CRUD!
			; now update the user's postcount! postcount++!!
			(set 'UserId Rockets:UserId)
			(set 'UserPosts (++ Rockets:UserPosts))
			(update-record "Users" UserId UserPosts)
			(if (= PostType "Podcast") (begin 
				(displayln "<p>Adding podcast...</p>")
				; add to the podcast XML feed in /podcast/ based on the podcast tag given
				; if no podcast tag then just call it "podcast".
				; this will all be in a partial file at some point - so we can remake the XML file even if we hit "edit"
				; but that means we have to do a query and get all podcast posts each time
				(set 'quo (char 34)) ; quote character, we'll be using it a lot
				(set 'cr (append (char 13) (char 10))) ; carriage return + LF character
				(setq PostTags (force-parameters 1 PostTags)) ; one-word only podcasts (for now-- can fix later)
				(displayln "<p>Looking for podcasts with tag: " PostTags "</p>")
				(setq podcast-file (string "podcast/" PostTags ".xml")) ; PostTags for a podcast will be "podcast" if blank
				; now we have to make the xml file. First, get all podcasts of the tag PostTag
				(setq podcast-feed (query (string "SELECT * FROM Posts WHERE PostType='Podcast' AND PostTags='" PostTags "' ORDER BY PostDate DESC;")))
				(displayln "<p>Podcast query results:</p>" podcast-feed)
				(if podcast-feed (begin 
					; go through all results from the feed and save them to an XML file
					; first, let's get the podcast config options from RocketsConfig (look for the tag in config, if no tag, use default config options)
					(setq podcast-ref (ref PostTags RocketsConfig:PodcastList))
					(if (nil? podcast-ref)
						(setq podcast-settings '("podcast" "Podcast Title" "Podcast Copyright" "Podcast Subtitle" "Podcast Author" "Podcast Summary" "Podcast Owner" "Podcast Email" "Podcast Image" "Podcast Category" "Podcast Subcategory"))
						(setq podcast-settings (RocketsConfig:PodcastList (first podcast-ref)))
					)
					(set 'silly-string " ")
					(set 'silly-string (append "<?xml version=" quo "1.0" quo " encoding=" quo "UTF-8" quo "?>"))
					(set 'silly-string (extend silly-string cr "<rss xmlns:itunes=" quo "http://www.itunes.com/dtds/podcast-1.0.dtd" quo " version=" quo "2.0" quo ">"))
					(set 'silly-string (extend silly-string cr "<channel>"))
					(set 'silly-string (extend silly-string cr "<title>" (podcast-settings 1) "</title>"))
					(set 'silly-string (extend silly-string cr "<link>" RocketsConfig:SiteURL "" "</link>"))
					(set 'silly-string (extend silly-string cr "<language>en-us</language>"))
					(set 'silly-string (extend silly-string cr "<copyright>" (podcast-settings 2) "</copyright>"))
					(set 'silly-string (extend silly-string cr "<itunes:subtitle>" (podcast-settings 3) "</itunes:subtitle>"))
					(set 'silly-string (extend silly-string cr "<itunes:author>" (podcast-settings 4) "</itunes:author>"))
					(set 'silly-string (extend silly-string cr "<itunes:summary>" (podcast-settings 5) "</itunes:summary>"))
					(set 'silly-string (extend silly-string cr "<description>" (podcast-settings 5) "</description>"))
					(set 'silly-string (extend silly-string cr "<itunes:new-feed-url>" podcast-file "</itunes:new-feed-url>"))
					(set 'silly-string (extend silly-string cr "<itunes:owner>"))
					(set 'silly-string (extend silly-string cr "<itunes:name>" (podcast-settings 6) "</itunes:name>"))
					(set 'silly-string (extend silly-string cr "<itunes:email>" (podcast-settings 7) "</itunes:email>"))
					(set 'silly-string (extend silly-string cr "</itunes:owner>"))
					(set 'silly-string (extend silly-string cr "<itunes:image href=" quo "images/" (podcast-settings 8) quo " />"))
					(set 'silly-string (extend silly-string cr "<itunes:category text=" quo (podcast-settings 9) quo ">"))
					(set 'silly-string (extend silly-string cr "<itunes:category text=" quo (podcast-settings 10) quo "/>"))
					(set 'silly-string (extend silly-string cr "</itunes:category>"))
					(dolist (x podcast-feed)
						; add the XML for each episode of the podcast
						(set 'silly-string (extend silly-string cr "<item>"))
						(set 'podcast-item-title (x 3))
						(set 'podcast-item-subtitle (x 3))
						(set 'podcast-item-summary (x 4))
						(set 'podcast-item-date (date (integer (x 2)) 0 "%a, %d %b %Y %H:%M:%S %Z"))
						(set 'podcast-filename "temp-testing.mp3") ; TEMPORARY TEST!!!! CHANGE WHEN WE EDIT!
						(set 'podcast-item-duration "35:00") ; <- change obviously
						(set 'podcast-item-keywords "test, test, test") ; <- NEED TO ADD TO PODCAST CONFIG
						(set 'podcast-item-url (string RocketsConfig:SiteURL "/audio/" podcast-filename))
						(set 'silly-string (extend silly-string cr "<itunes:summary>" podcast-item-summary "</itunes:summary>"))
						(set 'silly-string (extend silly-string cr "<enclosure url=" quo podcast-item-url quo " length=" quo "40000" quo " type=" quo "audio/x-mp3" quo " />"))
						(set 'silly-string (extend silly-string cr "<guid>" podcast-item-url "</guid>"))
						(set 'silly-string (extend silly-string cr "<pubDate>" podcast-item-date "</pubDate>"))
						(set 'silly-string (extend silly-string cr "<itunes:duration>" podcast-item-duration "</itunes:duration>"))
						(set 'silly-string (extend silly-string cr "<itunes:keywords>" podcast-item-keywords "</itunes:keywords>"))
						(set 'silly-string (extend silly-string cr "</item>"))
					)
					(set 'silly-string (extend silly-string cr "<itunes:explicit>Clean</itunes:explicit>"))
					(set 'silly-string (extend silly-string cr "</channel>"))
					(set 'silly-string (extend silly-string cr "</rss>"))
					; write the XML file to disk
					(write-file podcast-file silly-string)
				)) ; end if podcast-feed (if we got a result from query)
			)) ; end if PostType = 'Podcast'
		) ; end section of posting post from POST
	)
	)) ; end "if continue" check

	; Generate a full RSS feed XML file for the blog
	(display-partial "rockets-generate-rss")

)) ; end check to see if user is signed in

;(displayln "<a href='rockets-main.lsp'>Click here to return to the main page.</a>") ; for debugging
(if (= PostType "Forum post")
	(page-redirect "rockets-forum.lsp")
	(page-redirect "rockets-main.lsp"))
(display-page)
