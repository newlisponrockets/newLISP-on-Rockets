; Rockets partial file used to generate an XML file for RSS readers
; 
; Written July 21, 2021 by Jeremy Reimer
;
; called from: rockets-post.lsp, rockets-post-edit.lsp
;

(displayln "<p>Hello! We are creating a RSS XML file here...</p>")
                                (set 'quo (char 34)) ; quote character, we'll be using it a lot
                                (set 'cr (append (char 13) (char 10))) ; carriage return + LF character
				(set 'blog-file "blog-feed.xml")
 

(setq blog-feed (query (string "SELECT * FROM Posts WHERE PostType='Blog post' ORDER BY PostDate DESC;")))
                                (displayln "<p>Blog query results:</p>" blog-feed)
                                (if blog-feed (begin
                                        (set 'silly-string " ")
                                        (set 'silly-string (append "<?xml version=" quo "1.0" quo " encoding=" quo "UTF-8" quo "?>"))
					(set 'silly-string (extend silly-string cr "<rss version=" quo "2.0" quo " xmlns:atom=" quo "http://www.w3.org/2005/Atom" quo ">"))
					(set 'silly-string (extend silly-string cr "<channel>"))
                                        (set 'silly-string (extend silly-string cr "  <title>" RocketsConfig:ShortName "</title>"))
                                        (set 'silly-string (extend silly-string cr "  <link>http://" RocketsConfig:SiteURL "</link>"))
                                        (set 'silly-string (extend silly-string cr "  <description>" RocketsConfig:Name "</description>"))
					(set 'silly-string (extend silly-string cr "  <atom:link href=" quo "http://" RocketsConfig:SiteURL "/blog-feed.xml" quo " rel=" quo "self" quo " type=" quo "application/rss+xml" quo " />")) 

                                        (dolist (x blog-feed)
                                                ; add the XML for each episode of the podcast
                                                (displayln "<p>Processing result for post #: " $idx "</p>")
						(set 'post-content (x 4))
						(replace "<" post-content "&lt;")
						(replace ">" post-content "&gt;")
						(replace cr post-content "&lt;br&gt;")
						(replace "/" post-content "&#x2F;")
                                                (set 'silly-string (extend silly-string cr "  <item>"))
                                                (set 'silly-string (extend silly-string cr "    <title>" (x 3) "</title>"))
						(set 'temp-link (append "http://" RocketsConfig:SiteURL "/rockets-item.lsp?p=" (string (x 0))))
                                                (set 'silly-string (extend silly-string cr "    <link>" temp-link "</link>"))
						(set 'silly-string (extend silly-string cr "    <guid>" temp-link "</guid>"))
                                                (set 'silly-string (extend silly-string cr "    <description>" post-content "</description>"))
						(set 'silly-string (extend silly-string cr "  </item>"))
                                        )
                                        (set 'silly-string (extend silly-string cr "</channel>"))
					(set 'silly-string (extend silly-string cr "</rss>"))
					; debugging
					;(displayln silly-string)
					(displayln "Writing XML to file...")
					(write-file blog-file silly-string)
                                ))

; end of blog feed
