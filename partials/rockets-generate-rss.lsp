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
					(set 'silly-string (extend silly-string cr "<rss version=" quo "2.0" quo ">"))
					(set 'silly-string (extend silly-string cr "<channel>"))
                                        (set 'silly-string (extend silly-string cr "  <title>" RocketsConfig:ShortName "" "</title>"))
                                        (set 'silly-string (extend silly-string cr "  <link>" RocketsConfig:SiteURL "" "</link>"))
                                        (set 'silly-string (extend silly-string cr "  <description>" RocketsConfig:Name "" "</description>"))

                                        (dolist (x blog-feed)
                                                ; add the XML for each episode of the podcast
                                                (displayln "<p>Processing result for post #: " $idx "</p>")
                                                (set 'silly-string (extend silly-string cr "  <item>"))
                                                (set 'silly-string (extend silly-string cr "    <title>" (x 3) "</title>"))
                                                (set 'silly-string (extend silly-string cr "    <link>" RocketsConfig:SiteURL "/rockets-item.lsp?p=" (string (x 0)) "</link>"))
                                                (set 'silly-string (extend silly-string cr "    <description>" (x 4) "</description>"))
						(set 'silly-string (extend silly-string cr "  </item>"))
                                        )
                                        (set 'silly-string (extend silly-string cr "</channel>"))
					(set 'silly-string (extend silly-string cr "</rss>"))
					(write-file blog-file silly-string)
                                ))

; end of blog feed

