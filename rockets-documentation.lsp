#!/usr/bin/env newlisp 
(load "/var/www/newlisp-rockets.lisp") 
(display-header "newLISP on Rockets Documentation" "docs")
(open-database "ROCKETS-BLOG")
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-documentation")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(displayln "<h1>Documentation</h1>")
(displayln "<p>These are the basic functions of newLISP on Rockets.  Additional documentation and tutorials are coming soon.</p>")
(set 'documentation-list (parse (read-file "newlisp-rockets.lisp") "\n"))

; print headers and links

(start-div "row")
(start-div "span3 bs-docs-sidebar")
(displayln "<ul class='nav nav-list bs-docs-sidenav'>")

(dolist (h documentation-list)
	(set 'line h)
	(set 'link h)
	(set 'tag (slice h 0 2))
	(if (= tag ";!") (begin
		(replace ";!" line "")
		(replace "=" line "")
		(replace ";!" link "")
		(replace "=" link "")
		(set 'link (trim link))
		(replace " " link "_")
		(displayln "<li><a href=#" link "><i class='icon-chevron-right'></i>" line "</a></li>"))))
(displayln "</ul>")
(end-div)

(start-div "span9")

(dolist (d documentation-list)
	(set 'line d)
	(set 'tag (slice d 0 2))
	(if (= tag ";!") (begin 
		(replace ";!" line "")
		(replace "=" line "")
		(set 'link (trim line))
		(replace " " link "_")
		(displayln "<a name=" link "></a><p><br><br><h2>" line "</h2>"))
	)
	(if (= tag ";;") (begin
		(replace ";;" line "")
		(replace "<" line "&lt;")
		(replace ">" line "&gt;")
		(replace "*" line "<li>")
		(replace "Returns:" line "<b>Returns:</b>")
		(replace "Optional:" line "<b><i>Optional:</b></i>")
		(replace "Note:" line "<i>Note:</i>")
		(replace "Example:" line "<i>Example:</i>")
		(if (find "Function:" line) (display "<h4>"))
		(if (find "Usage:" line) (display "<h5>"))
		(displayln "<br>" line "")
		(if (find "Function:" line) (display "</h4>"))
		(if (find "Usage:" line) (display "</h5>"))
		))
	(if (= tag ";-") (begin
		(displayln "<hr>")
	))
)

(end-div)
(end-div)
(display-footer "Rocket Man") 
(display-page)