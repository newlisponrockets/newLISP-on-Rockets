#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

;; Rockets 2.0 - rockets-comic.html
;;
;; This is a view of comics.  The idea is to have multiple comics based on tags.
;; First we get the whole list of comics, and then identify unique tags, then display
;; comic based on that tag in date order with prev/next and including comments.


(load "Rockets-config.lisp") ; load configuration information
(display-header RocketsConfig:Name)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-main")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus


; function to display a list of available comics
(define (display-comic-list)
	; get the total list of comics
	(set 'total-comics-list (query "Select DISTINCT PostTags from Posts WHERE PostType='Comic';"))
	(if (nil? total-comics-list)
		(displayln "<p><br><br>Sorry! You don't have any comics yet on this site. Try posting some!")
	(begin
		(displayln "<br><p> List of comics on this site: ")
		(dolist (x total-comics-list)
			(displayln "<p>&nbsp;&nbsp;&nbsp;<a href='rockets-comic.lsp?c=" (first x) "'><img src='includes/images/monarch/comics/" (first x) ".jpg'></a>")
			(displayln "<p>&nbsp;&nbsp;&nbsp;<a href='rockets-comic.lsp?c=" (first x) "'>" (first x) "</a>"))
	))
)

; function to print out a comment box form
(define (print-post-comment-box)
	(displayln "<h4>Post comment</h4>")
	(displayln "<p><form name='comment' action='m-comment.cgi'><textarea name='post' cols='50' rows='10'></textarea><input type='hidden' name='r' value='comic?c=" comic-string "&i'><input type='hidden' name='i' value=" current-comic-num "><p><input type=submit value='Post'></form>"))

; function to display comments (again may be separate function later)
(define (display-comments list-comments)
	(displayln "<p><h4>Comments:</h4>")
	(dolist (x list-comments)
		(displayln "<P><i>By " (x 1) " on " (date (integer (x 3)))"</i>")
		(set 'thing-to-display (x 4))
		(set 'thing-to-display (replace (char 13) thing-to-display "<br>"))
		(set 'thing-to-display (replace "<script" thing-to-display ""))
		(set 'thing-to-display (replace "</script>" thing-to-display ""))
		(displayln "<p>" thing-to-display)))

; function to display all comics in a series with forward/back/next/prev buttons
(define (display-comic comic-string comic-item)

	(set 'comic-list (query (append "SELECT Id FROM Posts WHERE (PostType='Comic' AND PostTags='" comic-string "');")))
	;get rid of inner lists inside list
	(dolist (z comic-list)
		(replace z comic-list (first z)))
	(if comic-item ; if it's a specific comic display that one
		(set 'display-list (first (query (append "SELECT * FROM Posts WHERE Id=" (string comic-item))))) ; otherwise display most recent
		(set 'display-list (first (query (append "SELECT * FROM Posts WHERE Id=" (string (last comic-list)))))))
	(set 'comic-title (display-list 3))
	(set 'comic-text (display-list 4))	
	(set 'list-len (length comic-list))

	(if comic-item
	 	(set 'current-comic-num (int comic-item))
	  	(set 'current-comic-num (last comic-list)))
	(set 'current-comic-pos (int (first (ref current-comic-num comic-list))))
	(displayln "<table border=0><tr><td width=200></td><td width=200>")
	(if (> current-comic-pos 0)
		(displayln "<p><a href='rockets-comic.lsp?c=" comic-string "&i=" (comic-list 0) "'><img src=images/nav-button-first.jpg></a>"))
	(displayln "</td><td width=200>")
	(if (> current-comic-pos 0)
		(displayln "<a href='rockets-comic.lsp?c=" comic-string "&i=" (comic-list (- current-comic-pos 1)) "'><img src=images/nav-button-prev.jpg></a> "))
	(displayln "</td><td width=200>")
	(if (< current-comic-pos (- list-len 1))
		(displayln "<a href='rockets-comic.lsp?c=" comic-string "&i=" (comic-list (+ current-comic-pos 1)) "'><img src=images/nav-button-next.jpg></a> "))
	(displayln "</td><td width=200>")
	(if (< current-comic-pos (- list-len 1))
		(displayln "<a href='rockets-comic.lsp?c=" comic-string "&i=" (comic-list (- list-len 1)) "'><img src=images/nav-button-last.jpg>"))
	(displayln "</td><td width=200></td></tr></table>")
	(displayln "<br><br>")

	(display-individual-post display-list true comic-item true) ; show comments but hide the headers (last parameter) for comic view
)

;--------------------------- page start


(set 'comic-type ($GET "c"))
(set 'comic-item (force-parameters 1 ($GET "i")))

;(if comic-type (displayln "Comic type:" comic-type))

; if we're requesting a specific comic, do that, otherwise display list
(if comic-type
	(display-comic comic-type comic-item)
	(display-comic-list))

(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!	
		
 



 
	

 
