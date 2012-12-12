; Newlisp on Rockets framework
; ----------------------------
;
; For version number, see below.  For revision history, see revision-history.txt
;
; I have to give a HUGE acknowledgement to the Dragonfly framework at http://rundragonfly.com
; written by Marc Hildmann and Greg Slepak and available for download at: http://code.google.com/p/dragonfly-newlisp/downloads/list
; Without Dragonfly I would have been unable to do Rockets at all.  Some parts of Rockets contain
; snippets of Dragonfly code.  Also thanks to Lutz Mueller, the author of newLISP at http://www.newlisp.org 
;
; Copyright 2012 by Rocket Man
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

;------------------------------------------------------------------------------------------------------------

;!===== DEPENDENCIES ============================================================
;;* css/boostrap.css - Bootstrap framework CSS file
;;* css/bootstrap-responsive.css - Bootstrap responsive framework (for mobile devices)
;;* css/datepicker.css - For calendar dropdowns
;;* js/jquery-1.8.2.min.js - jQuery (minimum version required)
;;* js/bootstrap.min.js - Bootstrap Javascript
;;* js/boostrap-datepicker.js - For calendar dropdowns

;!===== GLOBAL VARIABLES ========================================================
;;* $ROCKETS_VERSION - current version of Rockets
(constant (global '$ROCKETS_VERSION) 0.23)    
;;* $MAX_POST_LENGTH - maximum size of data you are allowed to POST
(constant (global '$MAX_POST_LENGTH) 83886080) 
;;* $BASE_PATH - the absolute path for the installation (default is /)
(constant (global '$BASE_PATH) "/") 
;;* $PARTIAL_PATH - the relative path for when using (display-partial) (default is "partials")
(constant (global '$PARTIAL_PATH) "partials") 

;====== CONSTANTS ================================================================
(constant 'REGEX_HTTP_SPECIAL_STR (regex-comp {([^.0-9a-z]+)} 1))
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))

(set 'Rockets:status-codes
  '((200 "OK")
	(301 "Moved Permanently")
	(302 "Found")
	(400 "Bad Request")
	(401 "Unauthorized")
	(403 "Forbidden")
	(404 "Not Found")
	(410 "Gone")
	(500 "Internal Error"))
)

; This is the global buffer that Rockets writes to before printing the page out at the end
(set 'STDOUT "")

;===== INTERNAL FUNCTIONS ==========================================================================
; These should never normally be called directly.  They are not included in the documentation.

; Printing stuff to the screen
; define (display) and (displayln) to store to the variable STDOUT
(define (displayln)
	(apply display (push "\n" (args) -1)))

(define (display)
	(extend STDOUT (apply string $args))
	(last $args)) ; to behave the same way as print

; This reads in any file as part of the calling page
(define (display-file str-filename)
	(eval-string (read-file str-filename)))

; this function takes encoded strings and returns them to human-readable
; the regex and code were adapted from Dragonfly.  Basically %2B->"+", %27->"'", etc
(define (url-decode str-to-decode)
	(replace "+" str-to-decode " ")
	(replace REGEX_HEX_ENCODED_CHAR str-to-decode (pack "b" (int $1 nil 16)) 0x10000))

; this is a parsing function for both $GET and $POST, can be used for any other global tree
; it parses input based on "&" as a separator, and if you have multiple items with "[]" at the end
; it adds them to the same key in the tree, making life more convenient for everybody
(define (parse-get-or-post thing-to-parse tree-to-add)
		(dolist (r (parse thing-to-parse "&"))
			(let (rtemp (parse r "="))
			(if (> (length rtemp) 1) (begin 
				; if it's multi-value (last 2 chars end in []) AND this value exists in the tree already, add it to the list
 				(if (and (tree-to-add (rtemp 0)) (ends-with (rtemp 0) "[]"))
					(tree-to-add (rtemp 0) (flat (push (tree-to-add (rtemp 0)) (list (url-decode (rtemp 1))))))
					(tree-to-add (rtemp 0) (url-decode (rtemp 1))))
			))
			)))

; adds a new header (location, etc) to be set in the page's HTTP header (will take effect next page)
(define (add-header str-header-name str-header-value)
 (push (list str-header-name str-header-value) Rockets:headers -1)
)

; this prints a bunch of HTTP headers, starting with status, then any other headers, then cookies.
(define (send-headers)
	(print "Status: " Rockets:statuscode " " (lookup Rockets:statuscode Rockets:status-codes) "\n")
	; send cookies if they exist 
	(if Rockets:headers (begin
		(dolist (r Rockets:headers)
			(print (first r) ": " (last r) "\n"))))
	(if Rockets:cookielist (begin
		(dolist (r Rockets:cookielist)
			(print "Set-Cookie: " r "\n"))))
)

;! ===== BASIC FUNCTIONS =============================================================================

;; Function: (benchmark-result) 
;; Usage: (benchmark-result)
;; Returns: displays the text: "Rendered in ## milliseconds.  Used ## KB of memory, ## for LISP cells"
;-----------------------------------------------------------------------------------------------------

(set 'microtime-start (time-of-day))

(define (benchmark-result)
        (set 'mem_cells_bytes (* (sys-info 0) 16))
        (set 'mem_cells_kilobytes (/ mem_cells_bytes 1024))
        (set 'mem_cells-constant_bytes (* (sys-info 1) 16))
        (set 'mem_cells-constant_kilobytes (/ mem_cells-constant_bytes 1024))
        (set 'mem_cells-constant_megabytes (/ mem_cells-constant_kilobytes 1024))
        (set 'mem_symbols_bytes (* (sys-info 2) 32))
        (set 'mem_symbols_kilobytes (/ mem_symbols_bytes 1024))
			(set 'mem_total_usage (+ mem_cells_kilobytes mem_symbols_kilobytes))
        (set 'microtime-end (time-of-day))
        (set 'execution-time-milliseconds (- microtime-end microtime-start))
        (set 'execution-time-seconds (div execution-time-milliseconds 1000))
        (displayln "Rendered in " execution-time-milliseconds " milliseconds. Used " mem_total_usage " KB of memory, " mem_cells_kilobytes " KB for Lisp Cells."))

; note: I'm not overloading print and println for the moment, because it involves context switches
; that seem overly complicated, so I'm using (display) and (displayln) as replacements instead. But
; I might switch back, so that's why this code is here but commented out.
;(constant (global 'sys-print) print)
;(constant 'print print)
;(constant (global 'sys-println) println)
;(constant 'displayln println)

;; Function: (start-div)
;; Usage: (start-div "Div class name")
;; Returns: Starts an HTML <DIV> with a given class name
;-----------------------------------------------------------------------------------------------------
(define (start-div str-div-name)
	(displayln "<div class=\"" str-div-name "\">"))

;; Function: (end-div)
;; Usage: (end-div)
;; Returns: Closes off an HTML <DIV>
;-----------------------------------------------------------------------------------------------------
(define (end-div)
	(displayln "</div>"))

;; Function: (format-for-web)
;; Usage: (format-for-web "string of text")
;; Returns: Takes a string of text, for example a post full of data including carriage returns and
;; line feeds, and returns a string that can be used with (display) or (displayln)
;; Note: All HTML code is removed (< and > translated to & lt ; and & gt ; respectively) to avoid
;; cross-site scripting issues.  URLs are transformed into clickable links.  Some UBB code is also
;; translated, for example bold, italic, underline, and code tags.
;-----------------------------------------------------------------------------------------------------
(define (format-for-web str-input-for-web)
	; let's get rid of cross-site scripting, and for that matter all embedded HTML
	(replace "<" str-input-for-web "&lt;")
	(replace ">" str-input-for-web "&gt;")
	; but we need a way to do bold and italics at least, so let's do that, and images
	(set 'ubb-code-list '("i" "b" "u" "code" "pre"))
	(dolist (u ubb-code-list)
		(replace (string "[" u "]") str-input-for-web (string "<" u ">"))
		(replace (string "[" (upper-case u) "]") str-input-for-web (string "<" u ">"))
		(replace (string "[/" u "]") str-input-for-web (string "</" u ">"))
		(replace (string "[/" (upper-case u) "]") str-input-for-web (string "</" u ">")))
	(replace "[img]" str-input-for-web "<img src='")
	(replace "[/img]" str-input-for-web "'>")
	(replace "[IMG]" str-input-for-web "<img src='")
	(replace "[/IMG]" str-input-for-web "'>")
	; replace html links with clickable links
  	(set 'h "(?:^|[^=])((ftp|http|https|file):\\/\\/[\\S]+(\\b|$))")
  	(replace h str-input-for-web (string " <a href='" $1 "' target='new'>" $1 "</a>") 0)
	; replace youtube links
	(replace "[youtube]" str-input-for-web "<iframe width=\"560\" height=\"315\" src=\"http://www.youtube.com/embed/")
	(replace "[YOUTUBE]" str-input-for-web "<iframe width=\"560\" height=\"315\" src=\"http://www.youtube.com/embed/")
	(replace "[/youtube]" str-input-for-web "\" frameborder=\"0\" allowfullscreen></iframe>")
	(replace "[/YOUTUBE]" str-input-for-web "\" frameborder=\"0\" allowfullscreen></iframe>")
	; replace line breaks with HTML line breaks
	(replace "\r\n" str-input-for-web "<BR>")
)

;; Function: (force-parameters)
;; Usage: (force-parameters int-number-of-parameters "string containing spaces")
;; Example: (force-parameters 1 "First Word Only Returned")
;; Returns: Takes a string containing spaces and returns only the number of parameters provided
;; Use this in conjunction with ($GET) and ($POST) to eliminate SQL injection attempts
;-----------------------------------------------------------------------------------------------------
(define (force-parameters int-num-of-parameters str-to-parameterize)
	(if str-to-parameterize 
		(begin 
			(set 'tmp-to-parameterize (slice (parse str-to-parameterize " ") 0 int-num-of-parameters))
			(set 'return-result (join tmp-to-parameterize " ")))
		(set 'return-result nil)		
	)
)
		

;! ===== DISPLAY FUNCTIONS ===========================================================================

;; Function: (display-header)
;; Usage: (display-header "Optional page title" "optional-css-file")
;; Returns: Prints all the opening page HTML, such as page title, meta tags, and CSS
;; Note: Only one optional css file can be included for now.  The .css extension is not necessary.
;-----------------------------------------------------------------------------------------------------
(define (display-header str-page-title str-optional-css)
	(if (nil? str-page-title) (set 'str-page-title "newLISP on Rockets"))
	(displayln "<html lang=\"en\"><head><meta charset=\"UTF-8\">")
	(displayln "<title>" str-page-title "</title>")
   (displayln "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
	(displayln "<link href=\"" $BASE_PATH "css/bootstrap.css\" rel=\"stylesheet\">") ; loads Bootstrap CSS
	(displayln "<link href=\"" $BASE_PATH "css/bootstrap-responsive.css\" rel=\"stylesheet\">")
	(displayln "<link href=\"" $BASE_PATH "css/datepicker.css\" rel=\"stylesheet\">") ; loads date picker
	(if str-optional-css (displayln "<link href=\"" $BASE_PATH "css/" str-optional-css ".css\" rel=\"stylesheet\">"))
	;(displayln "<style> body { padding-top: 60px; /* fixes the container spacing */   }</style>")
	(displayln "</head><body data-spy=\"scroll\" data-target=\".bs-docs-sidebar\" data-twittr-rendered=\"true\">")
)

;; Function: (display-navbar)
;; Usage: (display-header "Site name" '(list of menus) "page-to-go-for-signing-in")
;; Returns: Prints the top navigation bar with menus and form for signing in. Also, 
;; calling this function also sets up the main <div> container for the whole page.
;; Note: The .lsp extension is added automatically to page-to-go-for-signing-in
;-----------------------------------------------------------------------------------------------------
(define (display-navbar str-name list-menus str-signin)
	(displayln "    <div class=\"navbar navbar-inverse navbar-fixed-top\">")
	(displayln "      <div class=\"navbar-inner\">")
	(displayln "        <div class=\"container\">")
	(displayln "          <a class=\"btn btn-navbar\" data-toggle=\"collapse\" data-target=\".nav-collapse\">")
	(displayln "            <span class=\"icon-bar\"></span>")
	(displayln "            <span class=\"icon-bar\"></span>")
	(displayln "            <span class=\"icon-bar\"></span>")
	(displayln "          </a>")
	(if (or (find ".png" str-name) (find ".jpg" str-name) (find ".gif" str-name)) ; name can be an image
		(displayln "          <a class=\"brand\" href=\"\"><img src=images/" str-name "></a>")
		(displayln "          <a class=\"brand\" href=\"\">" str-name "</a>"))
	(displayln "          <div class=\"nav-collapse collapse\">")
	(displayln "            <ul class=\"nav\">")
	(if list-menus (begin 
		(dolist (d list-menus)
			(display "              <li")
			(if (= (length d) 3) (display " class=\"active\"")) ; for active menu
			(displayln "><a href=\"" (d 1) ".lsp\"" (lower-case (d 0)) "\">" (d 0) "</a></li>"))))
	(displayln "            </ul>")
	(if Rockets:UserId (begin 
		(displayln "            <div style=\"display:inline-block\" class=\"pull-right\">")
		(displayln "              <ul class=\"nav pull-right\">")
		(displayln "                <li class=\"divider-vertical\"></li>")
		(displayln "                <li class=\"dropdown\">")
		(displayln "                  <a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\">Welcome, " Rockets:UserName "&nbsp;<b class=\"caret\"></b></a>")
		(displayln "                  <ul class=\"dropdown-menu\"><li><a href=\"rockets-profile.lsp\">Edit Profile</a></li>")
		(displayln "                                              <li><a href=\"rockets-signout.lsp\">Sign Out</a></li></ul>")
		(displayln "                </li>")
		(displayln "              </ul>")
		(displayln "            </div>")
		) (begin
		(displayln "            <form class=\"navbar-form pull-right\" method=\"post\" action=\"" str-signin ".lsp\">")
		(displayln "              <input class=\"span2\" name=\"activepage\" id=\activepage\" type=\"hidden\" value=\"" active-page "\">")
		(displayln "              <input class=\"span2\" name=\"email\" id=\"email\" type=\"text\" placeholder=\"Email\">")
		(displayln "              <input class=\"span2\" name=\"password\" id=\"password\" type=\"password\" placeholder=\"Password\">")
		(displayln "              <button type=\"submit\" class=\"btn\">Sign in</button>")
		(displayln "            </form>")
	)	)
	(displayln "          </div>")
	(displayln "       </div>")
   (displayln "     </div>")
	(displayln "    </div>")
	(displayln " <div class=\"container\" style=\"padding-top: 50px;\">") ; start the main container for the page, add padding
)

;; Function: (display-partial)
;; Usage: (display-partial "partial-file-name")
;; Returns: Loads the instructions in the file "partial-file-name" in the $PARTIAL_PATH subdirectory
;; Note: The ".lsp" extension is added automatically.
;-----------------------------------------------------------------------------------------------------
(define (display-partial partialname)
  	(display-file (string $PARTIAL_PATH "/" partialname ".lsp")))

;; Function: (display-footer)
;; Usage: (display-footer "Optional Company Name")
;; Returns: Prints the footer with benchmark result.  Also loads Javascript libraries.
;-----------------------------------------------------------------------------------------------------
(define (display-footer str-company-name)
	(if (nil? str-company-name) (set 'str-company-name ""))
	(display "<hr><footer><p>")
	(display-image "poweredby.png")
	(displayln "&copy; " (date (date-value) 0 "%Y") " " str-company-name ". ") ; always prints current year
	(displayln "<script src=\"" $BASE_PATH "js/jquery-1.8.2.min.js\"></script>") ; Loads jQuery
	(displayln "<script src=\"" $BASE_PATH "js/bootstrap.min.js\"></script>") ; Loads Bootstrap Javascript.
	(displayln "<script src=\"" $BASE_PATH "js/bootstrap-datepicker.js\"></script>") ; Loads Bootstrap datepicker Javascript.
	(if $FORM-DATEPICKER (begin ; add jQuery triggers for calendar datepickers if present
		(dolist (f $FORM-DATEPICKER)
			(displayln "<script> $(function(){	$('#" f "').datepicker({format: 'mm-dd-yyyy'}); });</script>"))
	))
	(displayln (benchmark-result) "</footer></div>") ; ends main container
	(displayln "</body></html>"))

;; Function: (display-image)
;; Usage: (display-image "imagename.jpg" int-width int-height)
;; Example: (display-image "rocket.jpg" 200 100)
;; Returns: Displays an image from the default /images/ subdirectory.  Width and height are optional  
;-----------------------------------------------------------------------------------------------------
(define (display-image str-image-to-print int-width int-height)
	(display (string "<img src=" $BASE_PATH "images/" str-image-to-print))
	(if int-width (display (string " width=" int-width)))
	(if int-height (display (string " height=" int-height)))
	(displayln ">"))

;; Function: (display-paging-links)
;; Usage: (display-paging-links int-start-page int-total-pages int-current-page "page-url")
;; Example: (display-paging-links 1 99 2 "rockets-main")
;; Returns: Displays a list of clickable paging links, in this example from page 1 to 99, current page 2
;; The "page-url" (.lsp extension added automatically) is the page that displays the content
;-----------------------------------------------------------------------------------------------------
(define (display-paging-links int-start-page int-total-pages int-current-page str-page-url)
	(start-div "pagination")
	(display "<ul>")
	(for (x int-start-page int-total-pages 1)
	(display "<li")
		(if (= x int-current-page) (display " class='active'"))
		(displayln "><a href='" str-page-url ".lsp?p=" x "'>" x "</a></li>")
	)
	(display "</ul>")
	(end-div)
)

;; Function: (display-success)
;; Usage: (display-success "Congratulations on your success!")
;; Returns: Displays a success text in a light green box that can be dismissed by clicking the "X" button.
;-----------------------------------------------------------------------------------------------------
(define (display-success str-success-text)
	(start-div "alert alert-success")
		(displayln "<button type='button' class='close' data-dismiss='alert'>&times;</button>")
		(displayln str-success-text)
	(end-div))

;; Function: (display-warning)
;; Usage: (display-warning "Warning: this is warning text!")
;; Returns: Displays a warning in a light yellow box that can be dismissed by clicking the "X" button.
;-----------------------------------------------------------------------------------------------------
(define (display-warning str-warning-text)
	(start-div "alert")
		(displayln "<button type='button' class='close' data-dismiss='alert'>&times;</button>")
		(displayln str-warning-text)
	(end-div))

;; Function: (display-error)
;; Usage: (display-error "Error: this is error text!")
;; Returns: Displays a warning in a light red box that can be dismissed by clicking the "X" button.
;-----------------------------------------------------------------------------------------------------
(define (display-error str-error-text)
	(start-div "alert alert-error")
		(displayln "<button type='button' class='close' data-dismiss='alert'>&times;</button>")
		(displayln str-error-text)
	(end-div))

;; Function: (display-page)
;; Usage: (display-page)
;; Returns: Displays the current page and exits.  This should normally be the last command on any page 
;; written in newLISP on Rockets.
;-----------------------------------------------------------------------------------------------------
(define (display-page)
	; Sending the page starts here-----------------------------------------------------------------------------
	; print headers
	(print "Content-type: text/html\n") 
	(set 'Rockets:statuscode 200) ; everything is OK
	(send-headers)
	(print "\n")
	(print "<!DOCTYPE html>") 	
	(print STDOUT) ; the whole page gets put here
	(exit)
)


;! ===== PAGE HANDLING ==========================================================

;; Function: (page-redirect)
;; Usage: (page-redirect "page-to-redirect-to" "p=optional-parameters")
;; Loads a new URL immediately when executed.  The current page will not be displayed to the user.
;; Note: You can add optional parameters to the redirected page.
;; Note: The ".lsp" extension is optional and will be added automatically if not entered.
;-----------------------------------------------------------------------------------------------------
(define (page-redirect str-url-to-redirect str-optional-parameters)
	(if (not (find ".lsp" str-url-to-redirect))
		(extend str-url-to-redirect ".lsp")) ; add .lsp extension if not already there
	(if str-optional-parameters (begin
		(if (find "?" strl-url-to-redirect)
			(extend str-url-to-redirect (string "&" str-optional-parameters))
			(extend str-url-to-redirect (string "?" str-optional-parameters)) ; if already has parameters, use &
		)	
	))
	(print "Content-type: text/html\n") 
	(set 'Rockets:statuscode 302) ; HTTP "FOUND" redirects to a new site
   (add-header "Location" str-url-to-redirect)
	(send-headers)
	(print "\n")
	(exit)
)

;! ===== COOKIE HANDLING ========================================================

;; Function: ($COOKIES)
;; Usage: ($COOKIES "optional cookie name")
;; Returns: With no argument, returns a list of all active cookies and their values in the environment.
;; When "optional cookie name" is specified, returns the value of that cookie. 
;-----------------------------------------------------------------------------------------------------
(new Tree '$COOKIES)
(when (env "HTTP_COOKIE")
	(dolist (c (parse (env "HTTP_COOKIE") "; "))
		(let (ctemp (parse c "="))
		(if (> (length ctemp) 1) (begin
			($COOKIES (first ctemp) (join (rest ctemp) "=")))))))

;; Function: (set-cookie)
;; Usage: (set-cookie "cookie name" "cookie value" cookie-expiry-date)
;; Returns: Sets a cookie.  Due to the nature of HTML, it will not actually take effect until
;; the next page is loaded.  Cookie expiry date should be in the format of a newLISP numeric 
;; date value, so to pass in a human-readable date, use something like: (date-value 2013 2 28) 
;; for cookie-expiry-date
;-----------------------------------------------------------------------------------------------------
(define (set-cookie str-cookie-name str-cookie-value date-cookie-expire-date)
 (push (string str-cookie-name "=" str-cookie-value "; Expires=" (date date-cookie-expire-date 0 "%a, %d-%b-%Y %H:%M:%S")) Rockets:cookielist -1)
)

;; Function: (delete-cookie)
;; Usage: (delete-cookie "cookie name")
;; Returns: Deletes a cookie.  Due to the nature of HTML, it will not actually take effect until
;; the next page is loaded.  
;-----------------------------------------------------------------------------------------------------
(define (delete-cookie str-cookie-name)
 (push (string str-cookie-name "=deleted; Expires=Thu, 01-Jan-1970 00:00:01") Rockets:cookielist -1)
)

;! ===== GET AND POST FUNCTIONS =================================================

;; Function: ($GET)
;; Usage: ($GET "optional key name")
;; Returns: ($GET) on its own returns a list of all key/value pairs from the current URL.
;; Optional: ($GET "key name") returns the value for that particular key name.
;; Note: You can retrieve multiple values for the same key name by appending [] to the key name.
;; Example: URL is "page.lsp?name[]=a&name[]=b", calling ($GET "name[]") will return ("a" "b")
;-----------------------------------------------------------------------------------------------------
(new Tree '$GET)
(when (env "QUERY_STRING")
		(parse-get-or-post (env "QUERY_STRING") $GET))

(new Tree '$POST)
(define (handle-multipart-data)
	; first we have to find the boundary string
	(set 'boundary-start (+ (length "boundary=") (find "boundary=" (env "CONTENT_TYPE"))))
	(set 'boundary (slice (env "CONTENT_TYPE") boundary-start 99))
	(let ((buffer "") (binary-buffer "") (found-variables nil))
		(while (read (device) buffer $MAX_POST_LENGTH)
			(write binary-buffer buffer) ; write all the data to binary-buffer, we'll chop the rest out later
			(if (and (find "Content-Disposition:" buffer) (not found-variables)) (begin
				(set 'temp-variables (parse buffer ";"))
				(dolist (t temp-variables)
					(set 'temp-temp-name (parse t "="))					
					(if (> (length temp-temp-name) 1) (begin
						(set 'temp-name (trim (first temp-temp-name)))
						(set 'temp-value (trim (first (parse (temp-temp-name 1) "\r\n")) "\""))
						(push (list temp-name temp-value) temp-form-variable-names -1)
						(displayln "<BR>*******************NAME:" temp-name)
						(displayln "<BR>*******************VALUE:" temp-value)
					))
				)			
				(set 'found-variables true) ; we only need the first bit
				(displayln "<BR>**** TEMP VARIABLES: " temp-form-variable-names)
			))
		)
		(set 'buffer-length (length binary-buffer))
		; okay now we have to get rid of the headers
		(if (find boundary binary-buffer) (begin ; get rid of headers
				(set 'header-end (find "\r\n\r\n" binary-buffer 0 (find boundary binary-buffer)))
				(set 'binary-buffer (slice binary-buffer (+ header-end 4) (- buffer-length (+ header-end 4)))) ; chop off headers
		))
		; now we have to get rid of everything after the SECOND boundary
		(if (find boundary binary-buffer 0 header-end) (begin
			(set 'second-boundary (find boundary binary-buffer 0 header-end))
			(set 'binary-buffer (slice binary-buffer 0 (- second-boundary 4)))
		))
		; parse the other field names into ($POST)
		(if temp-form-variable-names (begin
			(dolist (t temp-form-variable-names)
				($POST (t 0) (t 1)))))
		(if binary-buffer
			($POST "binary-data" binary-buffer)) ; adding file's binary data to $POST.
		;(write-file "test2.jpg" binary-buffer)) ; testing
	) ; end (let) - temporary variables expire

)

;; Function: ($POST)
;; Usage: ($POST "optional key name")
;; Returns: ($POST) on its own returns a list of all key/value pairs from the page's POST data.
;; Optional: ($POST "key name") returns the value for that particular key name.
;; Note: You can retrieve multiple values for the same key name by appending [] to the key name.
;; Example: POST data contains "name[]=a name[]=b", calling ($POST "name[]") will return ("a" "b")
;-----------------------------------------------------------------------------------------------------
(if (and (env "CONTENT_TYPE") (starts-with (env "CONTENT_TYPE") "multipart/form-data"))
	(handle-multipart-data) ; this is for file uploads, a special case
	(let ((buffer "") (post-buffer "")) ; this is for regular forms
		(unless (zero? (peek (device)))
			(while (read (device) buffer $MAX_POST_LENGTH)
				(write post-buffer buffer))
			(parse-get-or-post post-buffer $POST)))
)
; below is the old code that had a bug that truncated long posts.  Leaving it in for now for reference.
;(when (> (peek (device)) 0)
;	(if (and (read (device) post-buffer $MAX_POST_LENGTH) post-buffer) ; grab all post data, put it in variable 'post-buffer'
;		(parse-get-or-post post-buffer $POST)
;	)
;)

;! ===== DATABASE FUNCTIONS ======================================================

(module "sqlite3.lsp") ; loads the SQLite3 database module

;; Function: (open-database)
;; Usage: (open-database "database name")
;; Returns: Opens a SQLite3 database.  A ".db" extension will be added automatically to the name.
;; Note: Only one database can be open at a time.  You will need to close one database to open another.
;-----------------------------------------------------------------------------------------------------
(define (open-database sql-db-to-open)
	(if (sql3:open (string sql-db-to-open ".db"))  
		(displayln "")
		(displayln "There was a problem opening the database " sql-db-to-open ": " (sql3:error))))

;; Function: (close-database)
;; Usage: (open-database)
;; Returns: Closes the currently open database.
;-----------------------------------------------------------------------------------------------------
(define (close-database)
	(if (sql3:close)
		(displayln "")
		(displayln "There was a problem closing the database: " (sql3:error))))

;====== SAFE-FOR-SQL ===============================================================
; this function makes strings safe for inserting into SQL statements
; to avoid SQL injection issues
; it's simple right now but will add to it later
;===================================================================================
(define (safe-for-sql str-sql-query)
	(if (string? str-sql-query) (begin
		(replace "&" str-sql-query "&amp;")
		(replace "'" str-sql-query "&apos;")
		(replace "\"" str-sql-query "&quot;")
		))
		(set 'result str-sql-query))

;; Function: (query)
;; Usage: (query "SQL text")
;; This function sends a straight SQL query to the currently open database
(define (query sql-text)
 (set 'sqlarray (sql3:sql sql-text))    ; results of query
 (if sqlarray
   (setq query-return sqlarray)
		(if (sql3:error)
			(displayln (sql3:error) " query problem ")
			(setq query-return nil))))

;; Function: (create-record) 
;; Usage: (create-record "Table Name" ColumnName1 ColumnName2 ColumnName3 ...)
;; Returns: true if creation was successful, otherwise displays the SQL error
;; The way this works it that your variable names need to be the same as the column names
;; in the database.  Enforcing this makes the code simpler and easier to read.  Set the values
;; before calling (create-record).  When it is called, the values of each column will be set in a 
;; new record in the database.
;; Note: Variables need to be either integers or strings, depending on the type of column.
;; Note: Date columns need to be strings using the SQL Date format: "YYYY-MM-DD HH:MM:SS.000"
;-----------------------------------------------------------------------------------------------------
(define-macro (create-record)
	; first save the values
	(set 'temp-record-values nil)
	(set 'temp-table-name (first (args)))
	;(displayln "<BR>Arguments: " (args))
	(dolist (s (rest (args))) (push (eval s) temp-record-values -1))
	; now save the arguments as symbols under the context "DB"
	(dolist (s (rest (args)))
		(set 'temp-index-num (string $idx)) ; we need to number the symbols to keep them in the correct order
		(if (= (length temp-index-num) 1) (set 'temp-index-num (string "0" temp-index-num))) ; leading 0 keeps the max at 100.
		(sym (string temp-index-num s) 'DB))
	; now create the sql query 
	(set 'temp-sql-query (string "INSERT INTO " temp-table-name " ("))
	;(displayln "<P>TABLE NAME: " temp-table-name)
	;(displayln "<P>SYMBOLS: " (symbols DB))
	;(displayln "<BR>VALUES: " temp-record-values)
	(dolist (d (symbols DB)) (extend temp-sql-query (rest (rest (rest (rest (rest (string d)))))) ", "))
	(set 'temp-sql-query (chop (chop temp-sql-query)))
	(extend temp-sql-query ") VALUES (")
	(dolist (q temp-record-values)
		(if (string? q) (extend temp-sql-query "'")) ; only quote if value is non-numeric
		(extend temp-sql-query (string (safe-for-sql q)))
		(if (string? q) (extend temp-sql-query "'")) ; close quote if value is non-numeric
		(extend temp-sql-query ", ")) ; all values are sanitized to avoid SQL injection
	(set 'temp-sql-query (chop (chop temp-sql-query)))
	(extend temp-sql-query ");")
	;(displayln "<p>***** SQL QUERY: " temp-sql-query)
	(displayln (query temp-sql-query)) ; actually run the query against the database
	(delete 'DB) ; we're done, so delete all symbols in the DB context.
)	

;; Function: (update-record) 
;; Usage: (update-record "Table Name" ConditionColumn ColumnName1 ColumnName2 ColumnName3 ...)
;; Returns: true if update was successful, otherwise displays the SQL error
;; The way this works it that your variable names need to be the same as the column names
;; in the database.  Enforcing this makes the code simpler and easier to read.  Set the values
;; before calling (update-record).  When it is called, the values of each column will be set to those values.
;; Note: The variable "ConditionColumn" will check to see if the column equals that value
;; Example: (update-record "Posts" Id Subject Content) will update the Subject and Content columns
;; for all records where Id is equal to the value of the variable Id.
;; Note: Variables need to be either integers or strings, depending on the type of column.
;; Note: Date columns need to be strings using the SQL Date format: "YYYY-MM-DD HH:MM:SS.000"
;-----------------------------------------------------------------------------------------------------
(define-macro (update-record)
	; first save the values
	(set 'temp-record-values nil)
	(set 'temp-table-name (first (args)))
	(set 'continue true) ; debugging
	(dolist (s (rest (args))) (push (eval s) temp-record-values -1))
	; now save the arguments as symbols under the context "D2"
	(dolist (st (rest (args)))
		(set 'temp-index-num (string $idx)) ; we need to number the symbols to keep them in the correct order
		(if (= (length temp-index-num) 1) (set 'temp-index-num (string "0" temp-index-num))) ; leading 0 keeps the max at 100.
		;(displayln "<br>SYMBOL>>>>" (string temp-index-num st) "<<<") ; debugging
		(sym (string temp-index-num st) 'D2)
	)
	(if continue (begin ; --- temporary debugging
	; now create the sql query 
	(set 'temp-sql-query (string "UPDATE " temp-table-name " SET "))
	;(displayln "<P>TABLE NAME: " temp-table-name)
	;(displayln "<P>SYMBOLS: " (symbols D2))
	;(displayln "<BR>VALUES: " temp-record-values)
	(dolist (d (rest (symbols D2))) ; ignore the first argument, as it will be the ConditionColumn for later
		(extend temp-sql-query (rest (rest (rest (rest (rest (string d)))))) "=")
		(set 'q (temp-record-values (+ $idx 1)))
		(if (string? q) (extend temp-sql-query "'")) ; only quote if value is non-numeric
		(extend temp-sql-query (string (safe-for-sql q)))
		(if (string? q) (extend temp-sql-query "'")) ; close quote if value is non-numeric
		(extend temp-sql-query ", ") ; all values are sanitized to avoid SQL injection
	)	
	(set 'temp-sql-query (chop (chop temp-sql-query)))
	; okay now add the ConditionColumn value
	(extend temp-sql-query (string " WHERE " (rest (rest (rest (rest (rest (string (first (symbols D2)))))))) "="))
	(if (string? (first temp-record-values)) (extend temp-sql-query "'"))
	(extend temp-sql-query (string (safe-for-sql (first temp-record-values))))
	(if (string? (first temp-record-values)) (extend temp-sql-query "'"))
	(extend temp-sql-query ";")
	;(displayln "<p>***** SQL QUERY: " temp-sql-query)
	(query temp-sql-query) ; actually run the query against the database
	(delete 'D2) ; we're done, so delete all symbols in the DB context.
	)) ; --- end temporary debugging
)	

;; Function: (delete-record) 
;; Usage: (delete-record "Table Name" ColumnName1)
;; Returns: true if deletion was successful, otherwise displays the SQL error
;; The variable ColumnName1 needs to be assigned a value that will be checked to qualify the deletion
;; Example: (set 'Email "bob@bob.com") (delete-record "Posts" Email) will delete all records where
;; the column name "Email" is equal to "bob@bob.com".  
;; Note: Variables need to be either integers or strings, depending on the type of column.
;; Note: Date columns need to be strings using the SQL Date format: "YYYY-MM-DD HH:MM:SS.000"
;-----------------------------------------------------------------------------------------------------
(define-macro (delete-record)
	(set 'temp-table-name (first (args)))
	(set 'temp-record-values nil)
	(dolist (s (rest (args))) (push (eval s) temp-record-values -1)) ; only one value for NOW...
	(sym (first (rest (args))) 'DB) ; put the second argument (for now) into a symbol in the DB context
												; this will have to be in a dolist loop of (rest (args)) when I add more
	(set 'temp-sql-query (string "DELETE FROM " temp-table-name " WHERE "))
	(dolist (d (symbols DB)) (extend temp-sql-query (rest (rest (rest (string d))))))
	(extend temp-sql-query "=")
	; why am I doing a loop here?  There should be only one value, right?  But maybe for future extension...
	(dolist (q temp-record-values)
		(if (string? q) (extend temp-sql-query "'")) ; only quote if value is non-numeric
		(extend temp-sql-query (string (safe-for-sql q)))
		(if (string? q) (extend temp-sql-query "'"))) ; close quote if value is non-numeric
	(extend temp-sql-query ";")
	;(displayln "TEMP-DELETE-QUERY: " temp-sql-query)	
	(query temp-sql-query)
	(delete 'DB) ; we're done, so delete all symbols in the DB context.
)

;; Function: (get-record) 
;; Usage: (get-record "Table Name" ColumnName1)
;; Returns: A list of all values for the record if successful, otherwise displays the SQL error
;; The variable ColumnName1 needs to be assigned a value that will be checked to qualify the deletion
;; Example: (set 'Email "bob@bob.com") (get-record "Posts" Email) will retrieve all records where
;; the column name "Email" is equal to "bob@bob.com".  
;; Note: Variables need to be either integers or strings, depending on the type of column.
;; Note: Date columns need to be strings using the SQL Date format: "YYYY-MM-DD HH:MM:SS.000"
;-----------------------------------------------------------------------------------------------------
(define-macro (get-record)
	(set 'temp-table-name (first (args)))
	; if you have more arguments than just the table name, they become the elements of the WHERE clause
	(if (> (length (args)) 1) (begin
		(set 'temp-record-values nil)
		(dolist (s (rest (args))) (push (eval s) temp-record-values -1)) ; only one value for NOW...
		(sym (first (rest (args))) 'DB) ; put the second argument (for now) into a symbol in the DB context
													; this will have to be in a dolist loop of (rest (args)) when I add more
		(set 'temp-sql-query (string "SELECT * FROM " temp-table-name " WHERE "))
		(dolist (d (symbols DB)) (extend temp-sql-query (rest (rest (rest (string d))))))
		(extend temp-sql-query "=")
		; why am I doing a loop here?  There should be only one value, right?  But maybe for future extension...
		(dolist (q temp-record-values)
			(if (string? q) (extend temp-sql-query "'")) ; only quote if value is non-numeric
			(extend temp-sql-query (string (safe-for-sql q)))
			(if (string? q) (extend temp-sql-query "'"))) ; close quote if value is non-numeric
		(extend temp-sql-query ";")
	)
		; otherwise, just get everything in that table
		(set 'temp-sql-query (string "SELECT * FROM " temp-table-name ";"))
	)
	;(displayln "TEMP-GET-QUERY: " temp-sql-query)	
	(delete 'DB) ; we're done, so delete all symbols in the DB context.
	(set 'return-value (query temp-sql-query)) ; this returns a list of everything in the record
)

;! ===== FORM AND TABLE FUNCTIONS =========================================================================

;; Function: (display-post-box)
;; Usage: (display-post-box "Title" "Form Name" "page-to-submit" "Subject Line ID" "Postbox ID" "Submit Button Text" "optional linkback value" "optional text to pre-populate subject line" "optional text to pre-populate post box" "optional hidden value")
;; Returns: Displays a form with a subject line and a text box, and a submit button.  
;; The form will enter information into POST and redirect to "page-to-submit.lsp" when Submit is clicked.
;; Note: The .lsp extension is optional.  If it is not entered, it will be added automatically.
;; Note: You can pre-populate the post box (for example, for editing an existing post) by adding the last two optional parameters.
;; Note: You can hide the Subject Line text box by simply entering nil (no quotes) as the subject line.
;; Note: The "optional linkback value" parameter sets a hidden field named "linkbackid" in the form and sets it to that value.
;; Note: I've added another "optional hidden value" that sets a hidden field named "optionalhidden" in the form and sets it to that value.
;; If this becomes a trend, I might just turn hidden values into a list, which would be cleaner, but we'll leave it for now.
;; This is useful for when you want the page that is called via the submit button to remember the Id # of, for example,
;; which post you were editing or just added.
;-----------------------------------------------------------------------------------------------------
(define (display-post-box str-title str-form-name str-action-page str-subject-line str-postbox-id str-submit-button-text str-linkback-id str-optional-subject-value str-optional-post-value str-optional-hidden-value)
	(displayln "<h3>" str-title "</h3>")
	(if (not (find ".lsp" str-action-page)) (extend str-action-page ".lsp"))
	(displayln "<form name='" str-form-name "' METHOD='POST' action='" str-action-page "'>")
	; can either show or hide subject line if you enter a value for str-subject-line
	(if str-subject-line 
		(display "<input type='text' class='field span5' name='" str-subject-line "' ")
		(display "<input type='hidden' class='field span5' name='subjectline' "))
	(if str-optional-subject-value 
		(display "value='" str-optional-subject-value "'>")
		(display "value=''>"))
	(display "<p><textarea name='post' id='" str-postbox-id "' class='field span9' rows='12'>")
	(if str-optional-post-value (display str-optional-post-value))
	(displayln "</textarea>")
	; now add the two hidden values, if they exist
	(if str-linkback-id (displayln "<input type='hidden' name='linkbackid' value='" str-linkback-id "'>"))
	(if str-optional-hidden-value (displayln "<input type='hidden' name='optionalhidden' value='" str-optional-hidden-value "'>"))
	(displayln "<br><p><input type='submit' class='btn' value='" str-submit-button-text "'>")
	(displayln "</form>"))

;; Function: (form-datepicker)
;; Usage: (form-datepicker "Description text" "form-item-name" "form-initial-value" "form-id")
;; Example: (form-datepicker "Enter a date" "date" "03-16-2072" "dp1")
;; Returns: Displays a date field with a dropdown calendar when the user clicks on the field.
;; Note: Requires js/boostrap-datepicker.js and css/datepicker.css to work
;-----------------------------------------------------------------------------------------------------
(define (form-datepicker form-text form-item-name form-initial-value form-id)
	(displayln "<p>" form-text ": <input type='text' class='span2' name='" form-item-name "' value='" form-initial-value "' id='" form-id "'>")
	(push form-id $FORM-DATEPICKER -1) ; this is a global list needed to load the appropriate jQuery at the end
)

;; Function: (display-table)
;; Usage: (display-table list-of-headers nested-list-of-data "optional form styling" '((optional) (nested) (list) (of) (links)))
;; Example: (display-table '("First" "Last" "Email") '(("Joe" "McBain" "joe@joe.com") ("Bob" "McBain" "bob@bob.com")) "striped")
;; Returns: Displays a table with headers.  If no headers are provided (entered as nil) they will not display  
;; Note: Styling options are as follows:
;; striped - alternates rows in grey
;; bordered - adds borders and rounded corners to the table
;; hover - enables hover state on table rows when mousing over
;; condensed - more condensed style of table
;-----------------------------------------------------------------------------------------------------
(define (display-table list-of-headers nested-list-of-data str-optional-styling list-optional-links)
	(display "<table class=\"table")
	(if str-optional-styling (display " table-" str-optional-styling))
	(displayln "\">")
	(if list-of-headers (begin
		(displayln "<thead>")
		(displayln "<tr>")
		(dolist (th list-of-headers) 
			(displayln "<th>" th "</th>"))
		(displayln "</tr>")
		(displayln "</thead>")))
	(displayln "<tbody>")
	(if nested-list-of-data (begin
		(dolist (tr nested-list-of-data)
			(set 'temp-row-num $idx)
			(displayln "<tr>")
			(dolist (td tr)
				(display "<td>")
				(if list-optional-links (begin
					(if (list-optional-links temp-row-num) (begin ; this is in case there is an entire row of 'nil' in the list
						(if (list-optional-links temp-row-num $idx) (begin
							(display "<a href='" (list-optional-links temp-row-num $idx) "'>" td "</a>"))
							(display td))
						) ; -- end row of nil case
						(display td)
					)) ; -- end optional links existing case
					(display td))
				(displayln "</td>"))
			(displayln "</tr>"))))
	(displayln "</tbody>")
	(displayln "</table>")
)


;! ===== SOCIAL MEDIA AND EMAIL FUNCTIONS =====================================================
; Twitter functions (lifted from Dragonfly.. will rewrite later)

;; Function: (twitter-search)
;; Usage: (twitter-search "Key words" 10)
;; Returns: Displays the results of a Twitter search for the key words or phrases entered.
;; Optional: Add an integer to specify the maximum number of items to show
;-----------------------------------------------------------------------------------------
(define (twitter-search keyword max-items)
	(set 'xml (get-url (string "http://search.twitter.com/search.atom?rpp="max-items"&q="keyword) ))
	(xml-type-tags nil nil nil nil) ; no extra tags
	(set 'sxml (xml-parse xml 31)) ; turn on SXML options
	(set 'entry-index (ref-all '(entry *) sxml match))
	(when (empty? entry-index)
		(displayln "No entries found")
	)
	(dolist (idx entry-index)
		(set 'entry (sxml idx))
		(set 'dateseconds (parse-date (lookup 'published entry) "%Y-%m-%dT%H:%M:%SZ")) ; convert string date to seconds
		
		(displayln
			"<div class='row-fluid'>"
			"<h4>" (lookup 'title entry) "</h4><br/>"
			"<div class='thumbnail'>" (date dateseconds 0 "%a %d %b %Y %H:%M:%S") "</div><div class='author'>By&nbsp;" (lookup '(author name) entry) "</div><br/>"
			"</div>"
		)
	)
)

;; Function: (send-mail)
;; Usage: (send-mail "to@address.com" "from@address.com" "Real Sender Name" "Subject line" "Body text")
;; Returns: Sends email to any address from any address.  To avoid getting caught in spam
;; filters, use a valid From: email address with the same domain as your web site.
;; Note: Using this function requires that sendmail is installed on your system.
;; Install sendmail using the Linux (Ubuntu or Debian) command "sudo apt-get install sendmail"
;; Note: This function sets the appropriate email headers automatically.
;-----------------------------------------------------------------------------------------
(define (send-mail to from realname subject body)
   (exec (string "/usr/sbin/sendmail -t -f " from)
    (format "To: %s\nFrom: %s <%s>\nSubject: %s\nContent-Type: text/html; charset=iso-8859-1\n\n%s"
       to realname from subject body)))
