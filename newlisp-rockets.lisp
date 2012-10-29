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

;!===== GLOBAL VARIABLES ========================================================
;;* $ROCKETS_VERSION - current version of Rockets
(constant (global '$ROCKETS_VERSION) 0.16)    
;;* $MAX_POST_LENGTH - maximum size of data you are allowed to POST
(constant (global '$MAX_POST_LENGTH) 1048576) 
;;* $PARTIAL_PATH - the relative path for when using (display-partial)
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

; This is a shortcut to reading in files in the $PARTIAL_PATH subdirectory
(define (display-partial partialname)
  	(display-file (string $PARTIAL_PATH "/" partialname ".lsp")))

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
;-----------------------------------------------------------------------------------------------------
(define (format-for-web str-input-for-web)
	(replace "\r\n" str-input-for-web "<BR>"))

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
	(displayln "<link href=\"css/bootstrap.css\" rel=\"stylesheet\">") ; loads Bootstrap CSS
	(displayln "<link href=\"css/bootstrap-responsive.css\" rel=\"stylesheet\">")
	(if str-optional-css (displayln "<link href=\"css/" str-optional-css ".css\" rel=\"stylesheet\">"))
	(displayln "<style> body { padding-top: 60px; /* fixes the container spacing */   }</style>")
	(displayln "</head><body data-spy=\"scroll\" data-target=\".bs-docs-sidebar\">")
)

;; Function: (display-navbar)
;; Usage: (display-header "Site name" '(list of menus) "page-to-go-for-signing-in")
;; Returns: Prints the top navigation bar with menus and form for signing in. Also, 
; calling this function also sets up the main <div> container for the whole page.
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
	(displayln "          <a class=\"brand\" href=\"\">" str-name "</a>")
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
		(displayln "                  <ul class=\"dropdown-menu\"><li><a href=\"#\">Edit Profile</a></li>")
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
	(displayln " <div class=\"container\">") ; start the main container for the page
)

;; Function: (display-footer)
;; Usage: (display-footer "Optional Company Name")
;; Returns: Prints the footer with benchmark result.  Also loads Javascript libraries.
;-----------------------------------------------------------------------------------------------------
(define (display-footer str-company-name)
	(if (nil? str-company-name) (set 'str-company-name ""))
	(displayln "<hr><footer><p>&copy; " (date (date-value) 0 "%Y") " " str-company-name ". ") ; always prints current year
	(displayln "<script src=\"js/jquery-1.8.2.min.js\"></script>") ; Loads jQuery
	(displayln "<script src=\"js/bootstrap.min.js\"></script>") ; Loads Bootstrap Javascript.
	(displayln (benchmark-result) "</footer></div>") ; ends main container
	(displayln "</body></html>"))

;; Function: (display-image)
;; Usage: (display-image "imagename.jpg" 200 100)
;; Returns: Displays an image from the default /images/ subdirectory.  Width and height are optional  
;-----------------------------------------------------------------------------------------------------
(define (display-image str-image-to-print int-width int-height)
	(display (string "<img src=images/" str-image-to-print))
	(if int-width (display (string " width=" int-width)))
	(if int-height (display (string " height=" int-height)))
	(displayln ">"))

;; Function: (display-paging-links)
;; Usage: (display-paging-links 1 99 2 "page-url")
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
;; Usage: (display-error)
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
;; Usage: (page-redirect "page-to-redirect-to")
;; Loads a new URL immediately when executed.  The current page will not be displayed to the user.
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

;; Function: ($POST)
;; Usage: ($POST "optional key name")
;; Returns: ($POST) on its own returns a list of all key/value pairs from the page's POST data.
;; Optional: ($POST "key name") returns the value for that particular key name.
;; Note: You can retrieve multiple values for the same key name by appending [] to the key name.
;; Example: POST data contains "name[]=a name[]=b", calling ($POST "name[]") will return ("a" "b")
;-----------------------------------------------------------------------------------------------------
(new Tree '$POST)
;(when (> (peek (device)) 0)
	(read (device) post-buffer $MAX_POST_LENGTH) ; grab all post data, put it in variable 'post-buffer'
	(if post-buffer (begin
		;(displayln "POST BUFFER: " post-buffer " LENGTH: " (length post-buffer)) ; <- debugging
		(parse-get-or-post post-buffer $POST)
	))
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
	(if (string? str-sql-query) 
		(replace "'" str-sql-query "&apos;"))
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

;; Function: (delete-record) 
;; Usage: (delete-record "Table Name" ColumnName1)
;; Returns: true if deletion was successful, otherwise displays the SQL error
;; The variable ColumnName1 needs to be assigned a value that will be checked to qualify the deletion
;; Example: (set 'Email "bob@bom.com") (delete-record "Posts" Email) will delete all records where
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
;; Example: (set 'Email "bob@bom.com") (get-record "Posts" Email) will retrieve all records where
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

;! ===== FORM FUNCTIONS =========================================================================

;; Function: (display-post-box)
;; Usage: (display-post-box "Title" "Form Name" "page-to-submit" "Subject Line" "Postbox ID" "Submit Button Text")
;; Returns: Displays a form with a subject line and a text box, and a submit button.  
;; The form will enter information into POST and redirect to "page-to-submit.lsp" when Submit is clicked.
;; Note: The .lsp extension is optional.  If it is not entered, it will be added automatically.
;; Note: You can hide the Subject Line text box by simply entering nil (no quotes) as the subject line.
;-----------------------------------------------------------------------------------------------------
(define (display-post-box str-title str-form-name str-action-page str-subject-line str-postbox-id str-submit-button-text str-linkback-id)
	(displayln "<h3>" str-title "</h3>")
	(if (not (find ".lsp" str-action-page)) (extend str-action-page ".lsp"))
	(displayln "<form name='" str-form-name "' METHOD='POST' action='" str-action-page "'>")
	; can either show or hide subject line if you enter a value for str-subject-line
	(if str-subject-line 
		(displayln "<input type='text' class='field span5' name='" str-subject-line "'>")
		(displayln "<input type='hidden' class='field span5' name='subjectline' value='no subject'>"))
	(displayln "<p><textarea name='post' id='" str-postbox-id "' class='field span9' rows='12'></textarea>")
	(if str-linkback-id (displayln "<input type='hidden' name='linkbackid' value='" str-linkback-id "'>"))
	(displayln "<br><p><input type='submit' class='btn' value='" str-submit-button-text "'>")
	(displayln "</form>"))


;! ===== SOCIAL MEDIA FUNCTIONS =====================================================
; Twitter functions (lifted from Dragonfly.. will rewrite later)
;===============================================================================

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


