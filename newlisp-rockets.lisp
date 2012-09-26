; Newlisp on Rockets framework
; ----------------------------
;
; Version 0.09
;
; For revision history, see revision-history.txt
;
; I have to give a HUGE acknowledgement to the Dragonfly framework at http://rundragonfly.com
; written by Marc Hildmann and Greg Slepak and available for download at: http://code.google.com/p/dragonfly-newlisp/downloads/list
; Without Dragonfly I would have been unable to do Rockets at all.  Some parts of Rockets contain
; snippets of Dragonfly code.  Also thanks to Lutz Mueller, the author of newLISP at http://www.newlisp.org 
;
; Written 2012 by Rocket Man

;------------------------------------------------------------------------------------------------------------

;====== GLOBAL VARIABLES ========================================================
(constant (global '$ROCKETS_VERSION) 0.09)    ; this is the current version of Rockets
(constant (global '$MAX_POST_LENGTH) 1048576) ; the maximum size data you can POST.

;====== CONSTANTS ================================================================
(constant 'REGEX_HTTP_SPECIAL_STR (regex-comp {([^.0-9a-z]+)} 1))
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))

; This is the global buffer that Rockets writes to before printing the page out at the end
(set 'STDOUT "")

; Now we redefine (print) and (displayln) to store to the variable STDOUT
(define (displayln)
	(apply display (push "\n" (args) -1)))

(define (display)
	(extend STDOUT (apply string $args))
	(last $args)) ; to behave the same way as print

;; BENCHMARK STUFF===================================================================================
;  this was taken from Dragonfly, but just too cool not to use!
; ===================================================================================================

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



;====== HEADER ===================================================================
(define (display-header)
	(displayln "<html><head><meta charset=\"UTF-8\">")
   (displayln "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
	(displayln "<link href=\"css/bootstrap.css\" rel=\"stylesheet\">") ; loads Bootstrap CSS
	(displayln "<link href=\"css/bootstrap-responsive.css\" rel=\"stylesheet\"></head><body>")
)

;====== NAVBAR =========================================================================
; this is a Bootstrap navigation bar that stays at the top of the screen when you scroll
; calling this function also sets up the main <div> container for the whole page.
(define (display-navbar str-name list-menus)
	(displayln "    <div class=\"navbar navbar-inverse navbar-fixed-top\">")
	(displayln "      <div class=\"navbar-inner\">")
	(displayln "        <div class=\"container\">")
	(displayln "          <a class=\"btn btn-navbar\" data-toggle=\"collapse\" data-target=\".nav-collapse\">")
	(displayln "            <span class=\"icon-bar\"></span>")
	(displayln "            <span class=\"icon-bar\"></span>")
	(displayln "            <span class=\"icon-bar\"></span>")
	(displayln "          </a>")
	(displayln "          <a class=\"brand\" href=\"#\">" str-name "</a>")
	(displayln "          <div class=\"nav-collapse collapse\">")
	(displayln "            <ul class=\"nav\">")
	(displayln "              <li class=\"active\"><a href=\"#\">Home</a></li>")
	(dolist (d list-menus)
		(displayln "              <li><a href=\"#" (lower-case d) "\">" d "</a></li>")	)
	(displayln "            </ul>")
	(displayln "            <form class=\"navbar-form pull-right\">")
	(displayln "              <input class=\"span2\" type=\"text\" placeholder=\"Email\">")
	(displayln "              <input class=\"span2\" type=\"password\" placeholder=\"Password\">")
	(displayln "              <button type=\"submit\" class=\"btn\">Sign in</button>")
	(displayln "            </form>")
	(displayln "          </div>")
	(displayln "       </div>")
   (displayln "     </div>")
	(displayln "    </div>")
	(displayln " <div class=\"container\">") ; start the main container for the page
)


;====== FOOTER ===================================================================
(define (display-footer str-company-name)
	(displayln "<hr><footer><p>&copy; " (date (date-value) 0 "%Y") " " str-company-name ". ") ; always prints current year
	(displayln "<script src=\"js/jquery-1.8.2.min.js\"></script>") ; Loads jQuery
	(displayln "<script src=\"js/bootstrap.min.js\"></script>") ; Loads Bootstrap Javascript.
	(displayln (benchmark-result) "</footer></div>") ; ends main container
	(displayln "</body></html>"))

;====== START-DIV ===================================================================
; okay this is pretty simple, admittedly
(define (start-div str-div-name)
	(displayln "<div class=\"" str-div-name "\">"))

;====== END-DIV ===================================================================
; this might be more exciting in the future
(define (end-div)
	(displayln "</div>"))

;====== PRINT-IMAGE ==============================================================
(define (display-image str-image-to-print)
	(displayln (string "<img src=images/" str-image-to-print ">")))

;====== URL-DECODE =================================================================
; this function takes encoded strings and returns them to human-readable
; the regex and code were adapted from Dragonfly.  Basically %2B->"+", %27->"'", etc
;===================================================================================
(define (url-decode str-to-decode)
	(replace "+" str-to-decode " ")
	(replace REGEX_HEX_ENCODED_CHAR str-to-decode (pack "b" (int $1 nil 16)) 0x10000))

;====== FORMAT-FOR-WEB =============================================================
; this is a simple function to take a post full of data including carriage return/lf
; and make it print properly on the web
;===================================================================================
(define (format-for-web str-input-for-web)
	(replace "\r\n" str-input-for-web "<BR>"))

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

;================================================================================
;  ($COOKIES)
;================================================================================
; sets a global variable with all the cookies placed as key/value pairs
(new Tree '$COOKIES)
(when (env "HTTP_COOKIE")
	(dolist (c (parse (env "HTTP_COOKIE") "; "))
		(let (ctemp (parse c "="))
		(if (> (length ctemp) 1) (begin
			($COOKIES (first ctemp) (join (rest ctemp) "=")))))))

;================================================================================
;  ($GET)
;================================================================================
; takes anything on the URL line and adds it to some sort of global GET hash
; works for single value and multivalue with [] 
(new Tree '$GET)
(when (env "QUERY_STRING")
		(parse-get-or-post (env "QUERY_STRING") $GET))

;================================================================================
;  ($POST)
;================================================================================
; this is just for multipart form data sent via POST method right now
; will modify it for files and binary stuff later
; has some sort of weird random limit now unless you refresh, working on fixing that

(new Tree '$POST)
;(when (> (peek (device)) 0)
	(read (device) post-buffer $MAX_POST_LENGTH) ; grab all post data, put it in variable 'post-buffer'
	(if post-buffer (begin
		(displayln "POST BUFFER: " post-buffer " LENGTH: " (length post-buffer)) ; <- debugging
		(parse-get-or-post post-buffer $POST)
	))
;)

;=================================================================================
;  SQLITE 3 DATABASE FUNCTIONS BELOW
;; just loads sqlite module and defines functions
;=================================================================================

(module "sqlite3.lsp") ; load up the database

;; open a database
(define (open-database sql-db-to-open)
	(if (sql3:open sql-db-to-open)  
		(displayln "")
		(displayln "There was a problem opening the database " sql-db-to-open ": " (sql3:error))))

;; close the database
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

;; function for doing queries
(define (query sql-text)
 (set 'sqlarray (sql3:sql sql-text))    ; results of query
 (if sqlarray
   (setq query-return sqlarray)
		(if (sql3:error)
			(displayln (sql3:error) " query problem ")
			(setq query-return nil))))

;====== CREATE-RECORD ======================================================================================
;;  create record! The C in CRUD!  It's a little bit janky (like I can't call a macro with a macro)
;;  but I think it's neat. The idea is that your variable NAMES are in fact the same as the field names
;;  in the database.  These names are converted to symbols so we can reference both the names and values
;;  when constructing the SQL statement.  Also, all entries are run through (safe-for-sql) to make them safe
;;  from SQL injection hacks.
;===========================================================================================================
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

;====== DELETE-RECORD ======================================================================================
; format: (delete-record "TableName" ColumnVariable)
; 
; obviously this is for a very limited subset of deleting, but it may be extended later
; NOTE: Since the variable name is the same as the column name, you have to make sure to make it
; integer if the column is integer, otherwise the SQL WHERE query will look for a string, and it will
; fail.  Since newLISP has dynamic typing, we can't enforce the right type 
;===========================================================================================================
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

;----- Form functions----------------------------------------------------------

;===============================================================================
; DISPLAY-POST-BOX
; Just a simple function to print a form for entering posts (subject and postbox)
; (print-post-box Title FormName ActionPage SubjectLine PostboxID SubmitButton)
;===============================================================================
(define (display-post-box str-title str-form-name str-action-page str-subject-line str-postbox-id str-submit-button-text)
	(displayln "<h3>" str-title "</h3>")
	(displayln "<form name='" str-form-name "' METHOD='POST' action='" str-action-page "'>")
	(displayln "<input type='text' name='" str-subject-line "'>")
	(displayln "<p><textarea name='post' id='" str-postbox-id "' cols='50' rows='10'></textarea>")
	(displayln "<input type='submit' value='" str-submit-button-text "'>")
	(displayln "</form>"))

;===============================================================================
; !twitter functions (lifted from Dragonfly.. will rewrite later)
;===============================================================================

;; @syntax (Dragonfly:twitter-search <keyword> <max-items>)
;; @param <keyword> string containing the keyword for search
;; @param <max-items> INTEGER, maximum of items you want to show
;; <p>Writes the results of a Twitter search.</p>

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
			"<div class='entry'>"
			"<div class='headline'>" (lookup 'title entry) "</div><br/>"
			"<div class='published'>" (date dateseconds 0 "%a %d %b %Y %H:%M:%S") "</div><div class='author'>By&nbsp;" (lookup '(author name) entry) "</div><br/>"
			"</div>"
		)
	)
)

(define (display-page)
	; Sending the page starts here-----------------------------------------------------------------------------
	; print headers
	(print "Content-type: text/html\n") 
	; send cookies if they exist 
	(print "Set-Cookie: name2=value2; Expires=Wed, 09-Jun-2021 10:18:14 GMT\n")
	(print "\n")
	(print "<!DOCTYPE html>") 	
	(print STDOUT) ; the whole page gets put here
)

;(print "POST: " ($POST) " COOKIES: " ($COOKIES))