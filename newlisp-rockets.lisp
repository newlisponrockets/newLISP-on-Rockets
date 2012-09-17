; Newlisp on Rockets framework
; ----------------------------
;
; Version 0.06 - first standalone framework
;
; Revision history:
; 0.01 - first standalone framework without Dragonfly
; 0.02 - added ($GET) functionality
; 0.03 - 09/10/2012 - added ($POST) functionality (multipart forms only)
; 0.04 - 09/11/2012 - added SQLite database open/close and query functions
; 0.05 - 09/14/2012 - added (create-record) macro to add a new record to SQLite table
; 0.06 - 09/17/2012 - added (delete-record) macro to delete a record from SQLite table
;
; I have to give a HUGE acknowledgement to the Dragonfly framework at http://rundragonfly.com
; written by Marc Hildmann and Greg Slepak and available for download at: http://code.google.com/p/dragonfly-newlisp/downloads/list
; Without Dragonfly I would have been unable to do Rockets at all.  Some parts of Rockets contain
; snippets of Dragonfly code.  Also thanks to Lutz Mueller, the author of newLISP at http://www.newlisp.org 
;
; Written 2012 by Rocket Man

; print headers
(print "Content-type: text/html\n\n") 
(print "<!DOCTYPE html>\n\n") 

;====== GLOBAL VARIABLES ========================================================
(constant (global '$ROCKETS_VERSION) 0.06)    ; this is the current version of Rockets
(constant (global '$MAX_POST_LENGTH) 1048576) ; the maximum size data you can POST.

;====== CONSTANTS ================================================================
(constant 'REGEX_HTTP_SPECIAL_STR (regex-comp {([^.0-9a-z]+)} 1))
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))

;====== HEADER ===================================================================
(define (print-header)
	(println "<html><head><meta charset=\"UTF-8\"></head><body>"))

;====== FOOTER ===================================================================
(define (print-footer)
	(println "</body></html>"))

;====== PRINT-IMAGE ==============================================================
(define (print-image str-image-to-print)
	(println (string "<img src=images/" str-image-to-print ">")))

;====== URL-DECODE =================================================================
; this function takes encoded strings and returns them to human-readable
; the regex and code were adapted from Dragonfly.  Basically %2B->"+", %27->"'", etc
;===================================================================================
(define (url-decode str-to-decode)
	(replace "+" str-to-decode " ")
	(replace REGEX_HEX_ENCODED_CHAR str-to-decode (pack "b" (int $1 nil 16)) 0x10000))

;====== SAFE-FOR-SQL ===============================================================
; this function makes strings safe for inserting into SQL statements
; to avoid SQL injection issues
; it's simple right now but will add to it later
;===================================================================================
(define (safe-for-sql str-sql-query)
	(if (string? str-sql-query) 
		(replace "'" str-sql-query "&apos;"))
		(set 'result str-sql-query))

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
		(println "POST BUFFER: " post-buffer " LENGTH: " (length post-buffer)) <- debugging
		(parse-get-or-post post-buffer $POST)))
;)

;=================================================================================
;  SQLITE 3 DATABASE FUNCTIONS BELOW
;; just loads sqlite module and defines functions
;=================================================================================

(module "sqlite3.lsp") ; load up the database

;; open a database
(define (open-database sql-db-to-open)
	(if (sql3:open sql-db-to-open)  
		(println "")
		(println "There was a problem opening the database " sql-db-to-open ": " (sql3:error))))

;; close the database
(define (close-database)
	(if (sql3:close)
		(println "")
		(println "There was a problem closing the database: " (sql3:error))))

;; function for doing queries
(define (query sql-text)
 (set 'sqlarray (sql3:sql sql-text))    ; results of query
 (if sqlarray
   (setq query-return sqlarray)
		(if (sql3:error)
			(println (sql3:error) " query problem ")
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
	;(println "<BR>Arguments: " (args))
	(dolist (s (rest (args))) (push (eval s) temp-record-values -1))
	; now save the arguments as symbols under the context "DB"
	(dolist (s (rest (args)))
		(set 'temp-index-num (string $idx)) ; we need to number the symbols to keep them in the correct order
		(if (= (length temp-index-num) 1) (set 'temp-index-num (string "0" temp-index-num))) ; leading 0 keeps the max at 100.
		(sym (string temp-index-num s) 'DB))
	; now create the sql query 
	(set 'temp-sql-query (string "INSERT INTO " temp-table-name " ("))
	;(println "<P>TABLE NAME: " temp-table-name)
	;(println "<P>SYMBOLS: " (symbols DB))
	;(println "<BR>VALUES: " temp-record-values)
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
	;(println "<p>***** SQL QUERY: " temp-sql-query)
	(println (query temp-sql-query)) ; actually run the query against the database
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
	;(println "TEMP-DELETE-QUERY: " temp-sql-query)	
	(query temp-sql-query)
	(delete 'DB) ; we're done, so delete all symbols in the DB context.
)

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
		(println "No entries found")
	)
	(dolist (idx entry-index)
		(set 'entry (sxml idx))
		(set 'dateseconds (parse-date (lookup 'published entry) "%Y-%m-%dT%H:%M:%SZ")) ; convert string date to seconds
		
		(println
			"<div class='entry'>"
			"<div class='headline'>" (lookup 'title entry) "</div><br/>"
			"<div class='published'>" (date dateseconds 0 "%a %d %b %Y %H:%M:%S") "</div><div class='author'>By&nbsp;" (lookup '(author name) entry) "</div><br/>"
			"</div>"
		)
	)
)