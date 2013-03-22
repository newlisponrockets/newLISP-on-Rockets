; Access log -> Database script log-to-database.lisp
; 
; This script copies everything in /var/log/apache2/access.log into a database
; which can be queried later.
; 
; Written 2012 by Rocket Man

(module "sqlite3.lsp") ; loads the SQLite3 database module

; FUNCTIONS-------------------------------------------------

(define (displayln str-to-display)
 	(println str-to-display)	
)

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
;; The variable ColumnName1 needs to be assigned a value that will be checked to qualify the query
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

; END FUNCTIONS ===================


(open-database "SERVER-LOGS")
(query "CREATE TABLE Logs (Id INTEGER PRIMARY KEY, IP TEXT, UserId TEXT, UserName TEXT, Date DATE, Request TEXT, Result TEXT, Size INTEGER, Referrer TEXT, UserAgent TEXT)")
;(print (query "SELECT * from SQLITE_MASTER;"))
(set 'access-log (read-file "/var/log/apache2/access.log"))
(set 'access-list (parse access-log "\n"))
(set 'max-items (integer (first (first (query "select count(*) from Logs")))))
(println "Number of items in database: " max-items)
(println "Number of lines in log: " (length access-list))
(dolist (line access-list)
	(set 'line-list (parse line))
	;(println "Line# " $idx " - " line-list)
	;(println "Length of line: " (length line-list))
	(if (> (length line-list) 0) (begin
		(++ max-items)
		(set 'Id max-items) (print $idx "/" (length access-list))
		(set 'IP (string (line-list 0) (line-list 1) (line-list 2))) 
		(set 'UserId (line-list 3))
		(set 'UserName (line-list 4))
		(set 'Date (line-list 5))
		(set 'Date (trim Date "["))
		(set 'Date (trim Date "]")) 
		;(println "DATE: " Date) 
		(set 'date-parsed (date-parse Date "%d/%b/%Y:%H:%M:%S -0700"))
		;(println "DATE-PARSED: " date-parsed)
		(set 'Date (date date-parsed 0 "%Y-%m-%dT%H:%M:%S"))
		(println " " Date)
		(set 'Request (line-list 6))
		(set 'Result (line-list 7))
		(set 'Size (line-list 8))
		(set 'Referrer (line-list 9))
		(set 'UserAgent (line-list 10)) 
		(create-record "Logs" Id IP UserId UserName Date Request Result Size Referrer UserAgent)
	))
)
(close-database)
(exit)