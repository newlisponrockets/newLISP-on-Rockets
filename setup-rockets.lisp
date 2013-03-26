; setup-rockets.lisp - Sets up database and tables for a new installation of newLISP on Rockets
;
; Includes Users, Posts, and Comments for a bulletin-board and blog type setup

(define (displayln str)
	(println str))

(define (open-database sql-db-to-open)
	(if (sql3:open (string sql-db-to-open ".db"))  
		(displayln "")
		(displayln "There was a problem opening the database " sql-db-to-open ": " (sql3:error))))

(define (close-database)
	(if (sql3:close)
		(displayln "")
		(displayln "There was a problem closing the database: " (sql3:error))))

(define (query sql-text)
 (set 'sqlarray (sql3:sql sql-text))    ; results of query
 (if sqlarray
   (setq query-return sqlarray)
		(if (sql3:error)
			(displayln (sql3:error) " query problem ")
			(setq query-return nil))))

(define (safe-for-sql str-sql-query)
	(if (string? str-sql-query) (begin
		(replace "&" str-sql-query "&amp;")
		(replace "'" str-sql-query "&apos;")
		(replace "\"" str-sql-query "&quot;")
		))
		(set 'result str-sql-query))

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




(module "crypto.lsp")
(module "sqlite3.lsp") ; loads the SQLite3 database module

(set 'table1 "CREATE TABLE Posts (Id INTEGER PRIMARY KEY, PosterId TEXT, PostDate DATE, PostSubject TEXT, PostContent TEXT, PostComments INTEGER, PostType TEXT, PostViews INTEGER, PostPoll TEXT)")
(set 'table2 "CREATE TABLE Users (UserId INTEGER PRIMARY KEY, UserEmail TEXT, UserPasswordHash TEXT, UserSalt TEXT, UserPosts INTEGER, UserAchievements TEXT, UserReadPosts TEXT, UserName TEXT, CookieSalt TEXT, UserAvatar TEXT, UserBirthdate DATE, UserJoinedDate DATE, UserPollsVoted TEXT)")
(set 'table3 "CREATE TABLE Comments (Id INTEGER PRIMARY KEY, PostId INTEGER, CommenterId INTEGER, CommentDate DATE, CommentSubject TEXT, CommentContent TEXT)")

(println "This is a VERY RUDIMENTARY setup for the newLISP on Rockets database!")
(println)
(println "Now creating database...")
;(open-database database-name)
(println "Please enter a name for your blog (eg: The newLISP on Rockets Blog)")
(set 'RocketsConfig:Name (read-line))
(println "Now enter a short version of this name to appear on the header (eg: newLISP on Rockets)")
(set 'RocketsConfig:ShortName (read-line))
(println "Now enter the URL (minus the http://) of the blog (eg: newlisponrockets.com)")
(set 'RocketsConfig:SiteURL (read-line))
(println "Now enter the owner of the blog (eg: Rocket Man)")
(set 'RocketsConfig:Owner (read-line))
(println "Now setting up Posts, Users, and Comments tables...")
(print "Enter a database name (.db extension added automatically): ")
(set 'database-name (upper-case (read-line)))
(set 'RocketsConfig:Database database-name)
(print "Enter a user name for the ADMIN user (case sensitive): ")
(set 'UserName (read-line))
(print "Enter an email for the ADMIN user (case sensitive): ")
(set 'UserEmail (read-line))
(set 'RocketsConfig:AdminEmail UserEmail) ; add this to config file
(print "Now enter a password for the ADMIN user (case sensitive): ")
(set 'password (read-line))
(set 'UserSalt (uuid))
(set 'CookieSalt (uuid))
(println "Salt: " UserSalt)
(println "Cookie Salt: " CookieSalt)
(set 'UserPasswordHash (crypto:sha1 (string UserSalt password)))
(println "Password hash: " UserPasswordHash)
(set 'UserId 0) ; Admin user is always UserId 0
(set 'UserPosts 0) ; start from the bottom!

; save the configuration file
(save "Rockets-config.lisp" 'RocketsConfig)

; create the database
(open-database database-name)

(query table1)
(query table2)
(query table3)

(create-record "Users" UserId UserEmail UserPasswordHash UserSalt UserPosts UserName CookieSalt)

; now make a default post
;Id INTEGER PRIMARY KEY, PosterId TEXT, PostDate DATE, PostSubject TEXT, PostContent TEXT, PostComments INTEGER, PostType TEXT)
(set 'Id 0) ; first post # is always 0
(set 'PosterId 0) ; posted by admin user
(set 'PostDate (date (date-value) 0 "%Y-%m-%d %H:%M:%S.000"))
(set 'PostSubject "This is a test post")
(set 'PostContent "This is a test post to make sure the blog code works from scratch.  After you have added a new post yourself, you can delete this one.")
(set 'PostType "Blog post")
(create-record "Posts" Id PosterId PostDate PostSubject PostContent PostType)

; check to see if it worked!
(set 'user-table (query "select * from Users"))
(println "User data: " user-table)
(close-database)
(exit)