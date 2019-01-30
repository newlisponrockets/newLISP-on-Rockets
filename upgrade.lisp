; upgrade.lisp - upgrades the database from Rockets 0.x to 2.0

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
(module "crypto.lsp")
(module "sqlite3.lsp") ; loads the SQLite3 database module


(load "Rockets-config.lisp") ; load configuration information
(open-database RocketsConfig:Database)
(query "ALTER TABLE Posts ADD PostTags TEXT;")
(query "ALTER TABLE Posts ADD PostLastAuthor TEXT;")
(query "ALTER TABLE Posts ADD PostLastDate DATE;")

; go through all posts and find the last author and last author date
(set 'all-posts (query "SELECT * FROM Posts;"))
(dolist (p all-posts)
    (set 'replies (query (string "SELECT * FROM Comments WHERE PostId=" (p 0))))    
    (if replies (begin
        (setq final-reply (last replies))
        (print "FINAL AUTHOR: " (final-reply 2))
        (setq final-author (query (string "SELECT UserName from Users WHERE UserId=" (final-reply 2))))
        (print "FINAL AUTHOR: " final-author)
        (setq last-author (first (first final-author)))
        (setq last-date (final-reply 3))
        (println "LAST AUTHOR **COMMENT**:::>>>>>>>>>" last-author)
        (println "LAST DATE **COMMENT**:::>>>>>>>>>" last-date)
    ) (begin
        (setq last-author (first (first (query (string "SELECT UserName from Users WHERE UserId=" (p 1))))))
        (setq last-date (p 2))
        (println "LAST AUTHOR:::>>>>>>>>>" last-author)
        (println "LAST DATE:::>>>>>>>>>" last-date)
    ))
    (query (string "UPDATE Posts SET PostLastAuthor = '" last-author "', PostLastDate = '" last-date "' WHERE Id=" (p 0)))
)


(close-database)
(exit)

