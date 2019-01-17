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
(query "ALTER TABLE Posts ADD PostLastDate INTEGER;")
(close-database)
(exit)

