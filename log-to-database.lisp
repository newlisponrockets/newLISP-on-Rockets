#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Access log -> Database script log-to-database.lisp
; 
; This script copies everything in /var/log/apache2/access.log into a database
; which can be queried later.
; 
; Written 2012 by Rocket Man

(display-header "The newLISP on Rockets Blog")
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
		(set 'Id max-items) (print Id "/" (length access-list))
		(set 'IP (string (line-list 0) (line-list 1) (line-list 2))) 
		(set 'UserId (line-list 3))
		(set 'UserName (line-list 4))
		(set 'Date (line-list 5))
		(set 'Date (trim Date "["))
		(set 'Date (trim Date "]"))
		(set 'date-parsed (date-parse Date "%d/%b/%Y:%H:%M:%S -0800"))
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