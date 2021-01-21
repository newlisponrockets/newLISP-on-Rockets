newLISP-on-Rockets
==================

*Note: This Readme is being updated to prepare for the Rockets 2.0 release on July 1, 2021.*

To see the docs in progress, visit: http://newlisponrockets.github.io/newLISP-on-Rockets/

Source code for both the newLISP on Rockets framework and an example application, a blog that runs on Rockets.

NOTE: This is a very early version of the code.  To get the blog to work on a new server would require creating a database with various tables (Users, Posts, etc).  To do so I have provided a command-line utility (setup-rockets.lisp) to set up the database for the first time. Make sure this script is not accessible publicly!

Please check the blog for updates on the status of the project.

DISCLAIMER: In no way should this software be used to control actual rockets.  

What is newLISP-on-Rockets?
---------------------------

It's a very simple web application framework running on newLISP.  My goal is to simplify the applications I am already writing in newLISP by consolidating a bunch of useful functions for both front-end (jQuery) and back-end (SQLite) operations.  The emphasis is on simplicity and code conciseness.

See newlisp-rockets.lisp for more information.

Also, check out the code and the blog running at: http://newlisponrockets.com

