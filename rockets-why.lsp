#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; (rockets-verify.lsp) - Rockets - User verification page 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header (string RocketsConfig:Name " - Why Rockets?"))
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(set 'active-page "rockets-why")
(display-partial "rockets-navbar")

(displayln "<h1>Why newLISP on Rockets?</h1>")

(displayln "<P>Why indeed?  Why create yet another framework using yet another language?  What do you hope to accomplish?")
(displayln "<p>Let me tell you a story.</p>")
(display-image "eniac4.gif")
(displayln "<h3>In the beginning</h3>")
(displayln "<p>The first computers weren't really programmable as such.  You would plug in patch cables to")
(displayln "physically rewire the system.  This was quickly replaced with binary code, where instructions and data were")
(displayln "entered alternately in chunks.  Usually it was instruction, then data, or instruction then data then data.  The")
(displayln "code and the data were the same: it was just the order they came in that mattered.  Remember this for later.</p>")
(displayln "<h3>Languages</h3>")
(displayln "<p>Binary code was converted to hexadecimal for simplicity and easier recognition.  Then someone had the bright idea to")
(displayln "have the computer translate three or four digit mnemonics for the instructions, so instead of issuing instruction 8B you could")
(displayln "say MOV instead.  Now, the instructions stopped looking like the data.  The computer still saw them the same, but the")
(displayln "programmer no longer did.</p>")
(displayln "<p>High level languages continued this trend, abstracting huge chunks of assembly language code in order to simplify life")
(displayln "for the programmer.  Lots of high-level languages were developed in the 1950s, and they left a long shadow in the computer world:")
(displayln "<ul><li>FORTRAN (1957) - This was the first compiler and historically significant, even if the language syntax was a little too limited and clunky.  It was still being taught at my local university as late as the mid-1990s.</li>")
(displayln "<li>ALGOL (1958) - The forerunner of many modern languages.  If you change the BEGIN and END commands to { and } it looks a lot like modern C#.</li>")
(displayln "<li>LISP (1958) - The most visually distinct language, characterized by its (LOTS (OF (SINGLE (PARENTHESES)))).")
(displayln "</ul>")
(displayln "<h3>Why LISP?</h3>")
(displayln "<p>The one thing that LISP does that no other languages do is that data is written exactly the same way as code.  This is the sort of")
(displayln "brilliant idea that takes a while to really sink in.  The fundamental structure of LISP is lists.  Your data is in lists.  Your code is in lists. ")
(displayln "Everything is a function, and defining new functions is trivial, so you start extending the language without even realizing it.")
(displayln "A function is just a list with the first element in the list being special and the rest being data.  Older LISPs had functions to extract")
(displayln "the first element and the rest of the elements, called CAR and CDR because it was based on IBM assembly language instructions.  In newLISP, these")
(displayln "functions are more sensibly called (first) and (rest).")
(displayln "<p>Like many people, I encountered LISP in university and never quite 'grokked' why it was so special.  Many years later I came across")
(displayln "Paul Graham's essay called <a href='http://paulgraham.com/avg.html'>Beating the Averages</a> and got very excited about the idea of outperforming")
(displayln "much larger programming teams by using a 'secret' language that everyone else ignored.  It's a classic David-versus-Goliath scenario, and very appealing.")
(displayln "But would it actually work in real life?  I decided to try it.  To my surprise, it <i>worked exactly as advertised</i>.")
(displayln "<p>But surely the world has moved on, hasn't it?  The stuff Paul Graham's two-person team was doing was back in the late 1990s.  We have much better tools now, right?</p>")
(displayln "<p><b>Wrong</b>.  We have <i>bigger</i> tools, with more features, that take longer to learn.  Every few years there is a new 'sexy' language")
(displayln "or framework that promises to simplify things, and many them are quite good, like Ruby on Rails.  But none of them can ever have code that is the same as data.")
(displayln "They can't.  If they did, they would be LISP.")
(displayln "<h3>So why newLISP?</h3>")
(displayln "<p>There are a lot of LISP dialects out there.  The standard one is Common Lisp, which has grown and congealed over the years to become something")
(displayln "much more complicated than it needs to be.  There are new languages like Clojure, but as neat as it is it comes with the complexity of Java bolted underneath it.</p>")
(displayln "<p>There are other dialects like PicoLisp and newLISP that aimed at being simple, small, and fast.  I chose newLISP because someone had written a really cool")
(displayln "web development framework called <a href='rundragonfly.com'>Dragonfly</a> that made it ridiculously easy to make new web applications.")
(displayln "<h3>Okay, so why Rockets?</h3>")
(displayln "<p>As great as Dragonfly is, something kept pulling me towards writing a replacement.  It's a testament to LISP's power that doing so was even possible. ")
(displayln "Working with LISP makes you think you can do all sorts of crazy things.  I wanted a framework that simplified database work and jQuery access, something that ")
(displayln "Dragonfly didn't do, and wasn't likely to do as development seems to have stopped.  I just decided to start doing it, just for fun, just because I wanted to.")
(displayln "<p>And here we are.")
(displayln "<h3>So why should I use Rockets?</h3>")
(displayln "<P>You should use Rockets if you are looking for rapid application development or prototyping.  You should use Rockets if you want really fast performance.  You should")
(displayln "use Rockets if you want these things and want your life to be simple.  Modern web developers have to keep a lot of things in their heads and have to constantly learn new")
(displayln "technologies.  They have to worry about having a nice GUI with flashy Javascript things.  They have to worry about things like SQL injection and cross-site scripting attacks and salting passwords and all sorts of other things that keep popping up.</p>")
(displayln "<P>Rockets is designed to take care of a lot of things for you, so you can think about what your awesome new application will actually do.")


(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!