#!/usr/bin/env newlisp

(load "newlisp-rockets.lisp") ; this is where the magic happens!

; Rockets - Poll submission page rockets-poll.lsp 
; 
; This is the first version of the self-hosted blog for newLISP on Rockets.
; The blog is designed to showcase how you would use Rockets for a real application.
; 
; Written 2012 by Rocket Man

(load "Rockets-config.lisp") ; load configuration information
(display-header RocketsConfig:Name)
(open-database RocketsConfig:Database)
(display-partial "rockets-checksignin") ; checks to see if user is signed in
(display-partial "rockets-common-functions") ; loads functions common to the blog but not part of Rockets
(set 'active-page "rockets-main")
(display-partial "rockets-navbar") ; shows the navigation bar with Rockets blog menus

(if Rockets:UserId (begin ; must be signed in to vote

;(displayln "<P>Vote submitted!</p>")
;(displayln "<P>This feature isn't finished yet. Here is some debugging info:")
;(displayln "<P>Here is your POST data: " ($POST) )
(set 'PostSubject (first (first ($POST))))
(replace "_" PostSubject " ")
(set 'topic-vote (int (last (first ($POST))))) ; we know it's an int because it was an $idx
(set 'message-data (get-record "Posts" PostSubject))
(if message-data (begin
	(set 'message-data (first message-data))
	;(displayln "<p>Here is your message data:" message-data)
	(set 'Id (message-data 0)) ; this is the topic # for that post
	(set 'page-back (string "rockets-item.lsp?p=" Id "&f=true")) ; the page to go back to
	(set 'poll-data (message-data 8))
	;(displayln "<P>Here is your vote data:" topic-vote)
	;(displayln "<P>Here is your poll data: " poll-data)
	; First, check to see if you've already voted in this poll.
	(if (find (string Id "-") Rockets:UserPollsVoted) (begin
		(displayln "<p>Sorry, you've already voted in this poll!</p>")
		(displayln "<p><a href=" page-back ">Click here to return to the poll thread.</a>")
		)
	(begin ; otherwise, go nuts!
		; register that we've voted in the UserPollsVoted area
		(set 'UserPollsVoted (string Rockets:UserPollsVoted Id "-")) ; add this poll to polls visited
		(set 'UserId Rockets:UserId) ; make sure we update the user that's logged in!
		(update-record "Users" UserId UserPollsVoted) ; save to database
		(if (nil? poll-data) (begin ; if there's no data at all
			(set 'PostPoll (string topic-vote "-" 1 "_"))
			(displayln "<p>New poll data: " PostPoll " parsed: " (parse PostPoll "_"))
			(update-record "Posts" Id PostPoll)
			(page-redirect page-back)
		) (begin 
			; there's already poll-data so we have to update it
			(displayln "<p>Existing poll data: " poll-data)
			(if (find (string topic-vote "-") poll-data)
				(begin
					(displayln "<p>Votes for this choice are here already!")
					(set 'position-of-vote (find (string topic-vote "-") poll-data))
					(set 'end-position-of-vote (find "-" poll-data 0 position-of-vote))
					(set 'end-position-of-tally (find "_" poll-data 0 end-position-of-vote))
					(set 'old-vote (slice poll-data position-of-vote (- end-position-of-vote position-of-vote) 1))
					(set 'new-vote (slice poll-data (+ end-position-of-vote 1) (- (- end-position-of-tally end-position-of-vote) 1)))
					(displayln "<p>Vote number: " old-vote)
					(displayln "<p>Vote value: " new-vote)
					(displayln "<p>New vote: " position-of-vote "," end-position-of-vote "," end-position-of-tally)
					; bump up the new vote by 1
					(set 'new-vote (string (++ (int new-vote))))
					(displayln "<p>New vote bumped by one: " new-vote)
					; now recreate the string including the new vote
					(set 'new-poll-data (string (slice poll-data 0 end-position-of-vote) "-" new-vote (slice poll-data end-position-of-tally (- (length poll-data) end-position-of-tally))))
					(displayln "<p>New poll data: " new-poll-data)
					(set 'PostPoll new-poll-data)
					(update-record "Posts" Id PostPoll)
					(page-redirect page-back)
				)
				(begin
					(displayln "<p>No vote exists for this choice!")
					(set 'PostPoll (string poll-data topic-vote "-" 1 "_"))
					(update-record "Posts" Id PostPoll)
					(page-redirect page-back)
				)
			)
		))
	)) 
))

)
; not logged in
(begin

	(displayln "<br><p>You must be signed in to vote!</p>")
	(displayln "<p><a href=/>Return to the main page.</a></p>")
))
(close-database)
(display-footer RocketsConfig:Owner)
(display-page) ; this is needed to actually display the page!

