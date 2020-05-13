#lang racket 

(provide 
  bot
  launch-bot
  discord-key

  echo

  ;message->command 
  ;message->args
  ;rules->call
  ;test-bot
  ;command-line-bot
  )

(require racket/runtime-path)

(define-runtime-path bot-runtime "js/bot")

(define discord-key (make-parameter #f))

(define (echo . args)
  (string-join args " "))

;Put this at the end of your bot file to make your bot rkt into a launcher,
;  e.g. racket bot.rkt would run the bot
(define-syntax-rule (launch-bot b)
    (begin
      (module+ main

	       (when (not (discord-key))
		 (error "You need to specify your bot API key with the (discord-key) parameter"))

	       (define args 
		 (vector->list
		   (current-command-line-arguments)))

	       (if (empty? args)
		   (begin
		     (copy-bot-runtime-to (current-directory))
		     (system "node bot/bot.js"))
		   (command-line-bot b)))))


;Note: Storing the JS runtime in the user's working directory
;  has some pros and cons.
;We probably want to be able to hide the ugly JS stuff from the user...
(define (copy-bot-runtime-to place)
  (define target
    (build-path place "bot"))

  (when (directory-exists? target)
    (rename-file-or-directory
      target
      (build-path place (~a "archived-bot-" (random 10000)))))

  (copy-directory/files 
    bot-runtime
    target)
  
  (give-credentials-to-node.js target)
  )

(define (give-credentials-to-node.js js-runtime-dir)
  (local-require json)
  (with-output-to-file 
    #:exists 'replace
    (build-path js-runtime-dir "config.json")
    (thunk*
      (display 
	(jsexpr->string
	  (hash
	    'token (discord-key)
	    'prefix "!" ;Parameterize later?
	    ))
	))))

(define (message->command msg)
  (first (string-split msg " ")))

(define (message->args msg)
  (rest (string-split msg " ")))

(define (rules->call h k)
  (hash-ref h k 
	    (thunk* (thunk* 
		      (~a "Command not found: " k)))))


;Probably should be called run-bot
(define (test-bot f msg)
  (define reply
    (with-handlers ([exn:fail? 
		      (lambda (e)
			(exn-message e)) ])
		   (f msg)))


    ;If it's an image,
    ;  write to a file.
    ;Etc.  Handle special return
    ;  types here
    ;(displayln reply)

    (cond
      [(string? reply) (displayln reply)] 

      ;Handle images here...

      [else (void)] ;Doesn't trigger a reply
      )
  )

(define (command-line-bot b)
  (test-bot b 
	    (file->string
	      (first
		(vector->list
		  (current-command-line-arguments))))))


;A bot is just a function that takes strings, parses out their commands,
;  and calls predefined functions on their args, depending on the command.
;[Basically it handles messages that get chatted at it.  
;  For other bot, callbacks, we can use keywords to specify them.]
;[Consider making this a funciton, but making the bot rules a macro.]
(define-syntax-rule (bot [cmd bound-func] ...)
		    (let ()
		      (define (f msg)

			(define current-cmd
			  (message->command 
			    msg))

		        (define current-bound-func
			  (rules->call 
			    (apply hash 
			      (flatten (list (list cmd bound-func) ...)))
			    current-cmd))

			(apply current-bound-func (message->args msg)))

		      f))


