#lang at-exp racket 

(provide 
  log 
  ->discord-reply
  bot
  launch-bot
  discord-key

  echo

  run-js

  messaging-user-name
  messaging-user-full-message
  messaging-user-id
  messaging-user-id-override
  messaging-user-member-id
  messaging-user-member-id-override

  session-store
  session-load
  session-clear

  help-link ;All bots should have a link to their docs that they can return if a user types ! help

  message->command 
  message->args
  message->mentioned-bot 

  current-command
  current-args
  current-mentioned-bot

  is-mention?
  id->mention

  ensure-messaging-user-has-role-on-server! 
  user-has-role-on-server?

  get-users-from-channel

  on-reaction
  (except-out (struct-out reaction) reaction-data)
  (rename-out [ref-reaction-data reaction-data])

  get-message-data 
  send-message-on-channel
)

(require racket/runtime-path 
         json)

(define-runtime-path bot-runtime "js/bot")


(define (try-to-find-discord-key)
  (define usual-place (build-path 
			(current-directory)
			".discord-key"))
  (if (file-exists? usual-place)
      (begin
	(string-trim (file->string usual-place)))
      #f))

(define discord-key (make-parameter (try-to-find-discord-key)))

(define (echo . args)
  (string-join args " "))

(define (help-link link)
  (thunk
    (~a "You can find my docs here: " link)))

;Put this at the end of your bot file to make your bot rkt into a launcher,
;  e.g. racket bot.rkt would run the bot
(require syntax/parse/define)
(define-syntax (launch-bot stx)
 (syntax-parse stx 
  [(_ b flags ...)
#'(begin
  (module+ main
   (launch-bot-function b flags ...)))]))

  (define (launch-bot-function b #:persist [persist #f])
   (when (not (discord-key))
    (error "You need to specify your bot API key with the (discord-key) parameter"))

   (define args 
    (vector->list
     (current-command-line-arguments)))

   (if (empty? args)
    (begin
     (copy-bot-runtime-to (current-directory)
#:persist persist)
     (system "node bot/bot.js"))
    (command-line-bot b)))


;Note: Storing the JS runtime in the user's working directory
;  has some pros and cons.
;We probably want to be able to hide the ugly JS stuff from the user...
(define (copy-bot-runtime-to place #:persist [persist #f])
  (define target
    (build-path place "bot"))

  (define archived-bot-directory
    (build-path place (~a "archived-bot-" (random 10000))))

  (when (directory-exists? target)
    (rename-file-or-directory
      target
      archived-bot-directory))

  (copy-directory/files 
    bot-runtime
    target)

  (when (and persist 
	     (directory-exists? archived-bot-directory))
    (delete-directory/files #:must-exist? #f
      (build-path target "data"))
    (copy-directory/files
      (build-path archived-bot-directory "data") 
      (build-path target "data")))
  
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

;Message types

(define (reaction-message?)
 ;I guess I'm making an convention that file names carry semantic content.
 ;  Would be nice to clean the JS/Racket comm pipeline up, but no time...

 ;It's a reaction message if the file name is REACTION_***

 (and (current-message-name)
  (string-prefix? 
   (current-message-name)
   "REACTION")))


(define (current-message-name)
 (with-handlers ([exn:fail? (thunk* #f)]) ;Fails in weird cases like when the bot function is called from the server.
  (string-replace
   (first
    (vector->list
     (current-command-line-arguments)))
   "bot/data/" "")))

;PARSING

(define (is-mention? s)
  (string-prefix? s "<@!"))

(define (id->mention i)
    (~a "<@!" i ">"))

(define (mention->id s)
  (regexp-replaces s
    '([#rx"<@!" ""]
      [#rx">" ""])))

(define (message->mentioned-bot msg)
  (define maybe-cmd
    (first (string-split msg " ")))

  (if (is-mention? maybe-cmd)
      (mention->id maybe-cmd) ; It is not a cmd, it is a mention 
      #f))

(define (message->command unparsed-msg)
  (define msg-data 
    (with-handlers 
      ([exn:fail? (thunk* unparsed-msg)])
      (hash-ref (string->jsexpr unparsed-msg) 'cmd)))

  (define msg   (if (string? msg-data) msg-data   (hash-ref msg-data 'cmd)))

  (define maybe-cmd
    (first (regexp-split #rx"[ \n]+" (string-trim msg))))

  (if (is-mention? maybe-cmd)
      (let()
	(define l 
	  (regexp-split #rx"[ \n]+" (string-trim msg)))

	(when (> 2 (length l))
	  (error "You mentioned a bot, but there was no command afterward.  The general syntax is `!@SomeBot some-command some various args`"))

	(second l))
      maybe-cmd))

(define (message->args msg)
  (define parts
    (regexp-split #rx"[ ]+" (string-trim msg)))

  (define maybe-cmd
    (first parts))

  (if (is-mention? maybe-cmd)
      (drop parts 2)
      (drop parts 1)))


;END PARSING

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


 (display (->discord-reply reply)))


(define (->discord-reply reply)
  (local-require
    (only-in
      2htdp/image image? save-image))

  ;If it's an image,
  ;  write to a file.
  ;Etc.  Handle special return
  ;  types here
  ;(displayln reply)
  (cond
    [(string? reply) reply] 
    [(number? reply) (~a reply)] 
    [(boolean? reply) (~a reply)] 
    [(symbol? reply) (~a "`"(pretty-format reply)"`")] 
    [(list? reply) 
     (string-join
       (map ->discord-reply reply)
       "\n")] 
    [(image? reply) 
     (let ()
       (define name (~a (random 1000000) ".png"))
       (define path
        (build-path "bot" "data" name))
       (save-image reply path)
       (~a "FILE:" name))
     ] 

    ;Handle images here...

    [else ""] ;Doesn't trigger a reply.  Empty messages don't send.
    ))

(define (command-line-bot b)
 (test-bot b 

  (file->string
   (first
    (vector->list
     (current-command-line-arguments))))))


(define current-command (make-parameter #f))
(define current-args (make-parameter #f))
(define current-mentioned-bot (make-parameter #f))

;A bot is just a function that takes strings, parses out their commands,
;  and calls predefined functions on their args, depending on the command.
;[Basically it handles messages that get chatted at it.  
;  For other bot, callbacks, we can use keywords to specify them.]
;[Consider making this a funciton, but making the bot rules a macro.]
(define-syntax-rule (bot [the-cmd bound-func] ...)
  (lambda (msg)
     (cond 
      [(reaction-message?) (handle-reaction-message msg)]
      [else 
      (let ()
	(log msg)

	(define parsed-msg 
	  (with-handlers ([exn:fail? (thunk* msg)])
	    (hash-ref (string->jsexpr msg) 'cmd)))

	(define current-cmd   (message->command       parsed-msg))
	(define args          (message->args          parsed-msg))
	(define mentioned-bot (message->mentioned-bot parsed-msg))

	(log current-cmd)
	(log args)
	(log mentioned-bot)

	(parameterize ([messaging-user-full-message msg]
		       [current-command current-cmd]
		       [current-args args]
		       [current-mentioned-bot mentioned-bot])

		      (define current-bound-func
			(match current-cmd
			       [the-cmd bound-func] ...))

		      (apply current-bound-func args)))])))

(struct reaction (data) #:transparent)

(define (ref-reaction-data r key)
  (hash-ref (reaction-data r) key))

(define (handle-reaction-message data)
  (raise 
    (reaction (string->jsexpr data))))

(define (on-reaction b f)
 (lambda ( a . args) 
  (with-handlers ([reaction?
                  (lambda (r)
                   (f r))])
   (apply b a args))))

(define (run-js . strings)
  (define program
    @~a{
    const Discord = require('discord.js');
    const fs = require('fs');
    const client = new Discord.Client();
    const { exec } = require("child_process")

    const config = require("./config.json");
    client.login(config.token);

    client.on('ready', () => {
              @(string-join strings "")
    })

   }
  )

 ;Save to file and run...
 (define cmd.js
   (~a "bot/cmd-" (random 1000000) ".js"))

 (with-output-to-file cmd.js
   (thunk*
     (displayln program)))

 ;I kind of want to call this in a thread, but when we do, the main.rkt returns early and I guess the thread gets killed.
 (system (~a "node " cmd.js)))


(define messaging-user-full-message (make-parameter "no-message"))

;Ugly atm
(define (messaging-user-name)
  (define args 
    (vector->list
      (current-command-line-arguments)))

  (if (empty? args)
      "unknown-user"
      (hash-ref (hash-ref (string->jsexpr (file->string (first args))) 'author) 'username)
      ))

;Ugly atm

(define messaging-user-id-override
  (make-parameter #f))

(define messaging-user-member-id-override
  (make-parameter #f))

(define (messaging-user-id)
  (or
   (messaging-user-id-override)
   (let ()
    (define args 
     (vector->list
      (current-command-line-arguments)))

    (if (empty? args)
     "unknown-id"
     (string-replace
      (first ;id
       (string-split
        (first args)
        "-"))
      ".txt" ""))
   )))

;Ugly atm
(define (messaging-user-member-id)
  (or
    (messaging-user-member-id-override)
    (let ()
      (define args 
	(vector->list
	  (current-command-line-arguments)))

      (if (empty? args)
	"unknown-member-id"
	(string-replace
	  (third ;member id, is this third now?
	    (string-split
	      (first args)
	      "-"))
	  ".txt" "")))))

;STATE/SESSIONS

(define (session-store username key val)
  (define session-dir (build-path "bot" "data" username)) 

  (make-directory* session-dir)

  (with-output-to-file #:exists 'replace
		       (build-path session-dir (~a key))
		       (thunk*
			 (write val))))

(define (session-load username key [default-value (void)])
  (define key-file (build-path "bot" "data" username (~a key))) 


  (if (and (not (void? default-value))
	   (not (file-exists? key-file)))
      default-value
      (read (open-input-file key-file))))


(define (session-clear username)
  (define session-dir (build-path "bot" "data" username)) 

  (delete-directory/files session-dir
			  #:must-exist? #f))


(define (get-message-data mid cid)
 (string->jsexpr
  (with-output-to-string
   (thunk*
    @run-js{
    client.channels.cache.get('@cid').messages.fetch('@mid').
    then((m)=>{
        console.log(JSON.stringify({
              "content": m.content,
              "author-id": m.member.id,
              "author": {
              "roles": m.member.roles.cache
              .map((r)=>{
                  return {"id": r.id, "name": r.name}
              })
            }
         }))
        client.destroy()
        }) 
    }))))

(define (send-message-on-channel cid msg)
 (log "send-message-on-channel")
 (define ret
  (string->jsexpr
   (with-output-to-string
    (thunk*
     @run-js{
     client.channels.cache.get('@cid').send(`@(->discord-reply msg)`.trim()).
     then((m)=>{
         client.destroy()
         }) 
     }))))

 (log "finished: send-message-on-channel")

 ret)


;TODO: Rewrite this in terms of user-has-role-on-server?, which is more general
(define (ensure-messaging-user-has-role-on-server! role-id server-id
						   #:failure-message (failure-message "Insufficient Roles!"))
  (define s
    (with-output-to-string
      (thunk
	@run-js{
	client.guilds
	.resolve('@server-id').members
	.fetch('@(messaging-user-member-id)')
	.then(member => {
		     var role =
			member.roles.cache.find(role => {
						     return role.id == '@role-id'	
					        })
		     if(role){
		       console.log(role.name) 
		       client.destroy()
		     } else {
		       console.log("No Role Found") 
		       client.destroy()
		     }
		 })
	.catch(err =>  {
		   console.log(err)
		   console.log("No User Found") 
		   client.destroy()
		   })
	})))

  (when (string=? (string-trim s) "No Role Found")
    (error failure-message)))

(define (user-has-role-on-server? user-id role-id server-id)
  (log "user-has-role-on-server?")

  (define ret
  (read
   (open-input-string
    (with-output-to-string
     (thunk
      @run-js{
      client.guilds
      .resolve('@server-id').members
      .fetch('@user-id')
      .then(member => {
          var role =
          member.roles.cache.find(role => {
              return role.id == '@role-id'	
              })
          if(role){
          console.log(role.name) 
          client.destroy()
          } else {
          console.log("#f") 
          client.destroy()
          }
          })
      .catch(err =>  {
          console.log("#f") 
          client.destroy()
          })
      })))))

 
  (log "finished: user-has-role-on-server?")

  ret)



(define (get-users-from-channel voice-channel-id)
  (map id->mention
       (string-split
         (with-output-to-string
           (thunk*
             @run-js{
             client.channels
             .fetch('@voice-channel-id')
             .then(channel => {
                           var a = channel.members.keyArray()
                           for(var i = 0; i < a.length; 
                                   i++)
                           {
                           console.log(a[i])
                           }
                           client.destroy()
                           })

             }
             "\n")))))


(define (log msg . stuff)
  #;(define log-dir
    (build-path "logs"))

  #;(when (not (directory-exists? log-dir))
    (make-directory* log-dir))

  #;(with-output-to-file #:exists 'append
    (build-path log-dir "log")
    (thunk*
      (writeln msg)
      (map writeln stuff)))
  '()
  )

(module+ test
  (require rackunit) 

  (check-equal?
    (message->args "+ 1 2 3   4 5  6")
    '("1" "2" "3" "4" "5" "6"))

  (check-equal?
    (message->args "  + 1 2 3   4 5  6")
    '("1" "2" "3" "4" "5" "6"))

  (check-equal?
    (message->command "{\"cmd\": \"try\n(circle 30 'solid 'red)\"}")
    "try")

  (check-equal?
    (message->command "{\"cmd\": \"try (circle 30 'solid 'red)\"}")
    "try")

  (check-equal?
    (message->command "{\"cmd\": \"try (circle 30 'solid 'red)\"}")
    "try")

  (check-equal?
    (message->command "{\"cmd\": \"<@!abc> try (circle 30 'solid 'red)\"}")
    "try")

  (check-equal?
    (message->command "{\"cmd\": \"<@!abc>    try (circle 30 'solid 'red)\"}")
    "try")
  )



