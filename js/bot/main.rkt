#lang racket

;This needs to return quickly.
;  It can't start a new thread, though.
;  TODO: JS tries to get its output as a file after this returns.   We'll need more fancies in bot.js if we want to make this performant and multithreaded.

(define file
  (first (vector->list (current-command-line-arguments))))

(require 2htdp/image) 

(save-image (eval (read (open-input-string (file->string file) ))
		  (module->namespace '2htdp/image))
	    (~a file ".png"))


