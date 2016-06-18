(in-package :cjhunt.bitcoin-rpc)

;;; TODO: proper json falses, see https://stackoverflow.com/questions/27679494

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-rpc (command &optional args &rest rpc-args)
    (let* ((string (string-downcase command)) (all-args (or args rpc-args)))
      (awhen (member '&optional all-args)
	(rplacd it (mapcar (lambda (spec)
			     (flet ((build (param &optional (default () dp))
				      (apply #'list param default
					     (unless dp `(,(gensym))))))
			       (apply #'build (if (atom spec) `(,spec) spec))))
			   (cdr it))))
      `(export (defun ,command ,all-args ,(rpc "help" string)
		      ,(aif (position '&optional rpc-args)
			    `(multiple-value-call #'rpc
			       ,string ,@(subseq rpc-args 0 it)
			       ,@(mapcar (lambda (arg)
					   `(if ,(caddr arg) ,(car arg) (values)))
					 (subseq all-args (1+ it))))
			    `(rpc ,string ,@rpc-args))))))

  (define-rpc getrawtransaction (id &optional (jsonp 1)) id jsonp)
  (define-rpc listaccounts (&key (minconf 0) (watchonly t)) minconf watchonly)

  (macrolet ((porn ()	       ; if you have to ask, you'll never know
	       (labels ((split (delimiter sequence)
			  (split-sequence delimiter sequence
					  :remove-empty-subseqs t))
			(inter (string) (intern (string-upcase string))))
		 `(progn
		    ,@(loop for line in (split #\Newline (rpc "help"))
			 for (command . parameters) = (split #\  line) ;D
			 unless (or (and (string= "==" command)      :P
					 (string= "==" (first (last parameters))))
				    (nth-value 1 (inter command))) collect
			   `(define-rpc ,(inter command) () ;)
			      ,@(loop for raw in parameters
				   for trim = (string-trim ")\"" raw)
				   if (string= "(" trim) collect '&optional ;(
				   else unless (or (zerop (length trim))
						   (string= "delta>" trim) ;<
						   ) collect (inter trim))))))))
    (porn)))		       ; well maybe you will, once you see it.
