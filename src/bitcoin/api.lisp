(in-package :cjhunt.bitcoin-rpc)

(defmacro define-rpc (command &optional args &rest rpc-args)
  (let ((string (string-downcase command)))
    `(export (defun ,command ,(or args rpc-args) ,(rpc "help" string)
                    (rpc ,string ,@(remove '&optional rpc-args)))))) ;TODO:FIXME

(define-rpc getblock
    (&optional (id (cdr (assoc :bestblockhash (getblockchaininfo))))) id)
(define-rpc getrawtransaction (id &optional (jsonp 1)) id jsonp)

(macrolet ((porn ()                     ; if you have to ask, you'll never know
             (labels ((split (delimiter sequence)
                        (split-sequence delimiter sequence
                                        :remove-empty-subseqs t))
                      (inter (string) (intern (string-upcase string))))
               `(progn
                  ,@(loop for line in (split #\Newline (btc:rpc "help"))
                       for (command . parameters) = (split #\  line) ;D
                       unless (or (and (string= "==" command)      :P
                                       (string= "==" (first (last parameters))))
                                  (fboundp (inter command))) collect
                         `(define-rpc ,(inter command) () ;)
                            ,@(loop for raw in parameters
                                 for trim = (string-trim "\\\")" raw)
                                 if (string= "(" trim) collect '&optional ;(
                                 else unless (or (zerop (length trim))
                                                 (string= "delta>" trim) ;<
                                                 ) collect (inter trim))))))))
  (porn))                               ; well maybe you will, once you see it.
