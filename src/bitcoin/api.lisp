(in-package :cjhunt.bitcoin-rpc)

(defmacro define-rpc (command &optional args &rest rpc-args)
  (let ((string (string-downcase command)))
    `(export (defun ,command ,args
               ,(rpc "help" string) (rpc ,string ,@rpc-args)))))

(define-rpc getblockchaininfo)
(define-rpc getbestblockhash)
(define-rpc getblock
    (&optional (id (cdr (assoc :bestblockhash (getblockchaininfo))))) id)
(define-rpc getrawtransaction (id &optional (jsonp 1)) id jsonp)
