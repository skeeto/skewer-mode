;;; cache-table.el --- a hash table with expiring entries

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; Version: 1.0

;;; Commentary:

;; See the docstring of `make-cache-table'. There is no
;; `put-cache-table': use `setf' on `get-cache-table' instead.

;;; Code:

(eval-when-compile (require 'cl))

(defstruct cache-table-struct
  "A cache table with expiring entries. Use `make-cache-table' to
create instances of this struct."
  expire-time table)

(defun make-cache-table (expire-time &rest keyword-args)
  "Create a new cache-table with entries automatically removed
from the table after EXPIRE-TIME seconds. This function accepts
the same keyword arguments as `make-hash-table'. Entries are not
actually removed from the cache-table until an access is made to
the cache-table.

Use `get-cache-table' to get and put (via setf) entries."
  (make-cache-table-struct :expire-time expire-time
                           :table (apply #'make-hash-table keyword-args)))

(defun cache-table-clear-expired (cache-table)
  "Remove all expired entries from CACHE-TABLE."
  (loop with expire-time = (cache-table-struct-expire-time cache-table)
        with table = (cache-table-struct-table cache-table)
        with dead-time = (- (float-time) expire-time)
        for key being the hash-keys of table using (hash-value entry)
        for (time . value) = entry
        when (< time dead-time) do (remhash key table)))

(defun get-cache-table (key cache-table &optional default)
  "Access the value for KEY in CACHE-TABLE if it has not yet
expired. Behaves just like `gethash'."
  (cache-table-clear-expired cache-table)
  (cdr (gethash key (cache-table-struct-table cache-table) (cons 0 default))))

(defsetf get-cache-table (key cache-table) (value)
  "Put an entry in the hash table, like (setf (gethash key table) value)."
  `(progn
     (cache-table-clear-expired ,cache-table)
     (puthash ,key (cons (float-time) ,value)
              (cache-table-struct-table ,cache-table))))

(defun cache-table-map (f cache-table)
  "Like `maphash', call F for all non-expired entries in CACHE-TABLE."
  (cache-table-clear-expired cache-table)
  (maphash (lambda (k v) (funcall f k (cdr v)))
           (cache-table-struct-table cache-table)))

(defun cache-table-count (cache-table)
  "Like `hash-table-count', count the number of non-expired entries."
  (hash-table-count (cache-table-struct-table cache-table)))

(provide 'cache-table)

;;; cache-table.el ends here
