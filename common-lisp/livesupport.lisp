;; https://github.com/cbaggers/livesupport/blob/master/livesupport.lisp
;; http://techsnuffle.com/2018/04/17/slimeenableconcurrenthints

(defmacro continuable (&body body)
  "Allow continuing execution from errors."
  `(restart-case (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  "Handle REPL requests."
  #+swank
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

(defun %find-initial-thread ()
  (or #+sbcl (sb-thread:main-thread)
      ;; process == thread in these two
      #+ccl ccl::*initial-process*
      #+lispworks mp:*main-process*
      ;; https://github.com/jcbeaudoin/MKCL/blob/master/src/c/threads.d#L4229
      ;; Shows this is reliable
      #+mkcl (find "Initial" (mt:all-threads) :test #'equal
                   :key #'mt:thread-name)
      ;; https://github.com/rtoy/cmucl/blob/master/src/code/multi-proc.lisp#L1530
      ;;Shows this is reliable
      #+cmucl (find "Initial" mp:*all-processes* :test #'equal
                    :key #'mp:process-name)))

(defun get-server-connection ()
  (or swank::*emacs-connection*
      (swank::default-connection)))

(defun find-initial-thread ()
  (or (%find-initial-thread)
      (let ((connection (get-server-connection)))
        (when (swank::singlethreaded-connection-p connection)
          (swank::current-thread)))))

(defun move-repl-thread-to-initial-thread ()
  (let ((connection (get-server-connection))
        (repl-thread (swank::current-thread))
        (main-thread (find-initial-thread)))
    (unless (eq repl-thread main-thread)
      (setf (swank::mconn.repl-thread connection) main-thread)
      (swank::interrupt-thread main-thread
                               (lambda ()
                                 (swank::kill-thread repl-thread)
                                 (swank::with-bindings swank::*default-worker-thread-bindings*
                                   (swank::handle-requests connection)))))
    main-thread))
