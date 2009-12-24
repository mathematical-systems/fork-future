(in-package :fork-future)

(defvar *future-result-file-template* "future-result.~d.tmp~~")

(defvar *futures* (make-hash-table :test #'eql))

(defvar *after-fork-hooks* nil)
(defvar *before-fork-hooks* nil)

(defclass future ()
  ((pid :initarg :pid
        :reader pid-of
        :initform (error "Must provide PID for future"))
   (code :reader code-of
         :initarg :code
         :initform (error "Must provide code for future"))
   (result :reader result-of :initform 'unbound)
   (exit-status :reader exit-status-of :initform 'unknown)))

(defmethod print-object ((f future) stream)
  (with-accessors ((pid pid-of)
                   (exit-status exit-status-of)
                   (result result-of)
                   (code code-of)) f
    (print-unreadable-object (f stream :type t :identity t)
      (format stream "PID: ~A, CODE: ~A, EXIT-STATUS: ~A, RESULT: ~A"
	      pid code exit-status result))))

(defmethod initialize-instance :after ((f future) &key &allow-other-keys)
  (with-slots (pid) f 
    (assert (and (integerp pid) (> pid 0)))
    (assert (not (gethash pid *futures*)))
    (setf (gethash pid *futures*) f)))

(defmethod read-result ((future future) status-code)
  (check-type status-code integer)
  (with-slots (pid code result exit-status) future
    (setf exit-status status-code)
    (let* ((path (format nil *future-result-file-template* pid)))
      (destructuring-bind (output stored-result)
	  (cl-store:restore path)
	(when (and output (> (length output) 0))
          (format t "~&Child of PID ~a says ~a.~%" pid output))
	(setf result stored-result)
	(delete-file (probe-file path))
	(remhash pid *futures*)))
    (unless (zerop exit-status)
      (error "Future terminated with error ~a" result))
    future))

(defmethod wait-for-future ((future future))
  (if (not (eq (result-of future) 'unbound))
      future
      (loop for f = (wait-for-any-future)
            until (or (eq f future) (null f))
            finally
         (return (or f (error "Future seems to be already finished but the states are not updated."))))))

(defun wait-for-any-future (&optional error-p (warn-p t))
  (multiple-value-bind (maybe-pid status)
      (nix:waitpid 0)
    (cond ((and (> maybe-pid 0) (gethash maybe-pid *futures*))
	   (read-result (gethash maybe-pid *futures*) status))
	  ((< maybe-pid 0)
	   (when error-p
             (error "No more child process.")))
          ((= maybe-pid 0)
           (error "Child exit status shouldn't be 0."))
          (t
           (when warn-p
             (warn "A child process of PID ~a has just been reaped but it's not among the futures." maybe-pid))))))

(defun wait-for-all-futures ()
  (loop while (> (hash-table-count *futures*) 0)
        do (wait-for-any-future)))

(defmethod kill-future ((future future) &optional force) 
  (let* ((pid (pid-of future))
         (file (format nil *future-result-file-template* pid)))
    (nix:kill pid (if force nix:sigkill nix:sigterm))
    (when (probe-file file)
      (delete-file (probe-file file)))
    (remhash pid *futures*)))

(defun kill-all-futures (&optional force)
  (maphash #'(lambda (key value)
               (declare (ignore key))
	       (kill-future value force))
	   *futures*)
  (clrhash *futures*))


(defun eval-future (fn code)
  (check-type fn function)
  ;; before hook
  (mapc #'funcall *before-fork-hooks*)
  ;; eval
  (let ((pid (nix:fork)))
    (cond ((> pid 0)
           ;; parent process context
           (setf (gethash pid *futures*)
                 (make-instance 'future :pid pid :code code)))
          ((zerop pid)
           ;; child process context
           (let* ((in (make-string-input-stream ""))
                  (out (make-string-output-stream))
                  (tw (make-two-way-stream in out))
                  (*standard-input* in)
                  (*standard-output* out)
                  (*error-output* out)
                  (*trace-output* out)
                  (*terminal-io* tw)
                  (*debug-io* tw)
                  (*query-io* tw)) 
             (let* ((output-pathname (format nil *future-result-file-template* (nix:getpid))))
               (handler-case 
                   (progn
                     (mapc #'funcall *after-fork-hooks*)
                     (let ((result (funcall fn)))
                       (cl-store:store (list (get-output-stream-string out) result) output-pathname) 
                       (close tw)
                       (close in)
                       (close out)
                       (nix:exit 0)))
                 (error (e)
                   (cl-store:store (list (get-output-stream-string out) e) output-pathname)
                   (close tw)
                   (close in)
                   (close out)
                   (nix:exit 1))))))
          (t
           (error "Fork failed with error code: ~a" pid)))))

(defmacro future (&body body)
  "Evaluate expr in parallel using a forked child process. Returns a
'future' object whose value can be retrieved using touch. No
side-effects made in <expr> will be visible from the calling process."
  `(eval-future #'(lambda () ,@body) '(progn ,@body)))

(defmethod touch ((future future))
  "walk the list structure 'future', replacing any futures with their
evaluated values. Blocks if a future is still running."
  (with-slots (result) future
    (loop while (eq result 'unbound)
          do (wait-for-future future))
    (return-from touch result)))


