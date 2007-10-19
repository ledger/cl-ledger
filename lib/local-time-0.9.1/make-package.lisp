(require :sb-posix)

(defun mktemp (prefix &key (directory nil))
  (let* ((template (concatenate 'string prefix ".XXXXXX"))
         (process (sb-ext:run-program "/usr/bin/mktemp"
                          (if directory
                              (list "-d" "-t" template)
                              (list "-t" template))
                          :output :stream)))
    (prog1
        (read-line (sb-ext:process-output process) nil nil)
      (sb-ext:process-wait process))))


(defun run (program args)
  (format t "~a ~{~a~^ ~}~%" program args)
  (force-output)
  (sb-ext:run-program program args :input t :output t :search t))

(defun unix-cp (src-files dst)
  (run "cp"
       (append (mapcar #'namestring src-files)
               (list (namestring dst)))))

(defun unix-rm (doomed-files)
  (run "rm" (cons "-rf" (mapcar #'namestring doomed-files))))

(defun make-distribution (path version)
  (let* ((tmp-dir (mktemp "local-time-dist" :directory t))
         (dst-dir (format nil "local-time-~a" version))
         (dst-path (format nil "~a/~a" tmp-dir dst-dir))
         (dst-tgz (format nil "local-time-~a.tar.gz" version)))
    (sb-posix:mkdir dst-path #o755)
    (unix-cp (directory (format nil "~a/*.*" path)) dst-path)
    (unix-rm (append (list (format nil "~a/.svn" dst-path))
                     (directory (format nil "~a/*.fasl" dst-path))
                     (directory (format nil "~a/*.tar.gz" dst-path))))

    (run "tar" (list "-zcf" dst-tgz "-C" tmp-dir dst-dir))
    (unix-rm (list tmp-dir))
    (format t "Enter GPG password: ")
    (force-output)
    (let* ((pw (read-line))
           (gpg (sb-ext:run-program "gpg" (list "-ba" "--status-fd" "1" dst-tgz)
                             :pty t
                             :input :stream
                             :output :stream
                             :search t
                             :wait t)))
      (write-line pw (sb-ext:process-input gpg))
      (loop for c = (read-char (sb-ext:process-output gpg) nil nil)
            while c
            do (write-char c))
      (sb-ext:process-wait gpg))
    dst-tgz))
