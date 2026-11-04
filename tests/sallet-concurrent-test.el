;;; sallet-concurrent-test.el --- Tests for sallet-concurrent -*- lexical-binding: t -*-

(require 'sallet-concurrent)

(defun sallet-concurrent-test--drain (creator)
  "Run a process generator built on CREATOR until it finishes.

Return the candidate strings, or :timeout after two seconds."
  (let* ((generator (csallet-make-process-generator creator #'identity "x"))
         (constructor (plist-get generator :constructor))
         (deadline (time-add (current-time) 2))
         (candidates nil)
         (finished nil))
    (unwind-protect
        (progn
          (while (and (not finished) (time-less-p (current-time) deadline))
            (let ((data (funcall constructor nil nil)))
              (setq candidates (append candidates (mapcar #'car (plist-get data :candidates))))
              (setq finished (plist-get data :finished))
              (unless finished
                (accept-process-output nil 0.01))))
          (if finished candidates :timeout))
      (funcall (plist-get generator :destructor)))))

(defun sallet-concurrent-test--shell (script)
  "Return a process creator running SCRIPT.

The sentinel is `ignore' so the buffer holds only what SCRIPT wrote,
without the default \"Process ... finished\" line."
  (lambda (buffer _prompt)
    (let ((process (start-process "sallet-test" buffer "sh" "-c" script)))
      (set-process-sentinel process #'ignore)
      process)))

(describe "Process generator"
  (it "finishes when the process exits without output"
    (expect (sallet-concurrent-test--drain (sallet-concurrent-test--shell "true"))
            :to-equal nil))

  (it "emits an unterminated last line and finishes"
    (expect (sallet-concurrent-test--drain (sallet-concurrent-test--shell "printf 'a\\nb'"))
            :to-equal '("a" "b")))

  (it "finishes when the creator starts no process"
    (expect (sallet-concurrent-test--drain (lambda (_buffer _prompt) nil))
            :to-equal nil)))
