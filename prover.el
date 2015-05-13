;;; prover.el

;; 0. Open this file and M-x eval-buffer.
;;
;; 1. Create a new buffer, "foo". In it:
;;
;; 2. M-x prover-proof-mode (this could happen automatically for file
;;    extension like .proof files).
;;
;; 3. M-x prover-send-step. Should split adding two windows, *prover
;;    responses* and *prover goal*. This came from prover.rkt.
;;
;; 4. M-x prover-unsend-step. Likewise.

(define-derived-mode prover-proof-mode prog-mode
  "proof"
  "major mode for proofs
\\{prover-proof-mode-map}"
  (prover--connect)
  nil)

(defvar prover--prover.rkt
  (expand-file-name "prover.rkt"
                    (file-name-directory (or load-file-name (buffer-file-name))))
  "Path to prover.rkt")

(defvar prover--process nil)

(defun prover--connect ()
  (prover--disconnect) ;in case already running
  (setq prover--process
        (start-process "prover"
                       (get-buffer-create " *prover process*")
                       "/usr/racket/bin/racket"  ;; FIXME
                       prover--prover.rkt)))

(defun prover--disconnect ()
  (when prover--process
    (delete-process prover--process)
    (setq prover--process nil)))

(defun prover--command (sexpr)
  (with-current-buffer (process-buffer prover--process)
    (delete-region (point-min) (point-max))
    (process-send-string prover--process
                         (format "%S\n" sexpr))
    (with-timeout (10 (error "prover process timeout"))
      (while (and (memq (process-status prover--process) '(open run))
                  (or (condition-case ()
                          (progn
                            (goto-char (point-min))
                            (forward-sexp 1)
                            (= (point) (point-min)))
                        (scan-error t))))
        (accept-process-output nil 0.05))
      (cond ((not (memq (process-status prover--process) '(open run)))
             (error "prover process: Died"))
            ((= (point-min) (point))
             (error "prover process: Empty response"))
            (t
             (let* ((str (buffer-substring (point-min) (point-max)))
                    (result (eval (read str))))
               (delete-region (point-min) (point-max))
               (pcase result
                 (`(response ,data ,goal)
                  (prover--draw-response data)
                  (prover--draw-goal goal)))))))))

(defun prover-send-step ()
  "Send the proof step, and mark it read-only."
  (interactive)
  (prover--command `(send ,(thing-at-point 'line)))
  ;; TODO: Make read-only, change appearance
  nil)

(defun prover-unsend-step ()
  "Unsend the proof step, and mark it read/write."
  (interactive)
  (prover--command `(unsend))
  ;; TODO: Make read/write, change appearance
  nil)

;;;

(define-derived-mode prover-responses-mode fundamental-mode
  "responses"
  "major mode for responses"
  nil)

(defconst prover--responses-buffer-name " *prover responses*"))

(defun prover--draw-response (sexpr)
  (let* ((buf (get-buffer-create prover--responses-buffer-name))
         (new? (not (get-buffer-window buf)))
         (win (or (get-buffer-window buf)
                  (let ((window-combination-limit t))
                    (set-window-buffer (split-window-vertically)
                                       prover--responses-buffer-name)))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "%S\n" sexpr)))))

;;;

(define-derived-mode prover-goal-mode fundamental-mode
  "goal"
  "major mode for responses"
  nil)

(defconst prover--goal-buffer-name " *prover goal*"))

(defun prover--draw-goal (sexpr)
  (let* ((buf (get-buffer-create prover--goal-buffer-name))
         (new? (not (get-buffer-window buf)))
         (win (or (get-buffer-window buf)
                  (let ((window-combination-limit t))
                    (set-window-buffer (split-window-vertically)
                                       prover--goal-buffer-name)))))
    (with-current-buffer buf
      (delete-region (point-min) (point-max)) ;only last goal
      (insert (format "%S\n" sexpr)))))
