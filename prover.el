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

(defvar prover--current-point 1)
(defvar prover--process nil)

(defvar prover-read-only-face 'highlight
  "Face for read-only region")

(defun prover--clear-read-only (start end)
  (let ((inhibit-read-only t))
    (remove-overlays start end 'face prover-read-only-face)
    (remove-text-properties start end '(read-only nil))))

(defun prover--connect ()
  (prover--disconnect) ;; in case already running
  (if (get-buffer prover--responses-buffer-name)
      (kill-buffer prover--responses-buffer-name))
  (if (get-buffer prover--goal-buffer-name)
      (kill-buffer prover--goal-buffer-name))
  (setq prover--current-point (point-min))
  (prover--clear-read-only (point-min) (point-max))
  (setq prover--process
        (start-process "prover"
                       (get-buffer-create " *prover process*")
                       ;; FIXME
                       "/Users/jay/Dev/scm/plt/racket/bin/racket"  
                       prover--prover.rkt)))

(defun prover-restart ()
  "Restart the prover process"
  (interactive)
  (prover--connect))

(defun prover--disconnect ()
  (when prover--process
    (delete-process prover--process)
    (setq prover--process nil)))

(defun prover--command (label arg)
  (with-current-buffer (process-buffer prover--process)
    (delete-region (point-min) (point-max))
    (process-send-string prover--process (format "%S\n" `(,label ,arg)))
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
                  (prover--draw-response label arg data)
                  (prover--draw-goal goal)))))))))

;; Copied from http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
(defun j-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun j-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun j-trim (s)
  "Remove whitespace at the beginning and end of S."
  (j-trim-left (j-trim-right s)))

(defun prover-send-step ()
  "Send the proof step, and mark it read-only."
  (interactive)
  (let* ((start prover--current-point)
         (end (scan-sexps start +1)))
    (when end
      (goto-char end)
      (let* ((inhibit-read-only t)
             (x (make-overlay start end)))
        (overlay-put x 'face prover-read-only-face)
        (put-text-property start (- end 1) 'read-only "read-only: Sent to prover"))
      (setq prover--current-point end)
      (prover--command 'send (j-trim (buffer-substring-no-properties start end)))))
  nil)

(defun prover-unsend-step ()
  "Unsend the proof step, and mark it read/write."
  (interactive)
  (let* ((end prover--current-point)
         (start (scan-sexps end -1)))
    (when start
      (prover--command 'unsend "")
      (goto-char start)
      (prover--clear-read-only start end)
      (setq prover--current-point start)))
  nil)

;;;

(define-derived-mode prover-responses-mode fundamental-mode
  "responses"
  "major mode for responses"
  nil)

(defconst prover--responses-buffer-name " *prover responses*")

(defun prover--draw-response (label arg prover-sexpr)
  (let* ((buf (get-buffer-create prover--responses-buffer-name))
         (new? (not (get-buffer-window buf)))
         (win (or (get-buffer-window buf)
                  (let ((window-combination-limit t))
                    (set-window-buffer (split-window-vertically)
                                       prover--responses-buffer-name)))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (pcase label
                (`send (format "> %s\n" arg))
                (`unsend "<\n")))
      (insert (format "%s\n" prover-sexpr)))))

;;;

(define-derived-mode prover-goal-mode fundamental-mode
  "goal"
  "major mode for responses"
  nil)

(defconst prover--goal-buffer-name " *prover goal*")

(defun prover--draw-goal (new-goal)
  (let* ((buf (get-buffer-create prover--goal-buffer-name))
         (new? (not (get-buffer-window buf)))
         (win (or (get-buffer-window buf)
                  (let ((window-combination-limit t))
                    (set-window-buffer (split-window-vertically)
                                       prover--goal-buffer-name)))))
    (with-current-buffer buf
      ;; only last goal
      (delete-region (point-min) (point-max))
      (insert new-goal))))

(global-set-key (kbd "M-s-≤") 'prover-unsend-step)
(global-set-key (kbd "M-s-≥") 'prover-send-step)
