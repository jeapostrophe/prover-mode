;;; Library

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

;;; Main mode

(make-variable-buffer-local (defvar prover--current-point 1))
(make-variable-buffer-local (defvar prover--process nil))

(defvar prover-read-only-face 'highlight
  "Face for read-only region")
(defvar prover-cmd
  "/bin/cat"
  "Command-line for prover process")

(defun prover--read-only-hook (overlay after start end &optional len)
  (unless inhibit-read-only
    (error "read-only: Sent to prover")))
(defun prover--clear-read-only (start)
  (let ((inhibit-read-only t))
    (remove-overlays start (point-max)
                     'face prover-read-only-face)
    (remove-overlays start (point-max)
                     'modification-hooks '(prover--read-only-hook))
    (remove-overlays start (point-max)
                     'insert-in-front-hooks '(prover--read-only-hook))))
(defun prover--mark-read-only (end)
  (let ((inhibit-read-only t)
        (x (make-overlay (point-min) end)))
    (prover--clear-read-only (point-min))
    (overlay-put x 'face prover-read-only-face)
    (overlay-put x 'modification-hooks '(prover--read-only-hook))
    (overlay-put x 'insert-in-front-hooks '(prover--read-only-hook))))

;; TODO show message on stderr in responses
;; TODO send file name and position for srcloc

(defun prover-restart ()
  "Restart the prover process"
  (interactive)
  (prover--disconnect) ;; in case already running
  (if (get-buffer prover--responses-buffer-name)
      (kill-buffer prover--responses-buffer-name))
  (if (get-buffer prover--goal-buffer-name)
      (kill-buffer prover--goal-buffer-name))
  (setq prover--current-point (point-min))
  (prover--clear-read-only (point-min))
  (setq prover--process
        (start-process-shell-command
         "prover"
         (get-buffer-create "*prover process*")
         prover-cmd))
  (prover-layout)
  (prover--draw-response-raw
   (prover--slurp)))

(defun prover--disconnect ()
  (when prover--process
    (let ((buf (process-buffer prover--process)))
      (delete-process prover--process)
      (with-current-buffer buf
        (delete-region (point-min) (point-max)))
      (message "deleting prover--process")
      (setq prover--process nil))))

(defun prover--slurp ()
  (cond
   (prover--process
    (with-current-buffer (process-buffer prover--process)
      (with-timeout (10 (error "prover process timeout"))
        (while (and (memq (process-status prover--process) '(open run))
                    (or (condition-case ()
                            (progn
                              (goto-char (point-min))
                              (forward-sexp 1)
                              (= (point) (point-min)))
                          (scan-error t))))
          (accept-process-output nil 0.05))
        (format "%s%s"
                (buffer-substring (point-min) (point-max))
                (if (not (memq (process-status prover--process) '(open run)))
                    "\nprover process: Died"
                  "")))))
   (t
    "prover process: does not exist")))

(defun prover--command (label arg)
  (unless prover--process
    (prover-restart))
  (with-current-buffer (process-buffer prover--process)
    (delete-region (point-min) (point-max))
    (process-send-string prover--process (format "%S\n" `(,label ,arg))))  
  (let* ((str (prover--slurp))
         (result
          (condition-case ()
              (eval (read str))
            (error `(response ,str ,"")))))
    (pcase result
      (`(response ,data ,goal)
       (prover--draw-response label arg data)
       (prover--draw-goal goal)))))

(defun prover-send-step ()
  "Send the proof step, and mark it read-only."
  (interactive)
  (let* ((start prover--current-point)
         (end (scan-sexps start +1)))
    (when end
      (goto-char end)
      (prover--mark-read-only end)
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
      (prover--clear-read-only start)
      (setq prover--current-point start)))
  nil)

(defun prover-goto-point ()
  "Send or unsend proof steps until you get to this point"
  (interactive)
  (let* ((start prover--current-point)
         (dest (point)))
    (cond
     ((< start dest)
      (let ((last-pt (point)))
        (prover-send-step)
        (while (and (not (equal last-pt (point)))
                    (< (point) dest))
          (setq last-pt (point))
          (prover-send-step))))
     ((< dest start)
      (let ((last-pt (point)))
        (prover-unsend-step)
        (while (and (not (equal last-pt (point)))
                    (< dest (point)))
          (setq last-pt (point))
          (prover-unsend-step)))))))

;;;###autoload
(define-minor-mode prover-proof-mode
  "minor mode for proofs"
  :lighter " prover"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-s-÷") 'prover-goto-point)
            (define-key map (kbd "M-s-π") 'prover-goto-point)
            (define-key map (kbd "M-s-≤") 'prover-unsend-step)
            (define-key map (kbd "M-s-≥") 'prover-send-step)
            map)
  (prover-restart))

(provide 'prover-proof-mode)

;;; Response Log

;;;###autoload
(define-derived-mode prover-responses-mode fundamental-mode
  "responses"
  "major mode for responses"
  nil)
(defconst prover--responses-buffer-name " *prover responses*")
(defun prover--draw-response-raw (text)
  (let* ((buf (get-buffer-create prover--responses-buffer-name))
         (new? (not (get-buffer-window buf)))
         (win (or (get-buffer-window buf)
                  (let ((window-combination-limit t))
                    (set-window-buffer (split-window-vertically)
                                       prover--responses-buffer-name)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setq scroll-conservatively 100)
        (setq buffer-read-only t)
        (goto-char (point-max))
        (insert text)))
    (with-selected-window win
      (goto-char (point-max)))))
(defun prover--draw-response (label arg prover-sexpr)
  (prover--draw-response-raw
   (format "%s\n%s\n"
           (pcase label
             (`send (format "> %s" arg))
             (`unsend "<"))
           prover-sexpr)))

(provide 'prover-responses-mode)

;;; Goal

;;;###autoload
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
      (let ((inhibit-read-only t))
        (setq buffer-read-only t)
        ;; only last goal
        (delete-region (point-min) (point-max))
        (insert new-goal)))))

;;; Helper

(defun prover-layout ()
  "Layout the prover windows"
  (interactive)
  (delete-other-windows)
  (if (< (window-body-width) (* 2 80))
      (split-window-vertically)
    (split-window-horizontally))
  (switch-to-buffer (first (buffer-list)))
  (other-window 1)
  (split-window-vertically)
  (switch-to-buffer prover--goal-buffer-name)
  (other-window 1)
  (switch-to-buffer prover--responses-buffer-name)
  (other-window 1))

(provide 'prover-goal-mode)
