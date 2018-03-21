;;; thread-dump.el --- Java thread dump viewer
;;
;; Author: Dmitry Neverov
;; URL: http://github.com/nd/thread-dump.el
;; Version: 1.0
;;
;; Code goes here

(defconst thread-dump-overview-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'thread-dump-overview-show-next-thread)
    (define-key map (kbd "j") 'thread-dump-overview-show-next-thread)
    (define-key map (kbd "p") 'thread-dump-overview-show-prev-thread)
    (define-key map (kbd "k") 'thread-dump-overview-show-prev-thread)
    (define-key map (kbd "RET") 'thread-dump-overview-show-thread)
    (define-key map (kbd "<mouse-1>") 'thread-dump-overview-show-thread)
    (define-key map (kbd "o") 'thread-dump-overview-show-thread)
    (define-key map (kbd "v") 'thread-dump-overview-visit-thread)
    (define-key map (kbd "h") 'thread-dump-overview-hide)
    (define-key map (kbd "H") 'thread-dump-overview-hide-with-same-stack)
    (define-key map (kbd "q") 'thread-dump-overview-quit)
    (define-key map (kbd "/") 'thread-dump-overview-filter)
    (define-key map (kbd "N") 'thread-dump-overview-open-next-dump)
    (define-key map (kbd "P") 'thread-dump-overview-open-prev-dump)
    map))

(defun thread-dump-overview-mode ()
  (buffer-disable-undo)
  (setq major-mode 'thread-dump-overview-mode
        mode-name "Thread-Dump-Overview")
  (use-local-map thread-dump-overview-mode-map)
  (run-hooks 'thread-dump-overview-mode-hook))


(defun thread-dump-open-dir (dir)
  (interactive "DThread dump directory: ")
  (let ((files (directory-files dir t directory-files-no-dot-files-regexp)))
    (thread-dump-open-files files)))


(defun thread-dump-open-file (file)
  (interactive "FThread dump: ")
  (thread-dump-open-files (list file)))


(defun thread-dump-open-files (files &optional file-index use-old-buffer)
  (interactive)
  (let* ((findex (or file-index 0))
         (file (nth findex files))
         (threads (with-temp-buffer
                    (insert-file-contents file)
                    (thread-dump-parse-current-buffer))))
    (when (not use-old-buffer)
      (let ((old (get-buffer "*thread-dump-overview*")))
        (when old (kill-buffer old))))
    (with-current-buffer (thread-dump-get-overview-buffer)
      (setq thread-dump-file file)
      (setq thread-dump-files files)
      (setq thread-dump-file-index findex)
      (setq header-line-format (list file)))
    (thread-dump-show-overview threads)
    (thread-dump-overview-mode)))


(defun thread-dump-show-overview (threads)
  (let* ((buf (thread-dump-get-overview-buffer)))
    (set-buffer buf)
    (let ((thread (thread-dump-get-thread-at-point)))
      (let ((inhibit-read-only t))
        (setq thread-dump-threads threads)
        (erase-buffer)
        (dolist (thread threads nil)
          (unless (or (thread-dump-get-thread-hidden thread)
                      (thread-dump-filtered-thread? thread))
            (thread-dump-show-thread-header thread)))
        (backward-delete-char 1))
      (goto-char (point-min))
      (switch-to-buffer buf)
      (setq buffer-read-only t)
      (goto-line
       (or (thread-dump-find-thread-line thread) ; stay on same thread if possible
           thread-dump-ow-cur-thread-line)) ; otherwise, go to same line
      (thread-dump-overview-visit-thread)
      (pop-to-buffer buf))))

(defun thread-dump-get-overview-buffer ()
  (let ((existing (get-buffer "*thread-dump-overview*")))
    (or existing
        (let ((new (get-buffer-create "*thread-dump-overview*")))
          (with-current-buffer new
            (make-variable-buffer-local 'thread-dump-ow-cur-thread-line)
            (make-variable-buffer-local 'thread-dump-filter)
            (make-variable-buffer-local 'thread-dump-threads)
            (make-variable-buffer-local 'thread-dump-file)
            (make-variable-buffer-local 'thread-dump-files)
            (make-variable-buffer-local 'thread-dump-file-index)
            (make-variable-buffer-local 'truncate-lines)

            (setq thread-dump-ow-cur-thread-line nil)
            (setq thread-dump-filter nil)
            (setq thread-dump-threads nil)
            (setq thread-dump-file nil)
            (setq thread-dump-files nil)
            (setq thread-dump-file-index nil)
            (setq truncate-lines t)
            new)
          ))))

(defun thread-dump-scrub-stack (stack)
  (and stack (replace-regexp-in-string "<[^>]+>" "<>" stack)))

(defun thread-dump-filtered-thread? (thread)
  (when thread-dump-filter
    (not (thread-dump-match thread-dump-filter thread))))

(defun thread-dump-overview-hide (&optional arg)
  (interactive "P")
  (if arg
      ;; unhide all threads
      (mapc (lambda (thread) (thread-dump-set-thread-hidden thread nil))
            thread-dump-threads)
    (thread-dump-set-thread-hidden (thread-dump-get-thread-at-point) t))
  (thread-dump-show-overview thread-dump-threads))

(defun thread-dump-overview-hide-with-same-stack (&optional arg)
  (interactive "P")
  (if arg
      ;; unhide all threads
      (mapc (lambda (thread) (thread-dump-set-thread-hidden thread nil))
            thread-dump-threads)
    ;; hide matching threads
    (let ((stack (thread-dump-get-thread-stack-scrubbed (thread-dump-get-thread-at-point))))
      (mapc
       (lambda (thread)
         (when (string= stack (thread-dump-get-thread-stack-scrubbed thread))
           (thread-dump-set-thread-hidden thread t)))
       thread-dump-threads)))
  (thread-dump-show-overview thread-dump-threads))

(defun thread-dump-overview-quit ()
  (interactive)
  (delete-other-windows)
  (bury-buffer))

(defun thread-dump-get-thread-face (thread)
  `(:foreground ,(hexrgb-hsv-to-hex (/ (float (random (thread-dump-get-thread-stack-scrubbed thread))) most-positive-fixnum) 1 1)))

(defun thread-dump-get-thread-state-face (thread-state)
  `(:foreground
    ,(cond
      ((string= thread-state "BLOCKED") "red")
      ((string= thread-state "RUNNABLE") "green")
      ((string= thread-state "WAITING") "blue")
      ((string= thread-state "TIMED_WAITING") "yellow")
      ((string= thread-state "TERMINATED") "gray")
      ((string= thread-state "NEW") "white")
      (t nil))))

(defun thread-dump-show-thread-header (thread)
  (insert (propertize
           (concat
            (propertize (thread-dump-get-thread-name thread)
                        'face (thread-dump-get-thread-face thread))
            
            "\t"
            (let ((state (thread-dump-get-thread-state thread)))
              (when state
                (propertize state
                            'face (thread-dump-get-thread-state-face state))))
            (let ((waiting-to-lock (thread-dump-get-thread-waiting-to-lock thread)))
              (when waiting-to-lock
                (concat "  on " (thread-dump-link-lock waiting-to-lock))))
            (let ((locks-held (thread-dump-get-thread-locks-held thread)))
              (when locks-held
                (concat " holds " (mapconcat 'thread-dump-pretty-lock locks-held ","))))"\n")
           'id (thread-dump-get-thread-id thread))))

(defun thread-dump-link-lock (lock)
  (lexical-let ((lock lock))
    (propertize
     (thread-dump-pretty-lock lock)
     'action
     (lambda (x)
       (let ((thread (thread-dump-find-thread-holding-lock lock)))
         (if thread
             (thread-dump-goto-thread thread)
           (message "Can't find thread holding lock"))))
     'button '(t)
     'category 'default-button
     'follow-link "\C-m"
     )))

(defun thread-dump-pretty-lock (lock)
  (let ((thread (thread-dump-find-thread-holding-lock lock)))
    (propertize (replace-regexp-in-string "0x0*" "" lock)
                'face (and thread (thread-dump-get-thread-face thread)))))

(defun thread-dump-goto-thread (thread)
  (let ((line (thread-dump-find-thread-line thread)))
    (if line
        (progn
          (goto-line line)
          (thread-dump-overview-visit-thread))
      (message "Thread is hidden"))))

(defun thread-dump-overview-next-thread ()
  (interactive)
  (unless (eq (point-max) (line-end-position))
    (forward-line)))

(defun thread-dump-overview-prev-thread ()
  (interactive)
  (unless (eq (point-min) (line-beginning-position))
    (forward-line -1)))

(defun thread-dump-overview-show-next-thread ()
  (interactive)
  (thread-dump-overview-next-thread)
  (thread-dump-overview-visit-thread))

(defun thread-dump-overview-show-prev-thread ()
  (interactive)
  (thread-dump-overview-prev-thread)
  (thread-dump-overview-visit-thread))

(defun thread-dump-overview-show-thread ()
  (interactive)
  (thread-dump-overview-visit-thread t))

(defun thread-dump-overview-visit-thread (&optional switch-to-details)
  (interactive)
  (thread-dump-highlight-thread)
  (let* ((thread (thread-dump-get-thread-at-point))
         (file thread-dump-file)
         (buf (get-buffer-create "*thread-dump-details*"))
         (filter thread-dump-filter)
         (inhibit-read-only t))
    (set-buffer buf)
    (erase-buffer)
    (set (make-local-variable 'truncate-lines) t)
    (insert (if thread (thread-dump-get-thread-contents thread) "No thread selected"))
    (goto-char (point-min))
    (when filter
      (while (re-search-forward filter nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'face 'highlight)))
    (and file (setq header-line-format (list file)))

    (let* ((w (get-buffer-window buf))
           (cur-win (selected-window)))
      (if (and w switch-to-details)
          (select-window w)
        (unless w
          (delete-other-windows cur-win)
          (let ((w (split-window-right 60)))
            (select-window w)
            (switch-to-buffer buf)
            (unless switch-to-details
              (select-window cur-win))))))))

(defun thread-dump-get-thread-at-point ()
  (let ((id (get-text-property (point) 'id)))
    (and id (thread-dump-find-thread-by-id id))))

(defun thread-dump-find-thread-line (thread)
  (save-excursion
    (beginning-of-buffer)
    (loop
     if (eq thread (thread-dump-get-thread-at-point)) return (line-number-at-pos (point))
     while (= 0 (forward-line))
     finally return nil)))

(defun thread-dump-highlight-thread ()
  (let ((inhibit-read-only t))
    (when thread-dump-ow-cur-thread-line
      (save-excursion
        (goto-line thread-dump-ow-cur-thread-line)
        (add-face-text-property (point-at-bol) (point-at-eol) '(:underline nil :weight normal))))
    (setq thread-dump-ow-cur-thread-line (line-number-at-pos))
    (add-face-text-property (point-at-bol) (point-at-eol) '(:underline t :weight bold))))

(defun thread-dump-overview-open-next-dump ()
  (interactive)
  (with-current-buffer (thread-dump-get-overview-buffer)
  (when (and thread-dump-files
             thread-dump-file-index
             (< thread-dump-file-index (- (length thread-dump-files) 1)))
    (thread-dump-open-files thread-dump-files (+ 1 thread-dump-file-index) 't))))

(defun thread-dump-overview-open-prev-dump ()
  (interactive)
  (with-current-buffer (thread-dump-get-overview-buffer)
    (when (and thread-dump-files
             thread-dump-file-index
             (> thread-dump-file-index 0))
    (thread-dump-open-files thread-dump-files (- thread-dump-file-index 1) 't))))


(defun thread-dump-find-thread-by-id (id)
  (find id
        thread-dump-threads
        :test '(lambda (x y) (= x (cdr (assoc 'id y))))))

(defun thread-dump-find-thread-holding-lock (lock)
  (find lock
        thread-dump-threads
        :test '(lambda (lock thread) (member lock (cdr (assoc 'locks-held thread))))))

(defun thread-dump-overview-filter (term)
  (interactive "MFilter: ")
  (setq thread-dump-filter (if (equal term "") nil term))
  (thread-dump-show-overview thread-dump-threads))

(defun thread-dump-match (term thread)
  (string-match term (thread-dump-get-thread-contents thread)))

(defun thread-dump-parse-current-buffer ()
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let ((threads (list))
            (thread-id 0))
        (while (re-search-forward "^\"" nil t)
          (move-beginning-of-line 1)
          (setq threads
                (cons (thread-dump-parse-thread-at-point thread-id) threads))
          (setq thread-id (+ thread-id 1)))

        (sort threads '(lambda (t1 t2)
                         (string< (downcase (thread-dump-get-thread-name t1))
                                  (downcase (thread-dump-get-thread-name t2)))))))))

(defun thread-dump-parse-thread-at-point (thread-id)
  (let* ((thread-start (point))
         (name-start (or (search-forward "\"" (line-end-position) t) thread-start))
         (name-end (or (- (search-forward "\"" (line-end-position) t) 1) (line-end-position)))
         (state (thread-dump-parse-thread-state-at-point))
         (stack-start (thread-dump-get-stack-start-at-point))
         (thread-end (if (re-search-forward "^\n" nil t) (line-beginning-position 1) (point-max)))
         (waiting-to-lock (thread-dump-parse-waiting-to-lock-in-range thread-start thread-end))
         (waiting-on (thread-dump-parse-waiting-on-in-range thread-start thread-end))
         ;; anything we are waiting on is not a held lock, so take it out of locs-held.
         (locks-held (remove waiting-on (thread-dump-parse-locks-held-in-range thread-start thread-end))))
    (goto-char thread-end)
    (list
       (cons 'id thread-id)
       (cons 'name (buffer-substring-no-properties name-start name-end))
       (cons 'start thread-start)
       (cons 'end thread-end)
       (cons 'contents (buffer-substring-no-properties thread-start thread-end))
       (cons 'state state)
       (cons 'stack (if stack-start (buffer-substring-no-properties stack-start thread-end) nil))
       (cons 'waiting-to-lock waiting-to-lock)
       (cons 'locks-held locks-held)
       (cons 'hidden nil))))

(defun thread-dump-parse-locks-held-in-range (start end)
  (goto-char start)
  (let (locks-held)
    (while (re-search-forward "- locked <\\([^>]+\\)>" end t)
      (push (buffer-substring-no-properties (match-beginning 1) (match-end 1))
            locks-held))
    locks-held))

(defun thread-dump-parse-waiting-to-lock-in-range (start end)
  (goto-char start)
  (if (re-search-forward "- waiting to lock <\\([^>]+\\)>" end t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun thread-dump-parse-waiting-on-in-range (start end)
  (goto-char start)
  (if (re-search-forward "- \\(?:waiting on \\|parking to wait for *\\)<\\([^>]+\\)>" end t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun thread-dump-parse-thread-state-at-point ()
  (if (re-search-forward "java.lang.Thread.State: \\b\\([a-zA-Z_]+\\)\\b" (line-end-position 2) t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    nil))

(defun thread-dump-get-stack-start-at-point ()
  (if (re-search-forward "^\\( \\|\t\\)*at" (line-end-position 2) t)
      (line-beginning-position 1)
    nil))

(defun thread-dump-get-thread-name (thread)
  (cdr (assoc 'name thread)))

(defun thread-dump-get-thread-id (thread)
  (cdr (assoc 'id thread)))

(defun thread-dump-get-thread-contents (thread)
  (cdr (assoc 'contents thread)))

(defun thread-dump-get-thread-state (thread)
  (cdr (assoc 'state thread)))

(defun thread-dump-get-thread-waiting-to-lock (thread)
  (cdr (assoc 'waiting-to-lock thread)))

(defun thread-dump-get-thread-locks-held (thread)
  (cdr (assoc 'locks-held thread)))

(defun thread-dump-get-thread-stack (thread)
  (cdr (assoc 'stack thread)))

(defun thread-dump-get-thread-stack-scrubbed (thread)
  (thread-dump-scrub-stack (cdr (assoc 'stack thread))))

(defun thread-dump-get-thread-hidden (thread)
  (cdr (assoc 'hidden thread)))

(defun thread-dump-set-thread-hidden (thread hidden)
  (setf (cdr (assoc 'hidden thread)) hidden))



(defface thread-dump-current-thread
  '((t :underline t
       :weight bold))
  "Current thread face."
  :group 'thread-dump-faces)



(provide 'thread-dump)

;;; thread-dump.el ends here
