;;; shaoline-segments.el --- Standard segments for shaoline -*- lexical-binding: t -*-

(require 'shaoline-msg-engine)
(eval-when-compile
  (defvar shaoline-enable-dynamic-segments t "Docstring for eval-when-compile shadowing."))

;;; Buffer icon & name
(shaoline-define-segment shaoline-segment-icon-and-buffer (buffer)
  "Цветная иконка буфера (по режиму, как во вкладках) + имя буфера."
  (let* ((icon
          (when (and shaoline-enable-dynamic-segments (featurep 'all-the-icons))
            (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
            (let* ((mode (buffer-local-value 'major-mode buffer))
                   (raw (all-the-icons-icon-for-mode mode :height 0.9)))
              (cond
               ((stringp raw) raw)
               ((buffer-file-name buffer)
                (all-the-icons-icon-for-file
                 (buffer-file-name buffer) :height 0.9))
               (t (all-the-icons-faicon "file-o" :height 0.9))))))
         (name (buffer-name buffer))
         (text (if icon (concat icon " " name) name)))
    (add-face-text-property
     (if icon (length icon) 0) (length text)
     'shaoline-buffer-face 'append
     text)
    text))

;;; Project name
(shaoline-define-simple-segment shaoline-segment-project-name
  "Project name, if available."
  (unless (featurep 'projectile) (require 'projectile nil t))
  (let* ((project
          (cond
           ((and (featurep 'projectile) (projectile-project-name))
            (projectile-project-name))
           ((fboundp 'project-current)
            (when-let ((pr (project-current)))
              (file-name-nondirectory (directory-file-name (car (project-roots pr))))))
           (t nil))))
    (when (and project (not (string= "-" project)))
      (propertize project 'face 'shaoline-project-face))))

;;; Git branch
(shaoline-define-simple-segment shaoline-segment-git-branch
  "Current Git branch."
  (when (and (featurep 'vc-git) (buffer-file-name))
    (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
      (when branch
        (concat
         (when (and shaoline-enable-dynamic-segments (featurep 'all-the-icons))
           (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
           (all-the-icons-octicon "git-branch" :v-adjust 0 :height 1.0 :face 'shaoline-git-face))
         " "
         (propertize branch 'face 'shaoline-git-face))))))

;;; Echo-area message
(shaoline-define-simple-segment shaoline-segment-echo-message
  "Show the most recent user `message' for a fixed duration.
The segment itself no longer worries about width – `shaoline-compose-modeline'
will truncate it if necessary."
  (let ((show (and (shaoline-msg-active-p shaoline-message-timeout)
                   (shaoline-msg-current))))
    (if show
        (propertize show 'face 'shaoline-echo-face)
      "")))

;;; Battery
(shaoline-define-simple-segment
 shaoline-segment-battery
 "Show battery percentage and charging status (Dao minimalist, safe).
Wanders the Way: returns 'N/A' if nothing is known, but never leaves Emacs disturbed."
 (if (not shaoline-enable-dynamic-segments)
     ""  ;; Пустая строка, если отключено — Wu Wei: ничего не делать
   (unless (featurep 'battery) (require 'battery nil t))
   (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
   (let ((safe-n-a
          (propertize
           (if (featurep 'all-the-icons)
               (concat (all-the-icons-faicon "battery-empty" :face 'shaoline-battery-face :v-adjust 0 :height 0.75) " N/A")
             "N/A")
           'face '(:inherit shaoline-battery-face :slant italic))))
     (condition-case nil
         (if (and (fboundp 'battery) battery-status-function)
             (with-timeout (0.7 safe-n-a)
               (let ((data (funcall battery-status-function)))
                 (cond
                  ((and (listp data) (cl-every #'consp data))
                   (let* ((percent (or (cdr (assoc 112 data))   ; ?p
                                       (cdr (assoc "percentage" data))
                                       (cdr (assoc "perc" data))
                                       (cdr (assoc "capacity" data))))
                          (status (or (cdr (assoc 66 data))     ; ?b
                                      (cdr (assoc "status" data))
                                      (cdr (assoc "charging" data))
                                      (cdr (assoc "state" data))))
                          (icon
                           (cond
                            ((not (featurep 'all-the-icons)) "")
                            ((and percent (string-match "\\([0-9]+\\)" percent))
                             (let* ((n (string-to-number (match-string 1 percent)))
                                    (icon-face
                                     (cond
                                      ((and status (string-match-p "\\`discharging\\'" status) (< n 25)) '(:foreground "#aa0000"))
                                      ((and status (string-match-p "\\`charging\\'" status)) '(:foreground "#007700"))
                                      (t 'shaoline-battery-face))))
                               (cond
                                ((>= n 90) (all-the-icons-faicon "battery-full" :face icon-face :v-adjust 0 :height 0.75))
                                ((>= n 70) (all-the-icons-faicon "battery-three-quarters" :face icon-face :v-adjust 0 :height 0.75))
                                ((>= n 40) (all-the-icons-faicon "battery-half" :face icon-face :v-adjust 0 :height 0.75))
                                ((>= n 10) (all-the-icons-faicon "battery-quarter" :face icon-face :v-adjust 0 :height 0.75))
                                (t          (all-the-icons-faicon "battery-empty" :face icon-face :v-adjust 0 :height 0.75)))))
                            ((and status (string-match-p "full" status)) (all-the-icons-faicon "battery-full" :face 'shaoline-battery-face :v-adjust 0 :height 0.75))
                            ((and status (string-match-p "\\<ac\\>" status)) (all-the-icons-octicon "plug" :face 'shaoline-battery-face))
                            ((and status (string-match-p "charging" status)) (all-the-icons-faicon "bolt" :face '(:foreground "#007700") :v-adjust 0))
                            ((and status (string-match-p "discharging" status)) (all-the-icons-faicon "battery-empty" :face '(:foreground "#aa0000") :v-adjust 0 :height 0.75))
                            (t ""))))
                     (if percent
                         (concat
                          (if (and (stringp icon) (not (string-empty-p icon))) (concat icon " "))
                          (propertize (concat (replace-regexp-in-string "%" "" percent) "%")
                                      'face 'shaoline-battery-face))
                       (propertize
                        (if (featurep 'all-the-icons)
                            (concat (all-the-icons-faicon "battery-empty" :face 'shaoline-battery-face :v-adjust 0 :height 0.75) " No battery")
                          "No battery")
                        'face '(:inherit shaoline-battery-face :slant italic)))))
                  ((and (stringp data) (not (string-empty-p data)))
                   (propertize data 'face 'shaoline-battery-face))
                  (t safe-n-a))))
           safe-n-a)
       (error safe-n-a)))))

;;; Major-mode with icon
(shaoline-define-simple-segment shaoline-segment-major-mode
  "Major mode segment with icon."
  (let ((icon
         (when (and shaoline-enable-dynamic-segments (featurep 'all-the-icons) major-mode)
           (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
           (all-the-icons-icon-for-mode major-mode :height 0.9))))
    (concat
     (when (and icon (stringp icon))
       (concat icon " "))
     (propertize (format-mode-line mode-name)
                 'face 'shaoline-mode-face))))

;;; Time + Moon Phase
(defun shaoline--moon-phase-idx (&optional date)
  "Return moon phase index 0..7 for DATE."
  (unless (featurep 'calendar) (require 'calendar nil t))
  (let* ((d        (or date (calendar-current-date)))
         (abs-day  (float (calendar-absolute-from-gregorian d)))
         (synodic  29.530588853)
         (age      (- abs-day (* (floor (/ abs-day synodic)) synodic)))
         (idx      (mod (floor (* age (/ 8.0 synodic))) 8)))
    idx))

(shaoline-define-simple-segment shaoline-segment-time
  "Current time with moon phase."
  (if (not shaoline-enable-dynamic-segments)
      ""  ;; Пустая строка, если отключено
    (let* ((time (propertize (format-time-string " %H:%M ") 'face 'shaoline-time-face))
           (phase-number (shaoline--moon-phase-idx))
           (phases ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
           (moon (propertize (aref phases phase-number) 'face 'shaoline-moon-face)))
      (concat time " " moon "  "))))

;;; Segment: emptiness (visual spacer, Daoist non-doing)
(shaoline-define-simple-segment shaoline-segment-emptiness
  "The emptiest possible segment – a blank."
  " ")

(provide 'shaoline-segments)
;;; shaoline-segments.el ends here
