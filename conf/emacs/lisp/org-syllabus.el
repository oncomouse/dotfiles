;;; org-syllabus.el --- Manage syllabuses using org
;;; Commentary:
;;; Code:

(defun compile-holidays (y1 &optional y2 l label)
  "Display holidays for years Y1 to Y2 (inclusive).
Y2 defaults to Y1.  The optional list of holidays L defaults to
`calendar-holidays'.  If you want to control what holidays are
displayed, use a different list.  For example,

  (list-holidays 2006 2006
    (append holiday-general-holidays holiday-local-holidays))

will display holidays for the year 2006 defined in the two
mentioned lists, and nothing else.

When called interactively, this command offers a choice of
holidays, based on the variables `holiday-solar-holidays' etc.  See the
documentation of `calendar-holidays' for a list of the variables
that control the choices, as well as a description of the format
of a holiday list.

The optional LABEL is used to label the buffer created.

The list of holiday lists is computed by the
`holiday-available-holiday-lists' and you can alter the results
by redefining that function, or use `add-function' to add
values."
  (interactive
   (let* ((start-year (calendar-read-sexp
                       "Starting year of holidays (>0)"
                       (lambda (x) (> x 0))
                       (calendar-extract-year (calendar-current-date))))
          (end-year (calendar-read-sexp
                     "Ending year (inclusive) of holidays (>=%s)"
                     (lambda (x) (>= x start-year))
                     start-year
                     start-year))
          (completion-ignore-case t)
          (lists (holiday-available-holiday-lists))
          (choice (capitalize
                   (completing-read "List (TAB for choices): " lists nil t)))
          (which (if (string-equal choice "Ask")
                     (symbol-value (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays"
                    (format "%s Holidays" choice)))))
     (list start-year end-year which name)))
  (unless y2 (setq y2 y1))
  (message "Computing holidays...")
  (let ((calendar-holidays (or l calendar-holidays))
        (title (or label "Holidays"))
        (s (calendar-absolute-from-gregorian (list 2 1 y1)))
        (e (calendar-absolute-from-gregorian (list 11 1 y2)))
        (displayed-month 2)
        (displayed-year y1)
        holiday-list)
    (while (<= s e)
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (calendar-increment-month displayed-month displayed-year 3)
      (setq s (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    holiday-list))

(defcustom org-syllabus/holidays '((ap/filter-holidays ap/tamu-holidays))
  "List of holidays. Defaults to '().")

(defun org-syllabus/parse-meets ()
  "Parse M, a comma-separated string of day names, into a list of day numbers."
  (sort (mapcar (-compose (lambda (x) (seq-position calendar-day-name-array x))
                          'capitalize)
                (s-split "," (cdr (or (assoc "meets" org-macro-templates) (cons "meets" "monday,wednesday,friday")))))
        '<))

(defun org-syllabus/parse-macro-date (date-key)
  (let ((start-date (org-date-to-gregorian (cdr (assoc date-key org-macro-templates)))))
    (when (null (car start-date))
      (setq start-date (calendar-current-date)))
    start-date))

(defun org-syllabus/parse-start-date ()
  ""
  (org-syllabus/parse-macro-date "start_date"))

(defun org-syllabus/parse-end-date ()
  ""
  (org-syllabus/parse-macro-date "end_date"))

(defun org-syllabus/generate-course-days ()
  (let ((start-date (org-syllabus/parse-start-date))
        (end-date (org-syllabus/parse-end-date))
        (meeting-days (org-syllabus/parse-meets))
        (holidays '()))
    (setq holidays (compile-holidays (calendar-extract-year start-date) (calendar-extract-year end-date) org-syllabus/holidays))
    ))

(defun org-syllabus/parse-dates ()
  "Parse dates for syllabus."
  (let ((count 1)
        (course-days (org-syllabus/generate-course-days)))
    (org-element-map (org-element-parse-buffer) 'timestamp
      (lamdba (ts) 'identity))))

(provide 'org-syllabus)
;;; org-syllabus.el ends here
