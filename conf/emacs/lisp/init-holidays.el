;;; init-holidays.el --- Holiday configuration
;;; Commentary:
;;; Code:

(defun ap/define-holiday (month day year &optional name)
  (list (list month day year) (or name "")))

(defcustom ap/tamu-holidays
  (list (ap/define-holiday 9 2 2024 "Labor Day")
        (ap/define-holiday 10 7 2024 "Fall Break")
        (ap/define-holiday 10 8 2024 "Fall Break")
        (ap/define-holiday 11 27 2024 "Reading Day")
        (ap/define-holiday 11 28 2024 "Thanksgiving Break")
        (ap/define-holiday 11 29 2024 "Thanksgiving Break")
        (ap/define-holiday 1 20 2025 "MLK Day")
        (ap/define-holiday 3 10 2025 "Spring Break")
        (ap/define-holiday 3 11 2025 "Spring Break")
        (ap/define-holiday 3 12 2025 "Spring Break")
        (ap/define-holiday 3 13 2025 "Spring Break")
        (ap/define-holiday 3 14 2025 "Spring Break")
        (ap/define-holiday 4 18 2025 "Reading Day"))
  "List of days Texas A&M has no classes.")

(defcustom ap/daycare-closed
  (list)
  "List of days the daycare is closed.")

(defun calendar-in-range-p (d1 d2 x)
  "Is date X between the dates D1 and D2?"
  (and (calendar-date-compare (list d1) x) (null (calendar-date-compare (list d2) x))))

(defun ap/filter-holidays (holidays)
  (let ((m1 displayed-month)
        (m2 displayed-month)
        (y1 displayed-year)
        (y2 displayed-year))
    (calendar-increment-month m1 y1 -1)
    (calendar-increment-month m2 y2 1)
    (let ((d1 (list m1 1 y1)) (d2 (list m2 (calendar-last-day-of-month m2 y2) y2)))
      (cl-remove-if 'null (mapcar (lambda (x)
                                    (if (calendar-in-range-p d1 d2 x)
                                        x
                                      nil)
                                    )
                                  holidays)))))

;; Disable unused holidays:
(setq
 holiday-hebrew-holidays nil
 holiday-bahai-holidays nil
 holiday-islamic-holidays nil
 holiday-oriental-holidays nil)
;; Attach our custom holiday lists:
(setq holiday-other-holidays
      '((ap/filter-holidays ap/tamu-holidays)
        (ap/filter-holidays ap/daycare-closed)))
;; This gets overwritten somehow:
(setq calendar-holidays (append holiday-general-holidays holiday-local-holidays
                                holiday-other-holidays holiday-christian-holidays
                                holiday-hebrew-holidays holiday-islamic-holidays
                                holiday-bahai-holidays holiday-oriental-holidays
                                holiday-solar-holidays))

(require 'holidays)

(with-eval-after-load 'org
  (setq org-agenda-include-diary t))

(provide 'init-holidays)
;;; init-holidays.el
