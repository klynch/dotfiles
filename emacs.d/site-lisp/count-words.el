    ;; Count the words in the highlighted text region
    (defun count-words-region (beginning end)
      "Print number of words in the region."
      (interactive "r")
      (message "Counting words in region... ")

      ;; 1. Set up appropriate conditions.
      (save-excursion
        (let ((count 0))
          (goto-char beginning)

          ;; 2. Run the while loop.
          (while (and (< (point) end)
                      (re-search-forward "\\w+\\W*" end t))
            (setq count (1+ count)))

          ;; 3. Send a message to the user.
          (cond ((zerop count)
                 (message
                  "The region does NOT have any words."))
                ((= 1 count)
                 (message
                  "The region has 1 word."))
                (t
                 (message
                  "The region has %d words." count))))))


    ;; Count the words in the page.
    (defun count-words-page ()
      "Print number of words in the page."
      (interactive)
      (save-excursion
        (push-mark (point-min) t t)
        (goto-char (point-max))
        (count-words-region (mark) (point))))

(provide 'count-words)
