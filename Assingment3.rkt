#lang racket

(define (parse-accounts filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((accounts '()))
        (let ((line (read-line)))
          (cond
            ((eof-object? line) (reverse accounts))
            (else
             (let* ((match-result (regexp-match #rx"^\\s*(\\d+)\\s+\"([^\"]+)\"\\s+(\\d+(\\.\\d+)?)\\s*$" line))
                    (account-number (if match-result (list-ref match-result 1) ""))
                    (account-name (if match-result (list-ref match-result 2) ""))
                    (account-balance (if match-result (string->number (list-ref match-result 3)) 0)))
               (loop (cons (list account-number account-name account-balance) accounts))))))))))

(define (parse-transactions filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((transactions '())
                 (counter 10001))
        (let ((line (read-line)))
          (cond
            ((eof-object? line) (reverse transactions))
            (else
             (let* ((parts (regexp-split #rx"\t+" line)) ; Split based on tabs
                    (transaction-type (car parts))
                    (account-number (cadr parts))
                    (transaction-id (caddr parts))
                    (merchant (cadddr parts))
                    (amount (string->number (cadr (reverse parts))))) ; Get the last part as amount
               (loop (cons (list counter transaction-type account-number transaction-id merchant amount) transactions)
                      (add1 counter))))))))))
  


(define (process-transactions accounts transactions)
  (define (get-account name)
    (let ((account (assoc name accounts)))
      (if account
          account
          (error "Account not found"))))

  (define (format-transaction transaction)
    (define (truncate str max-len)
      (if (> (string-length str) max-len)
          (substring str 0 max-len)
          str))
    (let* ((counter (car transaction))
           (type (cadr transaction))
           (account-number (caddr transaction))
           (info (cadddr transaction))
           (amount (caddr (reverse transaction))))
      (cond
        ((string=? type "Purchase")
         (list (format "~a" counter)
               (format "~a" account-number)
               (truncate info 40)
               (format "~a" amount)))
        ((string=? type "Payment")
         (list (format "~a" counter)
               (format "~a" account-number)
               (format "~a" info)
               (format "~a" amount))))))

  (define (process-account account)
    (define (get-transactions-for-account acc-number)
      (filter (lambda (transaction)
                (string=? acc-number (cadr transaction)))
              transactions))

    (define (format-amounts amounts)
      (apply + (map (lambda (transaction)
                      (if (string=? "Purchase" (cadr transaction))
                          (caddr (reverse transaction))
                          (- (caddr (reverse transaction)))))
                    amounts)))

    (let* ((account-number (car account))
           (starting-balance (caddr account))
           (acc-transactions (get-transactions-for-account account-number))
           (processed-transactions (map format-transaction acc-transactions))
           (purchase-amounts (filter (lambda (x) (string=? "Purchase" (cadr x))) acc-transactions))
           (payment-amounts (filter (lambda (x) (string=? "Payment" (cadr x))) acc-transactions))
           (total-purchases (format-amounts purchase-amounts))
           (total-payments (format-amounts payment-amounts))
           (ending-balance (+ starting-balance total-purchases (- total-payments))))
      (cons (list account-number starting-balance)
            (cons processed-transactions
                  (list (list "Total purchases:" (format "~a" total-purchases))
                        (list "Total payments:" (format "~a" total-payments))
                        (list "Ending balance:" (format "~a" ending-balance)))))))

  ;; Fix: Sort accounts by the first element of each sublist (assumed to be the account number)
  (let* ((sorted-accounts (sort accounts (lambda (a b) (string<? (car a) (car b)))))
         (processed-accounts (map process-account sorted-accounts)))
    (with-output-to-file "STATEMENTS.txt"
      (lambda ()
        (for-each (lambda (account)
                    (for-each display (car account))
                    (newline)
                    (for-each (lambda (transaction)
                                (for-each display transaction)
                                (newline))
                              (cadr account))
                    (for-each display (caddr account))
                    (newline)
                    (newline))
                  processed-accounts)))))


;; Usage
(define accounts (parse-accounts "ACCOUNTS.txt"))
(define transactions (parse-transactions "TRANSACTIONS.txt"))
(process-transactions accounts transactions)




