#lang racket
(require racket/pretty)
(require racket/system)

(define (flatlist . a) (flatten a))
(define (quote-str str) (string-append  "'" str "'"))
(define (dbl-quote-str str) (string-append  "\"" str "\""))
(define (str-join . strings) (string-join strings))


;; Interleave two lists
(define (interleave a b)
  (if (null? a) b (cons (car a) (interleave b (cdr a)))))


;; Turn a cf-item function call into a hash table
(define (cf-item->hash caller-name kws kw-args . rest)
  (let ([keys (cons 'type  (map (lambda (x) (string->symbol (keyword->string x))) kws))]
        [vals  (cons caller-name kw-args)])
    (apply hash (interleave keys vals))))


;; Group a list by one of their properies then convert
;; to a hash table indexed by that property
(define (list->grouped-hash items key)
  (apply hash (foldr (λ (g acc) (cons (hash-ref (car g) key) (cons g acc)))
                     '()
                     (group-by (λ (i) (hash-ref i key)) items))))


;; Define new syntax for creating new cf-item types
(define-syntax-rule (define-cf-item name)
  (define name (make-keyword-procedure ((curry cf-item->hash) 'name))))


;; Define cf-item types
(define-cf-item aws-credential)
(define-cf-item aws-account)
(define-cf-item environment)
(define-cf-item stack-template)
(define-cf-item stack-group)
(define-cf-item stack-config)
(define-cf-item stack-param)
(define-cf-item stack-warning)


(define (eval-cft-syntax namespace filepath file-port)
  (for/list ([stx (in-port (λ (port) (read-syntax filepath port)) file-port)])
    (eval stx namespace)))


;; Parse a .cft into hash sets of cf-items
(define (eval-config namespace filepath)
  (list->grouped-hash
   (call-with-input-file filepath
     ((curry eval-cft-syntax) namespace filepath)) 'type))


;; Select unique item from list with matching id
(define (select-list items id)
  (let* ([filtered (filter (λ (x) (equal? (hash-ref x 'id) id)) items)]
         [len (length filtered)])
    (cond
      [(empty? filtered) (raise (format "Cannot find '~v'" id))]
      [(= 1 (length filtered)) (car filtered)]
      [else (raise (format "'~v' is not unique" id))])))


;; Select item from list matching selector
(define (select items selector)
  (cond
    [(list? items) (select-list items selector)]
    [(hash? items) (hash-ref items selector)]))


;; Select recursively
(define (selectr items selectors)
  (if (empty? selectors)
      items
      (selectr (select items (car selectors)) (cdr selectors))))


(define (exec-cmd cmd args)
  (let-values ([(sp out in err)
                (apply ((curry subprocess) #f #f #f (find-executable-path cmd)) args)])
    (subprocess-wait sp)
    (let ([status (subprocess-status sp)]
          [stdout (port->string out)]
          [stderr (port->string err)])
      (close-input-port out)
      (close-output-port in)
      (close-input-port err)
      (values status stdout stderr))))


(define (aws-cli cmd subcmd args dry-run)
  (if dry-run
      (exec-cmd "echo" (flatlist "aws" cmd subcmd args))
      (exec-cmd "aws" (flatlist cmd subcmd args))))


(define (stack-exists region stack-name)
  (let*-values
      ([(args) (list "--region" region
                     "--stack-name" stack-name)]
       [(status stdout stderr) (aws-cli "cloudformation" "describe-stacks" args)])
    (= status 0)))


(define (fetch-aws-account-id)
  (let*-values
      ([(args) (list "--query" "Account"
                     "--output" "text")]
       [(status stdout stderr) (aws-cli "sts" "get-caller-identity" args)])
    (if (= status 0)
        (string-trim stdout)
        (error "Failed to fetch AWS Account ID."))))


(define (stack-param->json param)
  (let ([key (select param 'id)]
        [value (select param 'value)])
    (string-append "{\"ParameterKey\": \"" key "\", \"ParameterValue\": \"" value "\"}")))


(define (jsonize-stack-params params)
  (string-append "[\n" (string-join (map stack-param->json params) ", ") "\n]"))


(define (stack-args template environment)
  (let ([config (selectr template (list 'configs (select environment 'id)))])
    (stack-args->list (select environment 'base-region)
                      (select config 'stack-name)
                      (select template 'path)
                      (select config 'parameters)
                      (select template 'capabilities))))


(define (stack-args-short template environment)
  (let ([config (selectr template (list 'configs (select environment 'id)))])
    (stack-args->list (select environment 'base-region)
                      (select config 'stack-name)
                      ""
                      '()
                      "")))


(define (stack-args->list region stack-name template-body parameters capabilities)
  (flatlist (if (eq? region "") '() (list "--region" (symbol->string region)))
            (if (eq? stack-name "") '() (list "--stack-name" stack-name))
            (if (eq? template-body "") '() (list "--template-body" template-body))
            (if (eq? capabilities "") '() (list "--capabilities" capabilities))
            (if (empty? parameters)   '() (list "--parameters" (jsonize-stack-params parameters)))))


(define (cmd-success? status stdout stderr)
  (if (= status 0)
      (when (non-empty-string? (string-trim stdout))
        (display stdout))
      (printf "status:\n~a\n\nstdout:\n~a\nstderr:\n~a\n" status stdout stderr))
  (= status 0))


(define (ask prompt)
  (display prompt)
  (flush-output)
  (let ([response (read-char (current-input-port))])
    (cond
      [(eq? response #\y) #t]
      [(eq? response #\n) #f]
      [else (begin (read-char (current-input-port)) (ask prompt))])))


(define (show-stack-warning cft-config stack-id action)
  (let* ([warning-id (string->symbol (string-append (symbol->string action) "-warning"))]
         [warning (selectr cft-config (list 'stack-template stack-id 'warnings warning-id 'value))])
    (when (non-empty-string? (string-trim warning))
      (displayln warning)
      (displayln "Please complete this step before continuing.")
      (when (not (ask "Would you like to continue? y/n "))
        (displayln "Aborting...")
        (exit)))))

(define (show-stack-group-warnings cft-config stack-group-id action)
    (for/and ([stack-id (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
      (show-stack-warning cft-config stack-id action)))

;; (define (create-stack cft-config stack-id environment-id show-warnings dry-run)
;;   (when show-warnings (show-stack-warning cft-config stack-id 'create-warning))
;;   (displayln (format "Creating Stack ~v in environment ~v" (symbol->string stack-id) (symbol->string environment-id)))
;;   (let* ([template (selectr cft-config (list 'stack-template stack-id))]
;;          [env (selectr cft-config (list 'environment environment-id))]
;;          [create-cmd (λ () (aws-cli "cloudformation" "create-stack" (stack-args template env) dry-run))]
;;          [wait-cmd   (λ () (aws-cli "cloudformation" "wait" (flatlist "stack-create-complete" (stack-args-short template env)) dry-run))]
;;          [create-successful (and (call-with-values create-cmd cmd-success?) (call-with-values wait-cmd cmd-success?))])
;;     (if create-successful
;;         (displayln (format "Created Stack ~v" (symbol->string stack-id)))
;;         (error (format "Failed to create stack ~v" (symbol->string stack-id))))
;;     create-successful))


;; (define (update-stack cft-config stack-id environment-id show-warnings dry-run)
;;   (when show-warnings (show-stack-warning cft-config stack-id 'update-warning))
;;   (displayln (format "Updating Stack ~v in environment ~v" (symbol->string stack-id) (symbol->string environment-id)))
;;   (let* ([template (selectr cft-config (list 'stack-template stack-id))]
;;          [env (selectr cft-config (list 'environment environment-id))]
;;          [update-cmd (λ () (aws-cli "cloudformation" "update-stack" (stack-args template env) dry-run))]
;;          [wait-cmd   (λ () (aws-cli "cloudformation" "wait" (flatlist "stack-update-complete" (stack-args-short template env)) dry-run))]
;;          [update-successful (and (call-with-values update-cmd cmd-success?) (call-with-values wait-cmd cmd-success?))])
;;     (if update-successful
;;         (displayln (format "Updated Stack ~v" (symbol->string stack-id)))
;;         (error (format "Failed to update stack ~v" (symbol->string stack-id))))
;;     update-successful))


;; (define (delete-stack cft-config stack-id environment-id show-warnings dry-run)
;;   (when show-warnings (show-stack-warning cft-config stack-id 'delete-warning))
;;   (displayln (format "Deleting Stack ~v in environment ~v" (symbol->string stack-id) (symbol->string environment-id)))
;;   (let* ([template (selectr cft-config (list 'stack-template stack-id))]
;;          [env (selectr cft-config (list 'environment environment-id))]
;;          [delete-cmd (λ () (aws-cli "cloudformation" "delete-stack" (stack-args-short template env) dry-run))]
;;          [wait-cmd   (λ () (aws-cli "cloudformation" "wait" (flatlist "stack-delete-complete" (stack-args-short template env)) dry-run))]
;;          [delete-successful (and (call-with-values delete-cmd cmd-success?) (call-with-values wait-cmd cmd-success?))])
;;     (if delete-successful
;;         (displayln (format "Deleted Stack ~v" (symbol->string stack-id)))
;;         (error (format "Failed to delete stack '~v'" (symbol->string stack-id))))
;;     delete-successful))


(define (parts-of-speech action)
  (match action
    ['create (values "create" "Created" "Creating")]
    ['update (values "update" "Updated" "Updating")]
    ['delete (values "delete" "Deleted" "Deleting")]))


(define (modify-stack action cft-config stack-id environment-id show-warnings dry-run)
  (let-values ([(action-str past present) (parts-of-speech action)])
    (when show-warnings (show-stack-warning cft-config stack-id action))
    (displayln (string-append present " Stack '" (symbol->string stack-id)"' in environment '" (symbol->string environment-id) "'"))
    (let* ([template (selectr cft-config (list 'stack-template stack-id))]
           [env (selectr cft-config (list 'environment environment-id))]
           [args (stack-args template env)]
           [short-args (stack-args-short template env)]
           [wait-args (flatlist (string-append "stack-" action-str "-complete") short-args)]
           [action-cmd (match action
                         ['create (λ () (aws-cli "cloudformation" "create-stack" args dry-run))]
                         ['update (λ () (aws-cli "cloudformation" "update-stack" args dry-run))]
                         ['delete (λ () (aws-cli "cloudformation" "delete-stack" short-args dry-run))])]
           [wait-cmd (λ () (aws-cli "cloudformation" "wait" wait-args dry-run))]
           [action-successful (and (call-with-values action-cmd cmd-success?) (call-with-values wait-cmd cmd-success?))])
      (if action-successful
          (displayln (string-append past " Stack '" (symbol->string stack-id) "'"))
          (error (string-append "Failed to " action-str " stack '" (symbol->string stack-id) "'")))
      action-successful)))



(define (list-stacks cft-config is-group)
  (if is-group
      (for ([stack (select cft-config 'stack-group)])
        (displayln (select stack 'id)))
      (for ([stack (select cft-config 'stack-template)])
        (displayln (select stack 'id)))))

;; (define (create-stack-group action cft-config stack-group-id environment-id show-warnings dry-run)
;;   (displayln (format "Creating Stack group ~v in environment ~v\n" (symbol->string stack-group-id) (symbol->string environment-id)))
;;   (when show-warnings (show-stack-group-warnings cft-config stack-group-id 'create-warning))
;;   (for/and ([stack-id (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
;;     (modify-stack action cft-config stack-id environment-id #f dry-run)
;;     (newline))
;;   (displayln (format "Created Stack group ~v" (symbol->string stack-group-id))))


;; (define (update-stack-group action cft-config stack-group-id environment-id show-warnings dry-run)
;;   (displayln (format "Updating Stack group ~v in environment ~v\n" (symbol->string stack-group-id) (symbol->string environment-id)))
;;   (when show-warnings (show-stack-group-warnings cft-config stack-group-id 'update-warning))
;;   (for/and ([stack-id (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
;;     (modify-stack action cft-config stack-id environment-id #f dry-run)
;;     (newline))
;;   (displayln (format "Updated Stack group ~v" (symbol->string stack-group-id))))


;; (define (delete-stack-group action cft-config stack-group-id environment-id show-warnings dry-run)
;;   (displayln (format "Deleting Stack group ~v in environment ~v\n" (symbol->string stack-group-id) (symbol->string environment-id)))
;;   (when show-warnings (show-stack-group-warnings cft-config stack-group-id 'delete-warning))
;;   (for/and ([stack-id (reverse (selectr cft-config (list 'stack-group stack-group-id 'stacks)))])
;;     (modify-stack action cft-config stack-id environment-id #f dry-run)
;;     (newline))
;;   (displayln (format "Deleted Stack group ~v" (symbol->string stack-group-id))))

(define (modify-stack-group action cft-config stack-group-id environment-id show-warnings dry-run)
  (let-values ([(action-str past present) (parts-of-speech action)]
               [(stack-ids) (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
    (displayln (string-append present "Stack group '" (symbol->string stack-group-id) "' in environment '" (symbol->string environment-id) "'\n"))
    (when show-warnings (show-stack-group-warnings cft-config stack-group-id action))
    (for/and ([stack-id (if (eq? action 'delete) (reverse stack-ids) stack-ids)])
         (modify-stack action cft-config stack-id environment-id #f dry-run)
         (newline))
    (displayln (string-append past "Stack group '" (symbol->string stack-group-id) "'"))))


(define (stack cft-config action is-group target env show-warnings dry-run)
  (match* (action is-group)
    [('create #f) (modify-stack action cft-config target env show-warnings dry-run)]
    [('update #f) (modify-stack action cft-config target env show-warnings dry-run)]
    [('delete #f) (modify-stack action cft-config target env show-warnings dry-run)]
    [('list #f) (list-stacks cft-config is-group)]
    [('create #t) (modify-stack-group action cft-config target env show-warnings dry-run)]
    [('update #t) (modify-stack-group action cft-config target env show-warnings dry-run)]
    [('delete #t) (modify-stack-group action cft-config target env show-warnings dry-run)]
    [('list #t) (list-stacks cft-config is-group)]))


;; Handle cmd line args
(define dry-run (make-parameter #f))
(define is-group (make-parameter #f))
(define show-warnings (make-parameter #t))
(define action (make-parameter 'nil))
(define env (make-parameter 'nil))

(define targets
  (command-line
   #:program "cftool"
   #:once-each
   [("-g" "--group") "Specify that the <target-names> are stack groups" (is-group #t)]
   [("-D" "--dry-run") "Print out the set of aws-cli commands which will be called, without than calling them" (dry-run #t)]
   [("-e" "--environment") environment "Set the environment for the command" (env (string->symbol environment))]
   [("-S" "--no-warnings") "Disable warning prompts" (show-warnings #t)]
   #:once-any
   [("-c" "--create") "Create the specified stack or stack group" (action 'create)]
   [("-u" "--update") "Update the specified stack or stack group" (action 'update)]
   [("-d" "--delete") "Delete the specified stack or stack group" (action 'delete)]
   [("-l" "--list") "List all known stacks or stack groups" (action 'list)]
   #:args target-names target-names))

;; NS stuff used for parsing cft file
(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

;; Parse configuration
(define cft-config (eval-config ns "../../../scratch/cftool/stacks.cft"))

;; Execute
(if (empty? targets)
    (stack cft-config (action) (is-group) 'nil (env) (show-warnings) (dry-run))
    (for ([target targets])
      (stack cft-config (action) (is-group) (string->symbol target) (env) (show-warnings) (dry-run))))
