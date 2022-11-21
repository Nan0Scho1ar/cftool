#lang racket
(require racket/pretty)
(require racket/system)

;; NS stuff used for parsing cft file
(define-namespace-anchor ns-anchor)
(define cftool-namespace (namespace-anchor->namespace ns-anchor))

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
;; (define-cf-item aws-hosted-zone)
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


(define (show-stack-warning action cft-config stack-id)
  (let* ([warning-id (string->symbol (string-append (symbol->string action) "-warning"))]
         [warning (selectr cft-config (list 'stack-template stack-id 'warnings warning-id 'value))])
    (when (non-empty-string? (string-trim warning))
      (displayln warning)
      (displayln "Please complete this step before continuing.")
      (when (not (ask "Would you like to continue? y/n "))
        (displayln "Aborting...")
        (exit)))))

(define (show-stack-group-warnings action cft-config stack-group-id)
  (for/and ([stack-id (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
    (show-stack-warning cft-config stack-id action)))




(define (list-stacks cft-config is-group)
  (if is-group
      (for ([stack (select cft-config 'stack-group)])
        (displayln (select stack 'id)))
      (for ([stack (select cft-config 'stack-template)])
        (displayln (select stack 'id)))))


;; Returns the commands use to modify a stack
(define (stack-cmds action cft-config stack-id environment-id dry-run)
  (let* ([env (selectr cft-config (list 'environment environment-id))]
         [template (selectr cft-config (list 'stack-template stack-id))]
         [args (stack-args template env)]
         [short-args (stack-args-short template env)]
         [wait-args (flatlist (string-append "stack-" (symbol->string action) "-complete") short-args)]
         [action-cmd (match action
                       ['create (λ () (aws-cli "cloudformation" "create-stack" args dry-run))]
                       ['update (λ () (aws-cli "cloudformation" "update-stack" args dry-run))]
                       ['delete (λ () (aws-cli "cloudformation" "delete-stack" short-args dry-run))])]
         [wait-cmd (λ () (aws-cli "cloudformation" "wait" wait-args dry-run))])
    (values action-cmd wait-cmd)))


(define (get-msgs action-id target-id environment-id type)
  (let ([action (symbol->string action-id)]
        [target (symbol->string target-id)]
        [environment (symbol->string environment-id)])
    (match type
      ['modify-stacks
       (values
        (string-append "Begin " action " Stack '" target"' in environment '" environment"'")
        (string-append "Completed " action " Stack '" target"'")
        (string-append "Failed to " action " stack '" target"'"))]
      ['modify-stack-groups
       (values
        (string-append "Begin " action " Stack group '" target"' in environment '" environment"'\n")
        (string-append "\nCompleted " action " Stack group '" target"'"))])))


(define (modify-stacks action cft-config stack-ids environment-id show-warnings dry-run)
  (for/and ([stack-id stack-ids])
    (when show-warnings (show-stack-warning action cft-config stack-id))
    (let-values ([(begin-msg success-msg error-msg)
                  (get-msgs action stack-id environment-id 'modify-stacks)]
                 [(action-cmd wait-cmd)
                  (stack-cmds action cft-config stack-id environment-id dry-run)])
      (displayln begin-msg)
      (if (and (call-with-values action-cmd cmd-success?)
               (call-with-values wait-cmd cmd-success?))
          (displayln success-msg)
          (error error-msg))
      #t)))


(define (modify-stack-groups action cft-config stack-group-ids environment-id show-warnings dry-run)
  (for/and ([stack-group-id stack-group-ids])
    (let-values ([(begin-msg success-msg)
                  (get-msgs action stack-group-id environment-id 'modify-stack-groups)])
      (let ([stack-ids
             (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
        (when (eq? action 'delete)
          (set! stack-ids (reverse stack-ids)))
        (displayln begin-msg)
        (when show-warnings
          (show-stack-group-warnings action cft-config stack-group-id))
        (modify-stacks action cft-config stack-ids environment-id #f dry-run)
        (displayln success-msg)))))


(define (cftool cft-config action is-group targets env show-warnings dry-run)
  (if (eq? action 'list)
      (list-stacks cft-config is-group)
      (if is-group
          (modify-stack-groups action cft-config targets env show-warnings dry-run)
          (modify-stacks action cft-config targets env show-warnings dry-run))))


;; Handle cmd line args and return a cftool configuration
(define (cftool-config namespace)
  (let* ([action (make-parameter 'nil)]
         [is-group (make-parameter #f)]
         [env (make-parameter 'nil)]
         [show-warnings (make-parameter #t)]
         [dry-run (make-parameter #f)]
         [targets (command-line
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
                   #:args target-names (for/list ([name target-names]) (string->symbol name)))]
         [cft-config (eval-config namespace "stacks.cft")])
    (when (eq? (action) 'nil)
      (displayln "Error an action is required.\nPlease provide an action and try again.")
      (exit))
    (values cft-config (action) (is-group) targets (env) (show-warnings) (dry-run))))

;; Execute cftool using a cftool config
;; (call-with-values (lambda () (cftool-config cftool-namespace)) cftool)

(pretty-print (eval-config cftool-namespace "stacks.cft"))
