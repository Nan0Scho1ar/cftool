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


(define (prefix-env-domain domain-prefix env)
  (string-append domain-prefix "." (select env 'hosted-zone)))

;; ;; Group a list by one of their properies then convert
;; ;; to a hash table indexed by that property
;; (define (list->grouped-hash items key)
;;   (apply hash (foldr (λ (g acc) (cons (hash-ref (car g) key) (cons g acc)))
;;                      '()
;;                      (group-by (λ (i) (hash-ref i key)) items))))


;; Define new syntax for creating new cf-item types
(define-syntax-rule (define-cf-item name)
  (define name (make-keyword-procedure ((curry cf-item->hash) 'name))))


;; Define cf-item types
(define-cf-item aws-credential)
(define-cf-item aws-account)
(define-cf-item acm-certificate)
(define-cf-item environment)
(define-cf-item stack-template)
(define-cf-item stack-group)
(define-cf-item stack-config)
(define-cf-item stack-param)
(define-cf-item stack-warning)

(define stacks (make-hash))
(define (register-stack new-stack)
  (hash-set! stacks (select (new-stack) 'id) new-stack))

(define stack-groups (make-hash))
(define (register-stack-group new-stack-group)
  (hash-set! stack-groups (select new-stack-group 'id) new-stack-group))

(define environments (make-hash))
(define (register-environment new-environment)
  (hash-set! environments (select new-environment 'id) new-environment))

(define accounts (make-hash))
(define (register-account new-account)
  (hash-set! accounts (select new-account 'id) new-account))

(define credentials (make-hash))
(define (register-credential new-credential)
  (hash-set! credentials (select new-credential 'id) new-credential))

(define certificates (make-hash))
(define (register-certificate new-certificate)
  (hash-set! certificates (select new-certificate 'id) new-certificate))




(define (eval-cft-syntax namespace filepath file-port)
  (for/list ([stx (in-port (λ (port) (read-syntax filepath port)) file-port)])
    (eval stx namespace)))


;; Parse a .cft into hash sets of cf-items
(define (eval-config namespace filepath)
  (call-with-input-file filepath
    ((curry eval-cft-syntax) namespace filepath))
  #t)



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
    [(hash? items) (if (hash-has-key? items selector)
                       (hash-ref items selector)
                       "")]))


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

(define (get-cert-full-domain cert)
  (if (hash-has-key? cert 'domain-prefix)
      (string-append (select cert 'domain-prefix) "." (select cert 'domain))
      (select cert 'domain)))


(define (acm-get-arn cert [dry-run #f])
  (let* ([region (symbol->string (select cert 'region))]
         [domain (get-cert-full-domain cert)]
         [query (string-append "CertificateSummaryList[?DomainName == '" domain "'].[CertificateArn][0][0] || ''")]
         [args (list "--region" region
                     "--query"  query
                     "--output" "text")])
    (let-values ([(status stdout stderr)
                  (aws-cli "acm" "list-certificates" args dry-run)])
      (if (= status 0)
          (string-trim stdout)
          (begin
            (displayln stderr)
            (error "Failed to fetch ACM Certificate."))))))


(define (stack-param->json param)
  (let* ([key (car param)]
         [val (cdr param)]
         [value (if (procedure? val) (val) val)])
    (pretty-print val)
    (string-append "{\"ParameterKey\": \"" key "\", \"ParameterValue\": \"" value "\"}")))


(define (jsonize-stack-params params)
  (string-append "[\n" (string-join (map stack-param->json params) ", ") "\n]"))


(define (stack-args template environment)
  (stack-args->list (select environment 'region)
                    (select template 'stack-name)
                    (select template 'path)
                    (select template 'parameters)
                    (select template 'capabilities)))


(define (stack-args-short template environment)
(stack-args->list (select environment 'region)
                      (select template 'stack-name)
                      ""
                      '()
                      ""))


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


(define (show-stack-warning action stack-id)
  (let* ([warning-id (string->symbol (string-append (symbol->string action) "-warning"))]
         [warning (select ((select stacks stack-id)) warning-id)])
    (when (non-empty-string? (string-trim warning))
      (displayln warning)
      (displayln "Please complete this step before continuing.")
      (when (not (ask "Would you like to continue? y/n "))
        (displayln "Aborting...")
        (exit)))))

(define (show-stack-group-warnings action cft-config stack-group-id)
  (for/and ([stack-id (selectr cft-config (list 'stack-group stack-group-id 'stacks))])
    (show-stack-warning cft-config stack-id action)))


(define (r53-get-hosted-zone-id env #:private-zone is-private #:dry-run [dry-run #f])
  (let* ([region (symbol->string (select env 'region))]
         [domain (select env 'hosted-zone)]
         [private-zone (if is-private "true" "false")]
         [query (string-append "HostedZones[?(Config.PrivateZone == `" private-zone "` && Name == `" domain ".`)].Id[] || \"\"")]
         [args (list "--region" region
                     "--query"  query
                     "--output" "text")])
    (let-values ([(status stdout stderr)
                  (aws-cli "route53" "list-hosted-zones" args dry-run)])
      (if (= status 0)
          (string-trim (string-trim stdout) "/hostedzone/")
          (begin
            (displayln stderr)
            (error "Failed to fetch hosted zone id."))))))

(define (r53-private-hosted-zone-id env) (r53-get-hosted-zone-id env  #:private-zone #t))
(define (r53-public-hosted-zone-id env) (r53-get-hosted-zone-id env #:private-zone #f))

(define (elbv2-dns-for-load-balancer env load-balancer-name #:dry-run [dry-run #f])
  (let* ([region (symbol->string (select env 'region))]
         [query (string-append "LoadBalancers[?LoadBalancerName == `" load-balancer-name "`].DNSName[] || \"\"")]
         [args (list "--region" region
                     "--query"  query
                     "--output" "text")])
    (let-values ([(status stdout stderr)
                  (aws-cli "elbv2" "describe-load-balancers" args dry-run)])
      (if (= status 0)
          (string-trim stdout)
          (begin
            (displayln stderr)
            (error "Failed to fetch load balancer DNS."))))))


(define (display-sorted-keys hashmap)
  (let ([keys (map symbol->string (hash-keys hashmap))])
    (for ([key (sort keys string<?)]) (displayln key))))


(define (list-stacks is-group)
  (display-sorted-keys (if is-group stack-groups stacks)))

;; Returns the commands use to modify a stack
(define (stack-cmds action stack-id environment-id dry-run)
  (let* ([env (select environments environment-id)]
         [template ((select stacks stack-id) env)]
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


(define (modify-stacks action stack-ids environment-id show-warnings dry-run)
  (for/and ([stack-id stack-ids])
    (when show-warnings (show-stack-warning action stack-id))
    (let-values ([(begin-msg success-msg error-msg)
                  (get-msgs action stack-id environment-id 'modify-stacks)]
                 [(action-cmd wait-cmd)
                  (stack-cmds action stack-id environment-id dry-run)])
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


(define (cftool action is-group targets env show-warnings dry-run)
  (cond [(eq? action 'list) (display-sorted-keys (if is-group stack-groups stacks))]
        [is-group (modify-stack-groups action targets env show-warnings dry-run)]
        [else (modify-stacks action targets env show-warnings dry-run)]))


;; Handle cmd line args and return a cftool configuration
(define (cftool-config)
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
                   #:args target-names (for/list ([name target-names]) (string->symbol name)))])
    (when (eq? (action) 'nil)
      (displayln "Error an action is required.\nPlease provide an action and try again.")
      (exit))
    (eval-config cftool-namespace "stacks.cft")
    (values (action) (is-group) targets (env) (show-warnings) (dry-run))))

;; Execute cftool using a cftool config
(call-with-values (lambda () (cftool-config)) cftool)

;; (eval-config cftool-namespace "stacks.cft")
;; (pretty-print (elbv2-dns-for-load-balancer (car (hash-values environments)) "MOST-Private-ALB" #:dry-run #f))
