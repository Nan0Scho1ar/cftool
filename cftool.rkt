#!/usr/bin/env racket
#lang racket
(require syntax/parse/define)
(require json)

;; #lang racket/base
;; (require racket/list)
;; (require racket/string)
;; (require racket/function)
;; (require racket/port)
;; (require racket/match)
;; (require racket/cmdline)

;; NS stuff used for parsing cft file
(define-namespace-anchor ns-anchor)
(define cftool-namespace (namespace-anchor->namespace ns-anchor))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

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


(define (ask prompt)
  (display prompt)
  (flush-output)
  (let ([response (string-trim (read-line (current-input-port)))])
    (newline)
    (cond
      [(equal? response "y") #t]
      [(equal? response "n") #f]
      [else (ask prompt)])))


(define (display-sorted-keys hashmap)
  (let ([keys (map symbol->string (hash-keys hashmap))])
    (for ([key (sort keys string<?)]) (displayln key))))



(define (prefix-env-domain domain-prefix env)
  (string-append domain-prefix "." (environment-hosted-zone env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cftool Syntax Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax-rule (define-mutable-hashmap name)
  (begin
    (define name (make-hash))
    (set! name (make-hash))))


(define (add-if-key-missing hashmap key value)
    (if (hash-has-key? hashmap key)
        (begin
          (displayln (string-append "ERROR: Duplicate key: " (symbol->string key)))
          (exit 1))
        (hash-set! hashmap key value)))


(define-mutable-hashmap stacks)
(struct stack (id
               path
               name
               capabilities
               dependencies
               before-create
               after-create
               before-update
               after-update
               before-delete
               after-delete
               create-warning
               update-warning
               delete-warning
               parameters))

(begin-for-syntax
  (define-syntax-class stack-names
    #:commit
    (pattern (stack-name:expr
              (~optional (~seq #:env environment:expr)))))

  (define-syntax-class parameter
    #:commit
    (pattern (key:expr
              value:expr
              (~optional (~seq #:env environment:expr))))))

(define-syntax-parser define-stack
  [(_:id (id:id env:id)
         (~alt
          (~once (~seq #:path path:expr))
          (~once
           (~or*
            (~seq #:stack-name (names:stack-names ...))
            (~seq #:stack-name stack-name:expr)))
          (~optional (~seq #:capabilities capabilities:expr))
          (~optional (~seq #:depends-on dependencies:expr))
          (~optional (~seq #:before-create before-create:expr))
          (~optional (~seq #:after-create after-create:expr))
          (~optional (~seq #:before-update before-update:expr))
          (~optional (~seq #:after-update after-update:expr))
          (~optional (~seq #:before-delete before-delete:expr))
          (~optional (~seq #:after-delete after-delete:expr))
          (~optional (~seq #:create-warning create-warning:expr))
          (~optional (~seq #:update-warning update-warning:expr))
          (~optional (~seq #:delete-warning delete-warning:expr))
          (~optional (~seq #:parameters (params:parameter ...))))
         ...)
   (syntax/loc this-syntax
     (begin
       (define (id [env env-default])
         (stack 'id
                path
                (~? (first (filter values (list (and (~? (eq? env names.environment) #t)
                                                     names.stack-name)
                                                ...)))
                    stack-name)
                (~? capabilities   null)
                (~? dependencies   null)
                (~? before-create  null)
                (~? after-create   null)
                (~? before-update  null)
                (~? after-update   null)
                (~? before-delete  null)
                (~? after-delete   null)
                (~? create-warning null)
                (~? update-warning null)
                (~? delete-warning null)
                (~? (filter values (list (and (~? (eq? env params.environment) #t)
                                              (cons (symbol->string params.key) params.value))
                                         ...))
                    null)))
       (add-if-key-missing stacks 'id id)))])


(define-mutable-hashmap stack-groups)
(struct stack-group (id stacks))
(define-syntax-rule (define-stack-group id stacks)
  (begin
    (define id (stack-group 'id stacks))
    (add-if-key-missing stack-groups 'id id)))


(define-mutable-hashmap accounts)
(struct account (id name aws-id))
(define-syntax-rule (define-account id #:name name #:aws-id aws-id)
  (begin
    (define id (account 'id name aws-id))
    (add-if-key-missing accounts 'id id)))


(define-mutable-hashmap environments)
(struct environment (id name account region hosted-zone))
(define-syntax-rule (define-environment id #:name name #:account account #:region region #:hosted-zone hosted-zone)
  (begin
    (define id (environment 'id name account region hosted-zone))
    (add-if-key-missing environments 'id id)))

;; ACM Certificates
(define-mutable-hashmap certificates)
(struct certificate (id region domain domain-prefix))
(define-syntax-rule (define-certificate (id env) #:region region #:domain domain #:domain-prefix domain-prefix)
  (begin
    (define (id [env env-default]) (certificate 'id region domain domain-prefix))
    (add-if-key-missing certificates 'id id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval Syntax Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-cft-syntax namespace filepath file-port)
  (for/list ([stx (in-port (λ (port) (read-syntax filepath port)) file-port)])
    (eval stx namespace)))


;; Parse a .cft into hash sets of cf-items
(define (eval-config namespace filepath)
  (call-with-input-file filepath
    ((curry eval-cft-syntax) namespace filepath))
  #t)


;;;;;;;;;;;;;;;;;;;;;;
;; Select Functions ;;
;;;;;;;;;;;;;;;;;;;;;;


;; Select unique item from list with matching id
(define (select-list items id)
  (let* ([filtered (filter (λ (x) (equal? (hash-ref x 'id) id)) items)]
         [len (length filtered)])
    (cond
      [(empty? filtered) (raise (format "Cannot find '~v'" id))]
      [(= 1 (length filtered)) (car filtered)]
      [else (raise (format "'~v' is not unique" id))])))

(define (select-proc proc selector)
  (select (proc) selector))

;; Select item from list matching selector
(define (select items selector)
  (cond
    [(list? items) (select-list items selector)]
    [(hash? items) (if (hash-has-key? items selector) (hash-ref items selector) null)]
    [(procedure? items) (select-proc items selector)]
    [else (error "ERROR: 'select' failed: Unhandled type")]))


;; Select recursively
(define (selectr items selectors)
  (if (empty? selectors)
     items
     (selectr (select items (car selectors)) (cdr selectors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base Exec Cmd Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(define (cmd-success? status stdout stderr)
  (if (= status 0)
     (when (non-empty-string? (string-trim stdout))
       (display stdout))
     (printf "status:\n~a\n\nstdout:\n~a\nstderr:\n~a\n" status stdout stderr))
  (= status 0))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query AWS Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stack-exists region stack-name)
  (let*-values
      ([(args) (list "--region" region
                    "--stack-name" stack-name)]
       [(status stdout stderr) (aws-cli "cloudformation" "describe-stacks" args)])
    (= status 0)))


(define (fetch-aws-account-id dry-run)
  (let*-values
      ([(args) (list "--query" "Account"
                    "--output" "text")]
       [(status stdout stderr) (aws-cli "sts" "get-caller-identity" args dry-run)])
    (if (= status 0)
       (string-trim stdout)
       (begin (displayln "Failed to fetch AWS Account ID.")
             (exit 1)))))


(define (get-cert-full-domain cert)
  (if (non-empty-string? (certificate-domain-prefix cert))
      (string-append (certificate-domain-prefix cert) "." (certificate-domain cert))
      (certificate-domain cert)))


(define (acm-get-arn cert [dry-run #f])
  (let* ([region (symbol->string (certificate-region cert))]
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
           (begin (displayln (string-append "Failed to fetch ACM Certificate '" domain "'"))
                 (exit 1)))))))


(define (r53-get-hosted-zone-id env #:private-zone is-private #:dry-run [dry-run #f])
  (let* ([region (symbol->string (environment-region env))]
         [domain (environment-hosted-zone env)]
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
           (begin
             (displayln (string-append "Failed to fetch hosted zone id. DOMAIN: " domain " PRIVATEZONE: " private-zone))
             (exit 1)))))))


(define (r53-private-hosted-zone-id env) (r53-get-hosted-zone-id env  #:private-zone #t))
(define (r53-public-hosted-zone-id env) (r53-get-hosted-zone-id env #:private-zone #f))


(define (elbv2-dns-for-load-balancer env load-balancer-name #:dry-run [dry-run #f])
  (let* ([region (symbol->string (environment-region env))]
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
           (begin
             (displayln "Failed to fetch load balancer DNS. LOADBALANCER: " load-balancer-name)
             (exit 1)))))))


(define (iam-create-service-linked-role env service-name [dry-run #f])
  (let* ([region (symbol->string (environment-region env))]
         [args (list "--region" region
                    "--aws-service-name" service-name)])
    (let-values ([(status stdout stderr)
                  (aws-cli "iam" "create-service-linked-role" args dry-run)])
      (if (= status 0)
         (string-trim stdout)
         (begin
           (displayln stderr)
           (begin
             (displayln "Failed to create service linked role for: " service-name)
             (exit 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify Stack Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (jsonize-stack-params params [pretty #f])
  (if pretty
      (string-append "[\n  " (string-join (map (λ (param) (stack-param->json param #t)) params) ",\n  ") "\n]")
      (string-append "[\n" (string-join (map stack-param->json params) ", ") "\n]")))


(define (stack-param->json param [pretty #f])
  (let* ([key (car param)]
         [val (cdr param)]
         [value (if (procedure? val) (val) val)])
    (if pretty
        (string-append "{\"" key "\": \""  value "\"}")
        (string-append "{\"ParameterKey\": \"" key "\", \"ParameterValue\": \"" value "\"}"))))


(define (stack-args template environment)
  (stack-args->list (environment-region environment)
                   (stack-name template)
                   (stack-path template)
                   (stack-parameters template)
                   (stack-capabilities template)))


(define (stack-args-short template environment)
  (stack-args->list (environment-region environment)
                   (stack-name template)
                   ""
                   '()
                   ""))


(define (stack-args->list region stack-name template-body parameters capabilities)
  (flatlist (if (eq? region "")
               '()
               (list "--region" (symbol->string region)))
           (if (eq? stack-name "")
              '()
              (list "--stack-name" stack-name))
           (if (eq? template-body "")
              '()
              (list "--template-body" template-body))
           (if (or (empty? capabilities) (eq? capabilities ""))
              '()
              (list "--capabilities" capabilities))
           (if (empty? parameters)
              '()
              (list "--parameters" (jsonize-stack-params parameters)))))


(define (show-stack-warning action stack-id)
  (unless (stack-definition-exists? stack-id)
    (displayln (string-append "ERROR: Unknown stack '" (symbol->string stack-id) "'"))
    (exit 1))
  (let* ([stack ((hash-ref stacks stack-id))]
         [warning (match action
                    ['create (stack-create-warning stack)]
                    ['update (stack-update-warning stack)]
                    ['delete (stack-delete-warning stack)])])
    (when (and (string? warning)
              (non-empty-string? (string-trim warning)))
      (displayln warning)
      (displayln "Please complete this step before continuing.")
      (when (not (ask "Would you like to continue? y/n "))
        (displayln "Aborting...")
        (exit)))))

(define (stack-definition-exists? stack-id)
  (hash-has-key? stacks stack-id))

(define (stack-group-definition-exists? stack-group-id)
  (stack-group? (hash-ref stack-groups stack-group-id)))

(define (show-stack-group-warnings action stack-group-id)
  (unless (stack-group-definition-exists? stack-group-id)
    (displayln (string-append "ERROR: Unknown stack group '" (symbol->string stack-group-id) "'"))
    (exit 1))
  (for/and ([stack-id (stack-group-stacks (hash-ref stack-groups stack-group-id))])
    (show-stack-warning action stack-id)))


;; Returns the commands use to modify a stack
(define (stack-cmds action stack-id environment-id dry-run)
  (let* ([env (hash-ref environments environment-id)]
         [template ((hash-ref stacks stack-id) env)]
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
    (when show-warnings
      (show-stack-warning action stack-id)
      (newline))
    (let-values ([(begin-msg success-msg error-msg)
                  (get-msgs action stack-id environment-id 'modify-stacks)]
                 [(action-cmd wait-cmd)
                  (stack-cmds action stack-id environment-id dry-run)])
      (when (not dry-run)
        (displayln begin-msg))
      (if (and (call-with-values action-cmd cmd-success?)
              (call-with-values wait-cmd cmd-success?))
         (when (not dry-run)
           (displayln success-msg))
         (begin
           (displayln error-msg)
           (exit 1)))
      (newline)
      #t)))

(define (print-stack-params stack-ids environment-id)
  (for/and ([stack-id stack-ids])
    (displayln
     (jsonize-stack-params
      (stack-parameters ((hash-ref stacks stack-id) (hash-ref environments environment-id)))
      #t))))


(define (modify-stack-groups action stack-group-ids environment-id show-warnings dry-run)
  (for/and ([stack-group-id stack-group-ids])
    (when show-warnings
      (show-stack-group-warnings action stack-group-id)
      (newline))
    (let-values ([(begin-msg success-msg)
                  (get-msgs action stack-group-id environment-id 'modify-stack-groups)])
      (let ([stack-ids
             (stack-group-stacks (hash-ref stack-groups stack-group-id))])
        (when (eq? action 'delete)
          (set! stack-ids (reverse stack-ids)))
        (displayln begin-msg)
        (modify-stacks action stack-ids environment-id #f dry-run)
        (displayln success-msg)))))

;;;;;;;;;;;;;;;;;;;;;;
;; CFTool Functions ;;
;;;;;;;;;;;;;;;;;;;;;;


(define (cftool action is-group targets env show-warnings dry-run)
  (cond [(eq? action 'list) (display-sorted-keys (if is-group stack-groups stacks))]
        [(eq? action 'params) (print-stack-params targets env)]
        [is-group (begin
                    (check-cli-credentials env #f)
                    (modify-stack-groups action targets env show-warnings dry-run))]
        [else (begin
                (check-cli-credentials env #f)
                (modify-stacks action targets env show-warnings dry-run))]))

(define (check-cli-credentials env-id dry-run)
  (let* ([fetched-id (fetch-aws-account-id dry-run)]
         [env-acc (environment-account (hash-ref environments env-id))]
         [acc-id (account-aws-id env-acc )])
    (unless (equal? fetched-id acc-id)
      (displayln (string-append "ERROR: Account ID of current AWS_PROFILE does not match ID specified for environment " (symbol->string env-id)))
      (displayln "Please ensure you are using the correct AWS_PROFILE and try again.")
      (exit 1))))

;; Handle cmd line args and return a cftool configuration
(define (cftool-config)
  (let* ([action (make-parameter 'nil)]
         [is-group (make-parameter #f)]
         [env (make-parameter 'nil)]
         [show-warnings (make-parameter #t)]
         [dry-run (make-parameter #f)]
         [cft-path (make-parameter "stacks.cft")]
         [targets (command-line
                   #:program "cftool"
                   #:once-each
                   [("-g" "--group") "Specify that the <target-names> are stack groups" (is-group #t)]
                   [("-D" "--dry-run") "Print out the set of aws-cli commands which will be called, without than calling them" (dry-run #t)]
                   [("-e" "--environment") environment "Set the environment for the command" (env (string->symbol environment))]
                   [("-f" "--file") filepath "Path to the .cft file" (cft-path filepath)]
                   [("-S" "--no-warnings") "Disable warning prompts" (show-warnings #t)]
                   #:once-any
                   [("-c" "--create") "Create the specified stack or stack group" (action 'create)]
                   [("-u" "--update") "Update the specified stack or stack group" (action 'update)]
                   [("-d" "--delete") "Delete the specified stack or stack group" (action 'delete)]
                   [("-l" "--list")   "List all known stacks or stack groups" (action 'list)]
                   [("-p" "--params") "Print the parameters for a stack" (action 'params)]
                   #:args target-names (for/list ([name target-names]) (string->symbol name)))])
    (when (eq? (action) 'nil)
      (displayln "Error an action is required.\nPlease provide an action and try again.")
      (exit 1))
    (eval-config cftool-namespace (cft-path))
    (values (action) (is-group) targets (env) (show-warnings) (dry-run))))

;; Execute cftool using a cftool config
(if (call-with-values (lambda () (cftool-config)) cftool)
   (exit 0)
   (exit 1))

;; (eval-config cftool-namespace "stacks.cft")
;; (pretty-print stacks)
;; (pretty-print ((hash-ref stacks 'SysAdmin-Service)))



;; (define iam
;;   (stack-template
;;    #:id 'IAM
;;    #:path "some-path"
;;    #:stack-name "test"))

;; (pretty-print stacks)
