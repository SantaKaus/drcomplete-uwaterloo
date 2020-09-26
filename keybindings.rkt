;;
;; **************************************************************
;;    Copyright (c) Raymond Li <racket@raymond.li> 2020
;;    Keybindings for DrRacket
;;    Last updated 2020-09-19
;;    Modified from Racket documentation
;; **************************************************************
;;

#lang s-exp framework/keybinding-lang
(require drracket/tool-lib srfi/2)
(require (for-syntax racket/list))

(define-syntax-rule (define-shortcut-internal (key ...) name proc)
  (begin
    (define (name ed evt . rest)
      (when (is-a? ed racket:text<%>)
        (send ed begin-edit-sequence)
        (apply proc ed evt rest)
        (send ed end-edit-sequence)))
    (keybinding key name) ...))

(define-syntax (define-shortcut stx)
  ;; Add esc; equivalent key bindings for all the meta bindings.
  (define (add-esc-and-option-key-bindings s-keys)
    (define keys (syntax->datum s-keys))
    (define esc-variants
      (for/list ([k (in-list keys)]
                 #:when (regexp-match? #rx"m:" k))
        (string-append "esc;" (regexp-replace* #rx"m:" k ""))))
    (define option-variants
      (for/list ([k (in-list keys)]
                 #:when (regexp-match? #rx"m:" k))
        (string-append "?:a:" (regexp-replace* #rx"m:" k ""))))
    ;; Use remove-duplicates to combine all key bindings, so that duplicates
    ;; are removed. This means that if we add some esc; key bindings manually,
    ;; for example by accident, it will not be duplicated, affecting display
    ;; of key bindings in DrRacket.
    (remove-duplicates (append esc-variants option-variants keys)))
  (syntax-case stx ()
    [(_ key (name . args) body* ...)
     #'(define-shortcut key name
         (λ args body* ...))]
    [(_ (key ...) name proc)
     #`(define-shortcut-internal
         (#,@(add-esc-and-option-key-bindings #'(key ...)))
         name proc)]
    [(_ key name proc)
     #'(define-shortcut (key) name proc)]))

(define (menu-bind key menu-item)
  (keybinding
   key
   (λ (ed evt)
     (define canvas (send ed get-canvas))
     (when canvas
       (define menu-bar (find-menu-bar canvas))
       (when menu-bar
         (define item (find-item menu-bar menu-item))
         (when item
           (define menu-evt
             (new control-event%
                  [event-type 'menu]
                  [time-stamp
                   (send evt get-time-stamp)]))
           (send item command menu-evt)))))))

(define/contract (find-menu-bar c)
  (-> (is-a?/c area<%>) (or/c #f (is-a?/c menu-bar%)))
  (let loop ([c c])
    (cond
      [(is-a? c frame%) (send c get-menu-bar)]
      [(is-a? c area<%>) (loop (send c get-parent))]
      [else #f])))

(define/contract (find-item menu-bar label)
  (-> (is-a?/c menu-bar%)
      string?
      (or/c (is-a?/c selectable-menu-item<%>) #f))
  (let loop ([o menu-bar])
    (cond
      [(is-a? o selectable-menu-item<%>)
       (and (equal? (send o get-plain-label) label)
            o)]
      [(is-a? o menu-item-container<%>)
       (for/or ([i (in-list (send o get-items))])
         (loop i))]
      [else #f])))

(define (rebind key command)
  (keybinding
   key
   (λ (ed evt)
     (send (send ed get-keymap) call-function
           command ed evt #t))))

(menu-bind "m:r" "Run")
(menu-bind ":?:s:c:F" "Reindent All")
(menu-bind "c:h" "Show Replace")
(menu-bind ":?:s:c:R" "Replace All")
(menu-bind "c:/" "Comment Out with Semicolons")

(rebind "c:backspace" "backward-kill-word")
(rebind "c:del" "kill-word")
(rebind "m:up" "shift-focus")
(rebind "m:down" "shift-focus")
