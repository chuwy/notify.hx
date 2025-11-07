(require-builtin helix/components)
(require-builtin steel/time)

(require "helix/misc.scm")
(require "helix/editor.scm")

(provide notify notify-focus-first)


(define MIN-WIDTH 20)
(define COMPONENT-NAME "notify.hx")

(define ERROR-ICON   "")
(define WARNING-ICON "")
(define INFO-ICON    "")


; Component's local state
; All notifications are in *notifications-queue*
(struct Notification
        ; Ever-incrementing integer
        (id
        ; When the notification was enqueued, formatted string
        time
        ; User-provided heading for the notification, string
        title
        ; List of string lines
        lines
        ; 'info (default), 'warning or 'error
        severity
        ; Mutable box of 'down, 'clicked of #false. Only one notification can have non-false click
        click
        ; Mutable area, changes after every rendering
        area
        ; If rendering function decided the notification doesn't fit the screen - it will be hidden
        hidden
        ; A flag that the notification just has been yanked. Must be #true only for single render
        blink))

(define (get-click notification)
  (unbox (Notification-click notification)))

; Update `state` in-place. Return #true if value has been updated
(define (set-click! notification click)
  (let* ([old-value (set-box! (Notification-click notification) click)])
         (not (equal? old-value click))))

(define (get-area notification)
  (unbox (Notification-area notification)))

(define (set-area! notification area)
  (set-box! (Notification-area notification) area))

(define (hidden? notification)
  (unbox (Notification-hidden notification)))

(define (set-hidden! notification flag)
  (set-box! (Notification-hidden notification) flag))

(define (bg-style notification)
  (if (unbox (Notification-blink notification))
      (begin (set-box! (Notification-blink notification) #f) (theme-scope *helix.cx* "ui.selection"))
      (theme-scope *helix.cx* "normal")))

(define (find pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find pred (cdr lst)))))

(define (repeat-string str n)
  (cond
    [(<= n 0) ""]
    [(= n 1) str]
    [else (string-append str (repeat-string str (- n 1)))]))

(define (get-header notification)
  (let* ([area (get-area notification)]
         [title (Notification-title notification)]
         [title-width (string-length title)]
         [pad-width (if area (- (area-width area) title-width 14) 1)]
         [icon (get-icon notification)])
        (string-append icon " " title (repeat-string " " pad-width) (Notification-time notification))))

(define (get-icon notification)
  (let ([severity (Notification-severity notification)])
       (case severity
             ((info) INFO-ICON)
             ((warning) WARNING-ICON)
             ((error) ERROR-ICON)
             (else INFO-ICON))))

(define (get-style notification)
  (theme-scope *helix.cx* (case (Notification-severity notification)
                                ((info) "info")
                                ((warning) "warning")
                                ((error) "error")
                                (else "info"))))

(define (Notification-height box)
  (let ([area (get-area box)]) (area-height area)))

; The global and extension's primary mutable state
; Contains all currently displayed `Notification` instances
; We don't use Helix' plugin state mechanism because we can't make it fully local,
; e.g. new state mutations could be coming from outside of its widget
(define *notifications-queue* '())

; Add a new notification to the end of mutable list
(define (enqueue notification)
  (set! *notifications-queue* (append *notifications-queue* (list notification))))

(define (no-notifications?)
  (null? *notifications-queue*))

; Remove a notification with specific `id` from *notifications-queue*
(define (pop-by-id id)
  (let loop ((remaining *notifications-queue*)
             (prev '())
             (found #f))
    (cond ((null? remaining) found)
          ((equal? (Notification-id (car remaining)) id)
           (set! *notifications-queue*
                 (append prev (cdr remaining)))
           (car remaining))
          (else
           (loop (cdr remaining) 
                 (append prev (list (car remaining)))
                 found)))))

; Find a notification where `get-click` is not `#f` (could be both `'down` and `'clicked`)
(define (get-clicked)
  (find (lambda (notification) (get-click notification)) *notifications-queue*))

(define (map-index func lst)
  (define index (box 0))
  (map (lambda (elem)
               (define indexVal (unbox index))
               (set-box! index (+ indexVal 1))
               (func indexVal elem)) lst))

(define (get-top-y id)
  (let loop ([lst *notifications-queue*] [acc 1])
    (if (null? lst)
        acc
        (let ([head (car lst)] [tail (cdr lst)])
          (if (equal? (Notification-id head) id)
              acc
              (loop tail (+ acc (Notification-height head))))))))

; Aux mutable state
(define *message-id* 1)

(define (get-message-id) (set! *message-id* (+ 1 *message-id*)))

; Set 'clicked on a notification coming right after one with `after-id` id
(define (notify-focus-next after-id)
  (let loop ((previous #f)  (lst *notifications-queue*))
    (let ([head (car lst)]
          [tail (cdr lst)])
         (cond
           ((and (equal? (Notification-id head) after-id) (not previous) (pair? tail)) (set-click! head #f) (loop #t tail))
           (previous (set-click! head 'clicked))
           ((pair? tail) (loop #f tail))))))
    

(define (notify-focus-first)
  (map-index
    (lambda (idx notification) (if (equal? idx 0) (set-click! notification 'clicked) (set-click! notification #f)))
    *notifications-queue*))


(define (notify msg #:severity [severity 'info] #:title [title ""] #:duration [duration 5000])
  (define text (if (string? msg) msg (to-string msg)))
  (define text-lines (split-many text "\n"))
  (define notification (Notification (get-message-id) (local-time/now! "%H:%M:%S") title text-lines severity (box #f) (box #f) (box #f) (box #f)))

  (when (no-notifications?)
        (push-component! (new-component! COMPONENT-NAME
                                         *notifications-queue*
                                         render-notifications-queue
                                         (hash "handle_event" handle-notification-event "cursor" get-notification-cursor))))

  (enqueue notification)
  (schedule-drop notification duration))

(define (render-notifications-queue _ rect frame)
  (map (lambda (notification) (render-notification notification rect frame)) *notifications-queue*))

(define (get-notification-cursor _ _)
  (let* ([with-cursor (get-clicked)]
         [area (if with-cursor (get-area with-cursor) #f)])
        (if area (position (+ 3 (area-y area)) (+ 2 (area-x area))))))


(define (handle-notification-event _ event)
  (define event-kind (event-mouse-kind event))
  (define click (cond ((equal? event-kind 0) 'down) ((equal? event-kind 3) 'up) (else #f)))
  (define char (key-event-char event))


  (define focused #f)

  (for-each
    (lambda (notification)
            (let* ([area (get-area notification)]
                   [prev-click (get-click notification)]
                   [within-area? (mouse-event-within-area? event area)])
                  (when (and within-area? (equal? click 'down))
                        (unless (equal? prev-click 'clicked) (set-click! notification 'down)))
                  (when (and within-area? (equal? click 'up) (equal? prev-click 'down))
                        (set-click! notification 'clicked))
                  (when (and (not within-area?) click)
                        (set-click! notification #f))

                  (when (and (get-click notification) (char? char))
                        (when (equal? char #\y)
                              (set-register! #\" (list (string-join (Notification-lines notification) "\n")))
                              (set-box! (Notification-blink notification) #t)))
                  (when (and (get-click notification) (key-event-escape? event))
                        (set-click! notification #f))
                  (when (and (get-click notification) (key-event-tab? event) (not focused))
                        (set! focused #true)
                        (notify-focus-next (Notification-id notification)))))
    *notifications-queue*)

  (if (get-clicked) event-result/consume event-result/ignore))

(define (schedule-drop notification timeout)
  (enqueue-thread-local-callback-with-delay
    timeout
    (lambda () (if (get-click notification) 
                   (schedule-drop notification 1000)
                   (begin
                     (pop-by-id (Notification-id notification))
                     (when (no-notifications?) (pop-last-component-by-name! COMPONENT-NAME)))))))
  
(define (render-notification notification rect frame)
  (calculate-popup-geometry rect notification)

  (unless (hidden? notification)
    (define outer-area (get-area notification))
    (define inner-area-x (+ (area-x outer-area) 1))
    (define inner-area-y (+ (area-y outer-area) 1))
    (define inner-width (- (area-width outer-area) 2))
    (define inner-height (- (area-height outer-area) 2))

    (define border-style (get-style notification))

    (define block (make-block (theme->bg *helix.cx*) border-style "all" "rounded"))
    (buffer/clear frame outer-area)
    (block/render frame outer-area block)

    (define header-area (area inner-area-x (+ inner-area-y 1) inner-width 1))
    (define header-line (make-block (theme->bg *helix.cx*) border-style "top" "plain"))
    (block/render frame header-area header-line)

    (render-lines frame (+ inner-area-x 1) inner-area-y notification (bg-style notification))

    notification))

;; A helper over `frame-set-string!` to render a list of lines, one list member per line
(define (render-lines frame start-x start-y notification bg-style)
  (define normal-style (theme-scope *helix.cx* "normal"))

  (frame-set-string! frame start-x start-y (get-header notification) (get-style notification))
  (frame-set-string! frame start-x (+ start-y 1) "" normal-style)

  (let loop ((lines (Notification-lines notification)) (y (+ start-y 2)))
    (when (not (null? lines))
      (frame-set-string! frame start-x y (car lines) bg-style)
      (loop (cdr lines) (+ y 1)))))

(define (calculate-content-width lines)
  (if (null? lines)
      0
      (apply max (map string-length lines))))

;; `get-popup-geometry` is a pure stateless version of `calculate-popup-geometry`
;; It knows only about an area needed for the text
;; It doesn't know anything about y position nor available area
;; `get-popup-geometry-rect` knows these attributes and can "shrink" the rectangle
(define (get-popup-geometry notification)
  (define text-lines (Notification-lines notification))
  (define heading (get-header notification))
  (define line-count (length text-lines))
  (define content-width (calculate-content-width (cons heading text-lines)))
  (define final-width (+ (exact (max MIN-WIDTH content-width)) 4))
  (define final-height (+ line-count 4))

  (list final-width final-height))

(define (calculate-popup-geometry rect notification)
  (define screen-width (area-width rect))
  (define screen-height (area-height rect))

  (define popup-square (get-popup-geometry notification))
  (define final-width (min screen-width (car popup-square)))
  (define final-height (min screen-height (cadr popup-square)))

  ; Position at top right corner
  (define x-pos (- screen-width final-width))
  (define y-pos (get-top-y (Notification-id notification)))
  
  (define geometry (area x-pos y-pos final-width final-height))

  (set-hidden! notification (< screen-height (+ y-pos final-height)))
  (set-area! notification geometry))
