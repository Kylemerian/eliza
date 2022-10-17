; заготовка "Доктора". Сентябрь 2022
#lang racket
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; 6 структура ключевых слов и шаблонов для них
(define keywords-structure '#(
    #(
        #(depressed suicide exams university)
        #(
            (when you feel depressed, go out for ice cream)
            (depression is a disease that can be treated)
            (did you feel depressed because of exams?)
            (is this connected with study at university?)
        )
    )
    #(
        #(mother father parents brother sister uncle aunt grandma grandpa)
        #(
            (tell me more about your * , i want to know all about your *)
            (why do you feel that way about your * ?)
            (are you in good relationship with your * ?)
            (tell me a little bit about your * ?)
        )
    )
    #(
       #(university scheme lections study seminars seminar lection)
       #(
            (your education is important)
            (how much time do you spend on your studies ?)
            (what do you think about * ?)
            (do you have any troubles with * ?)
       )
    )
    #(
        #(scheme lisp scala)
        #(
            (functional programming is a most attractive programming paradigm isnt it ?)
            (* rules whole world !)
            (i have heard many good things about *)
            (which functional language do you prefer?)
        )
    )
    #(
        #(c++ java c#)
        #(
            (object-oriented programming is a prettiest programming paradigm ever isnt it ?)
            (* is the best one !)
            (* is a reaaly good way to start programming)
            (* is attractive programming language!)
        )
    )
    #(
        #(sunny cloudy foggy wet)
        #(
            (weather could affect your current state)
            (do you like when it is * ?)
            (i really like when it is *)
            (i have heard that next week it will be *)
        )
    )
    #(
        #(maths algebra geometry physics chemistry biology)
        #(
            (do you have such a big experience in *)
            (which is your favourite subject among exact sciences)
            (it is really cool to see a person who in *)
            (* could help to solve some emotional problems)
        )
    )
    )
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
    (let ((length (vector-length vctr)))
        (let loop ((i 0) (result init))
            (if (= i length) 
                result
                (loop
                    (add1 i)
                    (f i result (vector-ref vctr i))
                )
            )
        )
    )
)
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
    (let ((length (vector-length vctr)))
        (let loop ((i (sub1 length)) (result init))
            (if (= i -1)
                result
                (loop
                    (sub1 i)
                    (f i result (vector-ref vctr i))
                )
            )
        )
    )
)
; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
    (printf "Hello, ~a!\n" name)
    (print '(what seems to be the trouble?))
    (doctor-driver-loop-v2 name)
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
        (cond 
	        (
                (equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                (printf "Goodbye, ~a!\n" name)
                (print '(see you next week))
                (newline)
            )
            (else
                (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                (doctor-driver-loop name)
            )
        )
    )
)
; 5 многопользовательская версия диалога Доктора
(define (visit-doctor-v2 stop-word patients-number)
    (let loop ((patients-count patients-number)) ; Итерируемся по пациентам
        (if (= patients-count 0) 
            `(time to go home)
            (let ((patient-name (ask-patient-name))) ; Считывание имени
                (cond ((equal? patient-name stop-word) `(time to go home)) ; Завершение диалога по "time to go home"
                    (else (printf "Hello, ~a!\n" patient-name)
                        (print '(what seems to be the trouble?))
                        (doctor-driver-loop-v2 patient-name)
                        (loop (- patients-count 1))
                    )
                )
            )
        )
    )
)

; 5
(define (ask-patient-name)
    (begin
        (println '(next!))
        (println '(who are you?))
        (print '**)
        (car (read))
    ) 
)

; 4
(define (doctor-driver-loop-v2 name)
    (let loop ((rep-history #()))
        (newline)
        (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
        (let ((user-response (read)))
            (cond 
                ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                    (printf "Goodbye, ~a!\n" name)
                    (print '(see you next week))
                    (newline)
                )
                (else
                    (print (reply-v3 user-response rep-history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                    (loop (vector-append (vector user-response) rep-history)) ; сохраняем реплику в историю пользователя
                )
            )
        )
    )
)

;4
;генерация ответной реплики по user-response -- реплике от пользователя
(define (reply-v2 user-response rep-history)
  (if (= 0 (vector-length rep-history))
      (reply user-response)
      (case (random 3) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge-answer))  ; 2й способ
          ((2) (history-answer rep-history)) ; 3й способ
      )
  )
)

; 6
(define (reply-v3 user-response rep-history)
  (case (random (if (= 0 (vector-length rep-history)) 1 0) (if (check-keywords? user-response) 4 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
    ((1) (qualifier-answer user-response)) ; 1й способ  (всегда)
    ((2) (hedge-answer))  ; 2й способ
    ((0) (history-answer rep-history))
    ((3) (keywords-answer user-response)) 
  )
)

; 7
(define reply-strategies
    (list
        (list 2  (lambda(x y) #t) (lambda(x y)(hedge-answer)))
        (list 5  (lambda(x y) #t) (lambda(x y)(qualifier-answer x)))
        (list 9  (lambda(x y) (if (= 0 (vector-length y)) #f #t)) (lambda(x y)(history-answer y)))
        (list 14 (lambda(x y) (if (check-keywords? x) #t #f)) (lambda(x y)(keywords-answer x))) 
    )
)

; 7
(define (reply-v4 reply-strategies user-response rep-history)
    (let ((strategy-list (filter (lambda (x)((cadr x) user-response rep-history)) reply-strategies)))
        (let ((weight (foldl (lambda(x y)(+ (car x) y)) 0 strategy-list)))
            (let loop ((rand (random weight)) (p (caar strategy-list)) (current-strategy (car strategy-list)) (other-strategies (cdr strategy-list)))
                (if (<= rand p)
                    ((caddr current-strategy) user-response rep-history)
                    (loop (- rand p) (caar other-strategies) (car other-strategies) (cdr other-strategies))
                )
            )
        )
    )
)

; 6
(define (check-keywords? user-response)
    (if (null? user-response)
        #f
        (if (vector-member? all-keywords (car user-response))
            #t
            (check-keywords? (cdr user-response))
        )
    )
)

; 6
(define (vector-member? vctr elem)
    (
        let ((vctr-length (vector-length vctr)))
        (
            let loop ((idx 0))
            (if (>= idx vctr-length)
                #f (
                    if (equal? elem (vector-ref vctr idx))
                        #t
                        (loop (+ idx 1))
                )
            )
        )
    )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
    (vector-ref vctr (random 0 (vector-length vctr)))
)


; случайный выбор одного из элементов непустого списка
(define (pick-random-list lst)
    (list-ref lst (random 0 (length lst)))
)

;4
(define (history-answer rep-history)
    (append `(earlier you said that) (change-person (pick-random-vector rep-history)))
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
    (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
        ((0) (hedge-answer))  ; 1й способ
        ((1) (qualifier-answer user-response)) ; 2й способ
    )
)

;6
(define (keywords-answer user-response)
    (
        let* (
            (rand-word (pick-random-list (filtered-user-response user-response)))
            (phrases-list (
                    pick-random-vector (get-doctor-responses rand-word)
                )
            )
        )
        (many-replace-v3
            (list (list '* rand-word))
            phrases-list
        )
    )
)

;6
; фильтр, оставляющий ключевые слова
(define (filtered-user-response user-response)
    (filter (lambda (rand-word) (vector-member? all-keywords rand-word)) user-response)
)


; вектор шаблонов ответа по ключевому слову
(define (get-doctor-responses rand-word)
    (vector-foldl
        (lambda
            (idx result x)
            (vector-append result x)
        )
        #()
        (vector-map
            (lambda (vct) (vector-ref vct 1))
            (vector-filter
                (lambda (t)
                    (vector-member? (vector-ref t 0) rand-word)
                )
                keywords-structure
            )
        )
    )
)

; список всех ключевых слов
(define all-keywords
    (vector-foldl 
        (lambda
            (i result t) (
                vector-append result (
                    vector-filter-not
                    (lambda (keyword) (vector-member? result keyword))
                    (vector-ref t 0)
                )
            )
        )
        '#()
        keywords-structure
    )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
    (pick-random-vector
        '#((please go on)
            (many people have the same sorts of feelings)
            (many of my patients have told me the same thing)
            (please continue)
            (just pay no attention to that)
            (it is absolutely normal to have problems like that)
            (please tell me details)
        )
    )
)


; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append
            (pick-random-vector
                '#((you seem to think that)
                    (you feel that)
                    (why do you believe that)
                    (why do you say that)
                    (why do you suppose that)
                    (it is not good you consider that)
                    (it is awful that)
                )
            )
            (change-person user-response)
        )
 )

; замена лица во фразе
(define (change-person phrase)
    (   
        many-replace-v3
        '((am are)
            (are am)
            (i you)
            (me you)
            (mine yours)
            (my your)
            (myself yourself)
            (you i)
            (your my)
            (yours mine)
            (yourself myself)
            (we you)
            (us you)
            (our your)
            (ours yours)
            (ourselves yourselves)
            (yourselves ourselves)
            (shall will)
        )
        phrase
    )
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
    (cond ((null? lst) lst)
        (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (cons 
                    (if pat-rep
                        (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                        (car lst) ; иначе в начале ответа помещается начало списка без изменений
                    )
                    (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                )
            )
        )
    )
)

; 2
(define (many-replace-v2 replacement-pairs lst)
    (let loop ((lst lst) (res '()))
        (if (null? lst) ; в фразе не осталось слов -> 
            (reverse res) ; возвращаем res 
            (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (loop
                    (cdr lst)
                    (cons (if pat-rep
                            (cadr pat-rep)
                            (car lst)
                        )
                        res
                    )
                )
            )
        )
    )
)

; 3
(define (many-replace-v3 replacement-pairs lst)
    (map
        (lambda (x)
            (let ((pat-rep (assoc x replacement-pairs)))
                (if pat-rep
                    (cadr pat-rep)
                    x
                )
            )
        )
        lst
    )
)