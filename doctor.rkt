; заготовка "Доктора". Сентябрь 2022
#lang racket
(require racket/vector)
(require racket)

; В учебных целях используется базовая версия Scheme

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

(define (visit-doctor-v2 stop-word patients-number)
    (let loop ((patients-count patients-number))
        (if (= patients-count 0)
            `(time to go home)
            (let ((patient-name (ask-patient-name)))
                (cond ((equal? patient-name stop-word) `(time to go home))
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

;4
(define (doctor-driver-loop-v2 name)
    (let loop ((rep-history #()))
        (newline)
        (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
        (let ((user-response (read)))
            (cond 
                ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                    (printf "Goodbye, ~a!\n" name)
                    (print '(see you next week))
                )
                (else
                    (print (reply-v2 user-response rep-history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                    (loop (vector-append (vector user-response) rep-history))
                )
            )
        )
    )
)

; 1-4
; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply-v2 user-response rep-history)
  (if (= 0 (vector-length rep-history))
      (reply user-response)
      (case (random 3) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge-answer))  ; 2й способ
          ((2) (history-answer rep-history))
      )
  )
)

; 1-4
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

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
    (vector-ref vctr (random 0 (vector-length vctr)))
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

;2
(define (many-replace-v2 replacement-pairs lst)
    (let loop ((lst lst) (res '()))
        (if (null? lst)
            (reverse res)
            (let ((pat-rep (assoc (car lst) replacement-pairs)))
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

;3
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
