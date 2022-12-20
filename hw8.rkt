#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
 count-compares)

; Please do not change lines above this one.

;************************************************************
; CS 201 HW #8  DUE Monday May 10th at 11:59 pm, 
; via the submit system on the Zoo.  This assignment is worth 90 points.
; This problem set is optional.  It can replace your lowest hw grade.
;************************************************************
; Name:Chocho Hu
; Email address: hudie.hu@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;******************************(******************************

(define (lorint count bound)
  (if (= 0 count)
      '()
      (cons (random 0 bound)
       (lorint (- count 1) bound))
       )
  )

(define (time-calls reps proc args)
  (let ((c1 (current-inexact-milliseconds))
         (c2 (time-calls-helper reps proc args)))
  (- c2 c1))
;  (print (current-inexact-milliseconds))
;  (print "|")
;  (print (time-calls-helper reps proc args))
  )

;helper function to run the proc
(define (time-calls-helper reps proc args)
  (apply proc args)
  (if (= 0 reps)
      (current-inexact-milliseconds)
      (time-calls-helper (- reps 1) proc args)
      )
  )


;************************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For the procedures length, take, and drop, replace length-runtime,
; take-runtime, and drop-runtime with either O(1) or O(n) to most accurately
; reflect each procedure's respective running time as a function of the length n
; of the list argument.

; Compare the times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest? Replace length-take-drop-ordering with an ordering of these
; procedures. Then, in the space below, explain *why* on the basis of how lists and
; their operations are implemented. (Complex statistical analysis is not
; necessary.)
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; Please report measurements here.

(define length-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.

(define take-runtime "O(logn)")
; Please briefly comment below on how your measurements support this conclusion.

(define drop-runtime "O(1)")
; Please briefly comment below on how your measurements support this conclusion.

; Please do not use commas, and please order from fastest to slowest
; e.g. (define length-reverse-powerset-ordering '(length reverse powerset))
(define length-take-drop-ordering '(drop take length))

; Please explain your ordering here.

;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
;           (compare? x y) 
;           #t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
;           (equal? x y) 
;           #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
;           (compare? x z) 
;           #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))

; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct. Replace "replace" in total-order-runtime
; with your answer.

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define total-order-runtime "replace")
; Explain your answer here.

(define (total-order? compare? domain)
  (if (< (length domain) 2)
      'domainlength-less-than-2
      (if (= (length domain) 2)
          (compare compare? (car domain) (cadr domain))
          (and
           (compare compare? (car domain) (cadr domain))
           (total-order? compare? (cons (car domain) (cddr domain)));first and others
           (total-order? compare? (cdr domain));second and others
           )
          )
      ))

;calculate compare
(define (compare compare? x y)
  (cond
    [(if (equal? x y) 
         (compare? x y) 
         #t)]
    [(if (and (compare? x y) (compare? y x)) 
         (equal? x y) 
         #t)]
;    [(if (and (compare? x y)(compare? y z)) 
;         (compare? x z) 
;         #t)]
    [(or (compare? x y) (compare? y x)) #t]
    [else #f]
    )
  )


;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangement"))
;'("the" "hello" "best" "arrangement")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(require racket/trace)
(define (sorted? compare? lst)
  (if (< (length lst) 2)
      'lstlength-less-than-2
      (if (= (length lst) 2)
          (compare? (car lst) (cadr lst))
          (and
           (compare? (car lst) (cadr lst))
           (sorted? compare? (cons (car lst) (cddr lst)));first and others
           (sorted? compare? (cdr lst));second and others
           )
          )
      ))
;(trace sorted?)

(define (insert compare? item lst [lst2 '()])
 (if (empty? lst)
     (append lst2 (list item))
     (if (compare? item (car lst))
         (append lst2 (cons item lst))
         (insert compare? item (cdr lst) (append lst2 (list (car lst))))
         )
     )
  )

(define (merge compare? lst1 lst2)
 (if (empty? lst2)
     lst1
     (merge compare? (insert compare? (car lst2) lst1) (cdr lst2))
     )
  )

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare? lst [lst2 '()])
 (if (empty? lst)
     lst2
     (isort compare? (cdr lst) (insert compare? (car lst) lst2))
  ))

(define (msort compare? lst [lst2 '()])
 (if (empty? lst)
     lst2
     (msort compare? (cdr lst) (merge compare? (list (car lst)) lst2))
  ))

;************************************************************
; ** problem 6 ** (20 points)
; (1)(a) Give empirical evidence that your implementation of insertion sort
;        (isort, above) has best case time Omega(n) and worst case time of
;        O(n^2).
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.
; (2)(a) Give empirical evidence that your implementation of merge sort
;        (msort, above) has best case and worst case times of Theta(n log n).
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.

; Be sure to use sufficiently long lists of integers and possibly repeat/average
; measurements.

; (3) Please identify inputs that give best and worst cases for your
; implementations of (a) isort and (b) msort. Be sure that you use sufficiently
; long lists of randomly chosen integers in a range larger than the length of
; the list, so that there are unlikely to be many duplicate values.

; (4) Roughly what is the longest list of random integers that your (a) isort
; procedure can sort in 10 seconds?  Same question for your (b) msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily be uniform over the whole range of feasible input lengths.
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; 3a) Please briefly describe best case inputs for your implementation of isort here.

; 3a) Please briefly describe worst case inputs for your implementation of isort here.

; 1b) Please indicate average case running time of your implementation of isort here.

; 3b) Please briefly describe best case inputs for your implementation of msort here.

; 3b) Please briefly describe worst case inputs for your implementation of msort here.

; 2b) Please indicate average case running time of your implementation of msort here.

; 1a, 2a) Please provide evidence for the above claims here.

; 4a, 4b) Please indicate the longest list of random integers that a) your isort
; procedure can sort in 10 seconds and b) your msort procedure can sort
; in 10 seconds.

;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************

(define count 0)
(define (compare! compare?)
  (set! count (+ 1 count))
  compare?
  )

(define (count-compares sort compare? lst) 
  (if (sorted? (compare! compare?) lst)
      count
      (count-compares sort (compare! compare?) (sort (compare! compare?) lst))
      ))
  
;************************************************************
; ** problem 8 ** (0 points)
; This is where the test code normally appears.
; For this assignment, write your own tests.  

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))




;********* end of hw8, end of hws! **************************


