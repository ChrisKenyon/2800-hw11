; **************** BEGIN INITIALIZATION FOR ACL2s B MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.


Pete Manolios
Thu Jan 27 18:53:33 EST 2011
----------------------------

The Beginner level is the next level after Bare Bones level.

|#

; Put CCG book first in order, since it seems this results in faster loading of this mode.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg/ccg" :uncertified-okp nil :dir :acl2s-modes :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "base-theory" :dir :acl2s-modes)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil :ttags :all)

;Settings common to all ACL2s modes
(acl2s-common-settings)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading trace-star and evalable-ld-printing books.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil)
(include-book "hacking/evalable-ld-printing" :uncertified-okp nil :dir :system :ttags ((:evalable-ld-printing)) :load-compiled-file nil)

;theory for beginner mode
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s beginner theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "beginner-theory" :dir :acl2s-modes :ttags :all)


#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Beginner mode.") (value :invisible))
;Settings specific to ACL2s Beginner mode.
(acl2s-beginner-settings)

; why why why why 
(acl2::xdoc acl2s::defunc) ; almost 3 seconds

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner
#|

CS 2800 Homework 11 - Spring 2017

This homework is to be done in a group of 2-3 students. 

If your group does not already exist:

 * One group member will create a group in BlackBoard
 
 * Other group members then join the group
 
 Submitting:
 
 * Homework is submitted by one group member. Therefore make sure the person
   submitting actually does so. In previous terms when everyone needed
   to submit we regularly had one person forget but the other submissions
   meant the team did not get a zero. Now if you forget, your team gets 0.
   - It wouldn't be a bad idea for group members to send confirmation 
     emails to each other to reduce anxiety.

 * Submit the homework file (this file) on Blackboard.  Do not rename 
   this file.  There will be a 10 point penalty for this.

 * You must list the names of ALL group members below, using the given
   format. This way we can confirm group membership with the BB groups.
   If you fail to follow these instructions, it costs us time and
   it will cost you points, so please read carefully.


Names of ALL group members: Chris Kenyon, Izaak Branch

Note: There will be a 10 pt penalty if your names do not follow 
this format.
|#

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Tail Recursion
A tail call is a function call performed as the final action of a function.
A tail call that calls the same function is tail recursive.  Tail recursive
calls have the special property that additions to the call stack can be 
eliminated, making the function equivalent to a non-recursive loop for each 
tail recursive call.

Converting a recursive function to a tail recursive form can substantially
improve performance. Frequently the tail recursive form of a function
uses additional parameters called accumulators which store calculated values. 
A new function that is equivalent to the original function is then defined 
non-recursively using the tail recursive function.  One must then prove that 
the original function is equivalent to the new function.  For more about this 
process, see the lecture notes.

We will use this convention for naming functions:

foo     Original function
foo-t   Tail recursive function using accumulators. The "-t" is for "tail".
foo*    Non-recursive function with the same signature as foo but using foo-t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For (parts of) this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw11.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.

- when done, save your file and submit it as hw11.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instructions for programming problems:

For each function definition, you must provide both contracts and a body.

You must also ALWAYS supply your own tests. This is in addition to the
tests sometimes provided. Make sure you produce sufficiently many new test
cases. This means: cover at least the possible scenarios according to the
data definitions of the involved types. For example, a function taking two
lists should have at least 4 tests: all combinations of each list being
empty and non-empty.

Beyond that, the number of tests should reflect the difficulty of the
function. For very simple ones, the above coverage of the data definition
cases may be sufficient. For complex functions with numerical output, you
want to test whether it produces the correct output on a reasonable
number of inputs.

Use good judgment. For unreasonably few test cases we will deduct points.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A. Warm-up Induction Proof
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A1: Sorted Insert

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consider the following  functions.
(defdata lor (listof rational))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; sortedp: All -> boolean
;; (sortedp l) takes any variable l and returns true 
;; if and only if l is a list of rational numbers
;; and the elements  are in non-decreasing order 
;; (ie they are sorted)
(defunc sortedp (l)
  :input-contract t
  :output-contract (booleanp (sortedp l))
  (cond ((not (lorp l))   nil)
        ((or (endp l)(endp (rest l)))  t)
        (t   (and (<= (first l) (second l)) 
                  (sortedp (rest l))))))

(check= (sortedp '(-1 -1/2 0 4 9/2)) t)
(check= (sortedp '(-1 -1/2 0 4 7/2)) nil)
(check= (sortedp nil) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; insert: Rational x LOR -> LOR
;; (insert e l) takes a rational e and a list
;; of rationals l and inserts e into l such
;; that e is strictly greater than all elements
;; in l before where it is inserted.
(defunc insert (e l)
  :input-contract (and (rationalp e)(lorp l))
  :output-contract (lorp (insert e l))
  (cond ((endp l) (cons e l))
        ((<= e (first l)) (cons e l))
        (t        (cons (first l) (insert e (rest l))))))

;; Prove the returned list from insert is still sorted
;; if it was sorted at the start.
(defthm phi_insert (implies (and (rationalp e)(lorp l)
                                 (sortedp l))
                            (sortedp (insert e l))))
#|

1. ~((rationalp e)/\(lorp l)) => phi_insert
2. (rationalp e)/\(lorp l)/\(endp l) => phi_insert
3. (rationalp e)/\(lorp l)/\~(endp l)/\(<= e (first l)) => phi_insert
4. (rationalp e)/\(lorp l)/\~(endp l)/\~(<= e (first l))/\phi_insert|((l (rest l))) => phi_insert

Obligation 1. ~((rationalp e)/\(lorp l)) => phi_insert
C1. ~((rationalp e)/\(lorp l))
(implies (and (rationalp e)(lorp l)(sortedp l))  (sortedp (insert e l))))
{C1, PL}
t

Obligation 2. (rationalp e)/\(lorp l)/\(endp l) => phi_insert
C1. (rationalp e)
C2. (lorp l)
C3. (endp l)
---------------
C4. (endp (rest (cons e l))) {first-rest ax, C3}
(sortedp (insert e l))
= {def. insert, C2,C3}
(sortedp (cons e l))
= {C4, def. sortedp}
t

Obligation 3. (rationalp e)/\(lorp l)/\~(endp l)/\(<= e (first l)) => phi_insert
|#
(defthm L1 (implies (and (lorp l)(not (endp l)))
                    (equal (second (cons a l))(first l))))
#|
Lemma 1 proof
C1. (lorp l)
C2. (not (endp l))
(second (cons a l)) = (first l)
={def. second, C1,C2}
(first (rest (cons a l))) = (first l)
= {first-rest ax}
(first l) = (first l)
t

C1. (rationalp e)
C2. (lorp l)
C3. ~(endp l)
C4. (<= e (first l))
C5. (sortedp l)
---------------
C6. (not (endp (rest (cons e l)))) {C3,first-rest ax}
C7. (and (<= (first (cons e l))(second (cons e l)))) {L1, C4}

(sortedp (insert e l))
= {def. insert, C2,C3,C4}
(sortedp (cons e l))
{def. sorted, C2, C3, C6,C7}
t

Obligation 4. (rationalp e)/\(lorp l)/\~(endp l)/\~(<= e (first l))/\phi_insert|((l (rest l))) => phi_insert
C1. (rationalp e)
C2. (lorp l)
C3. ~(endp l)
C4. ~(<= e (first l))
C5. (rationalp e)/\(lorp (rest l))/\(sortedp (rest l)) => (sortedp (insert e (rest l)))
C6. (sortedp l)
---------------
C7. (lorp (rest l)) {C2,C3,first-rest ax}

Case 1 -- C8. (endp (rest l))
C9. (sortedp (rest l)) {C7,C8, def. sortedp|((l (rest l)))}
C10. (< (first l) e) {C4, arith}

(sortedp (insert e l))
= {def. insert, C2,C3,C4}
(sortedp (cons (first l)(insert e (rest l))))
= {def. insert, C8}
(sortedp (cons (first l)(cons e '())))
= {def. sortedp, def. cons, first-rest ax}
(and (<= (first l) e)(sortedp (rest l)))
= {C9,C10}
t

Case 2 -- C8. (not (endp (rest l)))
C9. (sortedp (rest l)) {C6,C2,C3,C8, def. sortedp|((l l))}
C10. (sortedp (insert e (rest l))) {C1,C7,C9,C5,MP}
C11. ~(endp (insert e (rest l))) {C8, def cons}
C12. (< (first l) e) {C4,arith} 

(sortedp (insert e l))
= {def. insert, C2,C3,C4}
(sortedp (cons (first l)(insert e (rest l))))
= {def. sortedp, def. cons, first-rest ax, C11,L1,C4,C12}
(sortedp (insert e (rest l)))
= {C10}
t
                  
QED

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Section B. Tail Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rev-t and rev* are provided purely for efficiency purposes. You do not 
;; need to use them. You may also assume rev* = rev (we did this in class)
;; and just use rev in your proofs (it's easier)
(defunc rev-t (l acc)
  :input-contract (and (listp l)(listp acc))
  :output-contract (listp (rev-t l acc))
  (if (endp l) acc (rev-t (rest l)(cons (first l) acc))))

(defunc rev* (l)
  :input-contract (listp l)
  :output-contract (listp (rev* l))
  (rev-t l nil))

(defthm rev-lemma (implies (and (listp l)(listp acc))
                           (equal (rev-t l acc)(app (rev l) acc))))
(defthm ph_rev (implies (listp l) (equal (rev* l) (rev l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question B1: A faster swap?
;; Below is a function swap-all, which replaces all instances
;; of an element in a list with a new element.
;; Write a tail recursive version of swap-all which uses
;; an accumulator. Call this swap-all-t. 
;; You can use rev* above and replace it in your proofs with rev
;; (we proved their equivalence in class and
;; I just want swap-all-t to be as fast as possible).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; swap-all: All x All x List -> List
;; Goes through list l and replaces all occurance of element e
;; with element f.
(defunc swap-all (e f l)
  :input-contract (listp l)
  :output-contract (listp (swap-all e f l))
  (cond ((endp l)            l)
        ((equal e (first l)) (cons f (swap-all e f (rest l))))
        (t                   (cons (first l)(swap-all e f (rest l))))))
                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swap-all-t: All x All x list x List -> List
;; (swap-all e f l acc) replaces all elements e
;; in list f with element f. Acc is an accumulator
(defunc swap-all-t (e f l acc)
  .............

;; Write a function swap-all* that calls swap-all-t and has
;; the same input and output contracts as swap-all
..............



#|

Finally: Prove swap-all* = swap-all

A) Write a conjecture (use contract checking and completion)
that  accurately describes the equality we are trying to 
show.
   ............


B) What is the lemma demonstrating the relationship between 
swap-all-t, acc, and swap-all.
   ............


C) Prove your conjecture from A using the lemma from B 
   (and assuming it's valid).
   .................

D) Prove the swap-all-t lemma from B) above.
 ...........
 
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B2: Arithmetic Tail Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Recall the definition of pow from homwork 9
(defunc pow (x p)
  :input-contract (and (rationalp x) (natp p))
  :output-contract (rationalp (pow x p))
  (if (equal p 0)
    1
    (* x (pow x (- p 1)))))

#|
Write functions pow-t and pow* (following the pattern
we've established).  Make sure they are admissible and you provide
appropriate tests.
|#
............


#|
Now prove that pow* is equivalent to pow: 
phi_pow: (rationalp x)/\(natp p) => ((pow* x p) = (pow x p))

............


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B3: Insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-t can be difficult to admit, depending on the implementation
;; Let's make your life easier. Just convince yourself that insert-t terminates
;; and works as expected.
:program

;;A) Define insert-t below such that insert* should be equivalent to insert.
;;   You should make both functions as efficient as possible (so don't call app on
;;   recursive calls).
;;   Notice you don't necessarily have to store the final answer in acc
................


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; insert*: Rational x LOR -> LOR
;; (insert* e l) takes a rational e and a list
;; of rationals l and inserts e into l such
;; that e is strictly greater than all elements
;; in l before where it is inserted.
(defunc insert* (e l)
  :input-contract (and (rationalp e)(lorp l))
  :output-contract (lorp (insert* e l))
  (insert-t e l nil))

#|
B) Prove that insert* is equivalent to insert (the lists do not have
to be sorted):
    phi_insert_eq: (lorp l)/\(rationalp e) => ((insert* e l) = (insert e l))
    
...............


|#

#|
C) Finally, explain why the following is a theorem:

phi_insert*: (rationalp e) /\ (lorp l) /\ (sortedp l)
             => (sortedp (insert* e l))
             
HINT: this question is worth only one point AND you won't need induction. No
need for a formal proof.

..............


|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION C: Speed Tests I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
:logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; gen-lor-t: nat x integer x pos x lor -> lor
;; (gen-lor-t n i denom acc) takes a list size 
;; n, an increment value i and a divisor value denom and
;; returns a "random" list of n rationals. The
;; variable acc is an accumulator to speed up the
;; function.
;; gen-lor-t can be used to test your insert functions
(defunc gen-lor-t (n i max denom acc)
  :input-contract (and (natp n)(integerp i)(posp max)(lorp acc)
                       (posp denom))
  :output-contract (lorp (gen-lor-t n i max denom acc))
  (cond ((equal n 0) (cons i acc))
        ((> i max) (gen-lor-t (- n 1) (unary-- i) max denom
                              (cons (/ (+ n i) denom) acc)))
        (t         (gen-lor-t (- n 1) (unary-- (+ i n)) max denom
                              (cons (/ (+ n i) denom) acc)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; gen-lor*: nat -> lor
;; (gen-lor* n) generates a list of rationals length
;; n with "semi-random" values inside.
;; This is a wrapper function for gen-lor-t to extract
;; away complexity for generating a list. Modify
;; the numbers all you want.
(defunc gen-lor* (n)
  :input-contract (natp n)
  :output-contract (lorp (gen-lor* n))
  (gen-lor-t n -2 23 5 nil))

(defconst *big-list* (gen-lor* 20000))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C1.
;; Now that we've proven insert = insert*, let's show that insert* is
;; faster than insert

(acl2::er-progn
   (acl2::time$ (acl2::value-triple (insert 2000 *big-list*)))
   (acl2::value-triple nil))
;; How many seconds does this take?
;; ..........................


(acl2::er-progn
   (acl2::time$ (acl2::value-triple (insert* 2000 *big-list*)))
   (acl2::value-triple nil))
;; How many seconds does this take?
;;..........................


;; Wait. What??  Briefly explain why you (presumably) didn't see a speed
;; by using a tail recursive function.  Don't worry.  We'll
;; see performance improvements soon. It's just important
;; to see when accumulators help and when they don't.

#|
 .........................
 <Insert your explanation here>

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C2. Revisiting Swap-all

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; gen-dup-list-t: all x nat x list
;; (gen-dup-list e n acc) makes a list of n elements
;; with half the values being the index and the rest
;; being element e.  Hence swap-all should swap half
;; the elements in the generated list.
(defunc gen-dup-list-t (e n acc)
  :input-contract (and (natp n)(listp acc))
  :output-contract (listp (gen-dup-list-t e n acc))
  (cond ((equal n 0) acc)
        ((natp (/ n 2)) (gen-dup-list-t e (- n 1) (cons n acc)))
        (t  (gen-dup-list-t e (- n 1) (cons e acc)))))
    
(defconst *big-swap-list* (gen-dup-list-t 'foo 20000 nil))

(acl2::er-progn
   (acl2::time$ (acl2::value-triple (swap-all* 'foo 'bar *big-swap-list*)))
   (acl2::value-triple nil))
;; How many seconds does this take?
..........................

(acl2::er-progn
   (acl2::time$ (acl2::value-triple (swap-all 'foo 'bar *big-swap-list*)))
   (acl2::value-triple nil))
;; How many seconds does this take?
..........................

#|
Now (temporarily) change the definition of *big-swap-list* 
to have 50000 elements and run the speed tests again.
What happened?
................

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; D. Dynamic Programming and Tail Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

In this part of the assignment you will:

- understand why the classical textbook definition of the Fibonacci function is slow;
For more information on Fibonacci numbers see  https://en.wikipedia.org/wiki/Fibonacci_number

- implement two different techniques to make it much more efficient, including understanding why:

  * one algorithmic technique known as Dynamic Programming.
  * the other technique is more low-level and known as Tail Recursion.

- compare the effect of these two techniques.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
D1. Exploring the Fibonacci series.

(fib 0) = 0 (traditional Fibonacci numbers stop at 1 but adding 0 can make it easier to code)
(fib 1) = 1
(fib 2) = 1
For any integer value n > 2 (fib n) = (fib (n-1)) + (fib (n-2))
The first 7 Fibonacci numbers (including 0) are thus:
0 1 1 2 3 5 8

Thus the function can be naively written like the function below.

|#
;; We reduce the amount of testing done by ACL2 while it admits a function:
(acl2::acl2s-defaults :set acl2::testing-enabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fib: Nat -> Nat
;; (fib n) takes a natural number n
;; and returns the nth Fibonacci number.
(defunc fib (n)
  :input-contract (natp n)
  :output-contract (natp (fib n))
  (cond ((equal n 0)  0)
        ((equal n 1)  1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(check= (fib 1) 1)
(check= (fib 6) 8)
(check= (fib 8) 21)

#|
This is an elegant and very easy to read definition. But it is also very
slow. To see why, we trace the recursive calls to fib when processing
input. This is done as follows:
|#

(acl2::trace! fib)
(acl2::set-guard-checking :none)

#|

(Hint: you can use (acl2::untrace$ ...) to stop tracing a function.)

Now try fib using some _small_ inputs by typing calls to fib in the REPL. 
Start with n=1, n=2, to get a
feeling for the output trace produces. A line of the form

2> (FIB 4)

indicates that fib was called recursively with argument 4, 
whereas a line of the form

<2 (FIB 3)

indicates that the most recent recursive call beginning with ">2" returned,
and the result is 3. (The 3 is the result, not the argument to fib)

In the evaluation of (fib  6), how many times is fib called on argument 2 ?
(ie how many lines start with #> and end with FIB 2)
........


In the evaluation of (fib 8), how many times is fib called on argument 2 ? 
...........


Hint: you can use the Eclipse editor to count occurrences of certain text
strings, or you can copy the output of trace into your favorite alternative
editor.  However, the numbers aren't as huge as they hvae been in the past
so counting manually is possible.

Notice what happens to the number of recursive calls as
n becomes larger?
|#
(acl2::untrace$ fib)

(defdata lon (listof nat))

:program

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
D2. FIB-FAST
Write a revisted fib function, fib-fast, 
The idea is as follows. First write a function fib-help 
that, for input n, computes the _list_ of Fibonacci 
numbers 0,1,1,2,3,5,8,... in _descending order from 
(fib n) down to (fib 0) = 0. See tests below, and also 
note the output contract, which is provided for you. 
Provide 3 more tests.

To minimize the number of recursive calls required to 
evaluate (fib-help n), you MUST use (let ...)  whenever 
you need the result of a recursive call several times. 
Your solution will be considered incorrect if your code 
contains several calls to fib-help with the same 
arguments (although you won't receive a 0). After all, 
the point here is EXACTLY to avoid  evaluating fib several 
times on the same argument.
What does (fib-help (- n 1)) return?
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; fib-help: nat -> lon
;; (fib-help n) takes a natural number n
;; and returns the list of Fibonacci numbers
;; from n to 0 (inclusive).
(defunc fib-help (n)
  .................

(check= (fib-help 1) '(1 0))
(check= (fib-help 2) '(1 1 0))
(check= (fib-help 3) '(2 1 1 0))

;; Design more tests.
..............


;; Now write a non-recursive function fib-fast, with 
;; contracts identical to the original fib function, 
;; which calls fib-help to compute the value.

(defunc fib-fast (n)
  .........

;; Test that fib-fast computes the same values as fib above!

(check= (fib-fast 1) (fib 1))
(check= (fib-fast 6) (fib 6))
(check= (fib-fast 8) (fib 8))

;; Design more tests.
.................


;; Now let's see whether fib-fast deserves that name: 
;; ... and try fib on input 40. If you think your machine 
;; can handle it, try 50 !
;; Also try fib-fast on these numbers and on 100. 
;; And on 200. And on 1000.
;;
;; What do you observe?
...................


;; Let's find out what's going on. Turn on tracing for the helper function
;; (fib-fast itself is not recursive, so no tracing needed):

(acl2::trace! fib-help)

#|

In the evaluation of (fib-fast  5), how many times is fib-help called on argument 2 ?
.............

In the evaluation of (fib-fast 10), how many times is fib-help called on argument 2 ? 
.............


Compare your results to those obtained with (fib n).
..........


|#
(acl2::untrace$ fib-help)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
D3. Fib*
Impressive! Can we push the envelop further?

fib-help was algorithmically very efficient, although it is not
tail-recursive. This means it will still use some non-trivial amount of
memory for large inputs.

Now write a tail-recursive version fib-t of fib (not of fib-fast). There
are several ways to do this. One is for fib-t to count DOWN and have two additional
arguments, a and b, such that in the ith recursive call, b contains fib(i)
and a contains fib(i-1). That means that after the nth iteration, the
result fib(n) will be stored in b. To maintain that invariant, think about
how to update a and b in each recursive call.

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; fib-t: nat x nat x nat -> nat
;; (fib-t n a b) takes three natural numbers and returns
;; the nth Fibonnacci number.
;; n is the current fib number index and a and b are
;; accumulators representing (fib (i-1)) and (fib i) respectively
;; If n_init is the initial value for n (and thus the index of the 
;; Fibonacci number we want to calculate), then i = (n_init - n).
(defunc fib-t (n a b)
  ...........

(check= (fib-t 1 0 1) 1)
(check= (fib-t 2 0 1) 1)
(check= (fib-t 3 0 1) 2)
(check= (fib-t 6 0 1) 8)
(check= (fib-t 8 0 1) 21)

(check= (fib-t 10 3 5) (fib 14))

;; Finally, write a non-recursive wrapper function fib* that has the same
;; signature as fib, and computes the same value as fib, but uses fib-t,
;; initializing the arguments a and b of fib-t appropriately:
...........


;; Test that fib* computes the same values as fib above!

(check= (fib* 1) (fib 1))
(check= (fib* 6) (fib 6))
;; Design more tests.
..............



;; Compare fib-fast and fib* ! Which one wins? It should be fib*, but you
;; need large arguments to see any difference! (Try n=5000 or more.)

(acl2::er-progn
   (acl2::time$ (acl2::value-triple (fib* .........)))
   (acl2::value-triple nil))

(acl2::er-progn
   (acl2::time$ (acl2::value-triple (fib-fast .......)))
   (acl2::value-triple nil))

.............

;; Make sure you understand why fib* is more efficient than fib-fast.

#| 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
D4. BACK TO PROOFS
Prove that fib* is equivalent to fib
  eps: (natp n) => ((fib* n) = (fib n))

A) Start your proof.  You will need a lemma which you will write in part B).
Feel free to use that lemma here to complete the proof.
.............


B) Devise a lemma L1 that relates fib-t and fib. The lemma should have the
form

L1: <hyp> => (fib-t n a b) = ...

where <hyp> are hypotheses necessary to prevent contract violations, and
... is an expression involving fib. The ... should not contain any call to
fib-t.

Hint: write the recursive calls to (fib-t 5 0 1) and see how it compares
to outputs to (fib n).
n=5: (fib-t 5 0 1) = 0 + (fib 5)*1
n=4: (fib-t 4 1 1) = ........  
n=3: (fib-t 3 .........
n=2: (fib-t 2 .........
n=1: (fib-t 1 .........
Do you see a pattern?  Make sure you include variables a and b in  your lemma.
Notice that (fib-t 4 1 1) = (fib-t 5 0 1)? The fib-t always returns the same
value, even when n decreases but (fib 4) does not equal (fib 5)

.............


C) Prove lemma L1 by induction.
...............

|#

#|

In conclusion, observe this: the efficiency difference between fib and
fib-fast is HUGE but the difference between fib-fast and fib* is
comparatively marginal:

fib* < fib-fast <<<< fib

where '<' means 'is faster than' , and '<<<<' means 'is much faster than' .

In other words, coming up with a smart algorithm to compute fib is the
match winner. Designing correct and elegant and efficient algorithms should
always be the first concern of a computer scientist.

Both fib-fast and fib* avoid the naive and repeated re-computation of (fib
i) for many i, so in that sense both are dynamic. What distinguishes them
at the end is the more efficient use of memory of fib* due to tail
recursion; as we have seen this adds some but not magnitudinal efficiency.

|#