;;****************
;;* MODULE MAIN  *
;;****************

(deffacts MAIN::control-information
(phase-sequence QUESTIONS QUESTION-INFERENCE INIT RESET RATE-RESORT RATE-HOTEL RATE-PATH RATE-DURATION)
)

;;(defrule MAIN::init
;;    (declare (salience 1000))
;;    (initial-fact)
;;=>
;;    (focus INIT)
;;)

(defrule MAIN::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
)
  

(defmodule COMMON (export deftemplate dv dv-numeric))

(deftemplate COMMON::dv 
   (multislot description)
   (multislot value)
   (slot CF (default 100.0))
   (slot basic (default FALSE))
)

(deftemplate COMMON::dv-numeric
   (multislot description)
   (multislot value (type INTEGER))
   (slot CF (default 100.0))
   (slot basic (default FALSE))
)
  
(defrule COMMON::combine-certainties-both-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description $?d) (value $?v) (CF ?C1&:(>= ?C1 0.0)))
    ?fact2 <- (dv (description $?d) (value $?v) (CF ?C2&:(>= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (- (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (CF ?C3))
)
    
(defrule COMMON::combine-certainties-both-negative
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description $?d) (value $?v) (CF ?C1&:(<= ?C1 0.0)))
    ?fact2 <- (dv (description $?d) (value $?v) (CF ?C2&:(<= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (+ (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (CF ?C3))
)

(defrule COMMON::combine-certainties-negative-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description $?d) (value $?v) (CF ?C1))
    ?fact2 <- (dv (description $?d) (value $?v) (CF ?C2))
    (test (neq ?fact1 ?fact2))
    (test (< (* ?C1 ?C2) 0.0))
=>
    (retract ?fact1)
    (bind ?C3 (/ (+ ?C1 ?C2) (- 1 (min (abs ?C1) (abs ?C2) ))))
    (modify ?fact2 (CF ?C3))
)


;;********************
;;* MODULE QUESTIONS *
;;********************

(defmodule QUESTIONS (export deftemplate preference))

(deftemplate QUESTIONS::preference
   (slot topic)
   (slot answer-value)
)

(deftemplate QUESTIONS::question
   (slot preference-topic (default ?NONE))
   (slot type (default closed))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE))
)
   

(deffacts QUESTIONS::questions-list
  (question (preference-topic temperature)
            (the-question "Posti caldi o freddi?")
            (valid-answers cool both warm))
  (question (preference-topic cost)
            (the-question "Viaggio economico o lussuoso?")
            (valid-answers cheap budget normal expensive very-expensive))
  (question (preference-topic culture)
            (the-question "Ti interessa la cultura?")
            (valid-answers no sometimes yes))
  (question (preference-topic people-num)
            (type range)
            (the-question "In quanti siete a viaggiare?")
            (valid-answers 1 10))
  (question (preference-topic max-distance)
            (type range)
            (the-question "Massima distanza? (compresa fra 25 e 100 km)")
            (valid-answers 25 100))
  (question (preference-topic duration)
            (type range)
            (the-question "Quanti giorni? (compreso fra 3 e 20")
            (valid-answers 3 20))
) 

;; *********the ask function********
(deffunction QUESTIONS::ask-closed-question (?question ?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer
)
   
;; *********the ask function********
(deffunction QUESTIONS::ask-range-question (?question ?min ?max)
   (printout t ?question)
   (bind ?answer (read))
   (while (or (not (numberp ?answer)) (< ?answer ?min) (> ?answer ?max)) do
      (printout t ?question)
      (bind ?answer (read)))
   ?answer
)

(defrule QUESTIONS::ask-a-closed-question
   ?fact <- (question (already-asked FALSE)
                   (type closed)
                   (the-question ?q)
                   (preference-topic ?pt)
                   (valid-answers $?va))
=>
   (modify ?fact (already-asked TRUE))
   (assert (preference (topic ?pt) (answer-value (ask-closed-question ?q ?va))))
)

(defrule QUESTIONS::ask-a-range-question
   ?fact <- (question (already-asked FALSE)
                   (type range)
                   (the-question ?q)
                   (preference-topic ?pt)
                   (valid-answers ?min ?max))
=>
   (modify ?fact (already-asked TRUE))
   (assert (preference (topic ?pt) (answer-value (ask-range-question ?q ?min ?max))))
)
   

;;*****************************
;;* MODULE QUESTION-INFERENCE *
;;*****************************
 

(defmodule QUESTION-INFERENCE (import COMMON ?ALL) (import QUESTIONS ?ALL))

(defrule QUESTION-INFERENCE::temperature-warm
    (preference (topic temperature) (answer-value warm))
=>
    (assert (dv (description tourism-type-is) (value sea) (CF 0.4) (basic TRUE)))
    (assert (dv (description tourism-type-is) (value mountain) (CF -0.4) (basic TRUE))) 
)
    
(defrule QUESTION-INFERENCE::temperature-cool
    (preference (topic temperature) (answer-value cool))
=>
    (assert (dv (description tourism-type-is) (value mountain) (CF 0.4) (basic TRUE)))
    (assert (dv (description tourism-type-is) (value sea) (CF -0.4) (basic TRUE))) 
)

    
(defrule QUESTION-INFERENCE::culture-yes
    (preference (topic culture) (answer-value yes))
=>
    (assert (dv (description tourism-type-is) (value cultural) (CF 0.8) (basic TRUE)))
    (assert (dv (description tourism-type-is) (value religious) (CF 0.5) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::culture-no
    (preference (topic culture) (answer-value no))
=>
    (assert (dv (description tourism-type-is) (value cultural) (CF -0.8) (basic TRUE))))
    (assert (dv (description tourism-type-is) (value religious) (CF -0.5) (basic TRUE)))) 
)

(defrule QUESTION-INFERENCE::cost-cheap
    (preference (topic cost) (answer-value cheap))
=>
    (assert (dv-numeric (description how-many-hotel-stars) (value 1) (CF 0.6) (basic TRUE)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 2) (CF 0.2) (basic TRUE))) 
    (assert (dv-numeric (description how-many-hotel-stars) (value 3) (CF -0.2) (basic TRUE)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 4) (CF .-0.6) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-normal
    (preference (topic cost) (answer-value normal))
=>
    (assert (dv-numeric (description how-many-hotel-stars) (value 1) (CF -0.2) (basic TRUE)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 2) (CF 0.6) (basic TRUE))) 
    (assert (dv-numeric (description how-many-hotel-stars) (value 3) (CF 0.6) (basic TRUE)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 4) (CF -0.2) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-expensive
    (preference (topic cost) (answer-value expensive))
=>
    (assert (dv-numeric (description how-many-hotel-stars) (value 1) (CF -0.6) (basic TRUE)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 2) (CF -0.2) (basic TRUE))) 
    (assert (dv-numeric (description how-many-hotel-stars) (value 3) (CF 0.2) (basic TRUE)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 4) (CF 0.6) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::people-number
    (preference (topic people-num) (answer-value ?v))
=>
    (assert (dv-numeric (description how-many-people) (value ?v) (CF 1.0) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::max-distance
    (preference (topic max-distance) (answer-value ?v))
=>
    (assert (dv-numeric (description the-max-distance-is) (value ?v) (CF 1.0) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::duration
    (preference (topic duration) (answer-value ?v))
=>
    (assert (dv-numeric (description trip-duration) (value ?v) (CF 1.0) (basic TRUE)))
)


(defrule QUESTION-INFERENCE::reassert-basic-dv
    (declare (salience (-100))
    ?fact <- (dv (description $?d) (value $?v) (CF ?c) (basic TRUE))
=>  
    (retract ?fact)
    (assert (dv (description $?d) (value $?v) (CF ?c) (basic TRUE)))
)


(defrule QUESTION-INFERENCE::remove-derived-dv
    (declare (salience (-100))
    ?fact <- (dv (description $?d) (value $?v) (CF ?c) (basic FALSE))
=>  
    (retract ?fact)
)


;;*****************
;;* MODULE RESORT *
;;*****************

(defmodule RESORT (export ?ALL))

(deftemplate RESORT::resort
  (slot name  (default ?NONE))
  (slot region (default ?NONE))
)
  
(deftemplate RESORT::resort-tourism
  (slot resort-name  (default ?NONE))
  (slot tourism-type (default ?NONE))
  (slot score (type INTEGER) (range 0 5))) 
  

(deftemplate RESORT::route
  (slot resort-src (default ?NONE))
  (slot resort-dst (default ?NONE))
  (slot distance (type INTEGER) (range 1 ?VARIABLE))
) 

(deffacts RESORT::resort-list 
  (resort (name BiancaVilla) (region Kanto))
  (resort (name Lavandonia) (region Kanto))
  (resort (name MonteFatuo) (region Kanto))
)

;;;;; FACTS ;;;;;;
  
(deffacts RESORT::resort-tourism-list 
  (resort-tourism (resort-name BiancaVilla) (tourism-type sea) (score 4))
  (resort-tourism (resort-name BiancaVilla) (tourism-type mountain) (score 1))
  (resort-tourism (resort-name BiancaVilla) (tourism-type cultural) (score 3))
  (resort-tourism (resort-name Lavandonia) (tourism-type sea) (score 3))
  (resort-tourism (resort-name Lavandonia) (tourism-type religious) (score 3))
  (resort-tourism (resort-name MonteFatuo) (tourism-type cultural) (score 3))
  (resort-tourism (resort-name MonteFatuo) (tourism-type mountain) (score 4))
)

(deffacts RESORT:route-list
    (route (resort-src BiancaVilla) (resort-dst MonteFatuo) (distance 30))
    (route (resort-src MonteFatuo) (resort-dst Lavandonia) (distance 10))
    (route (resort-src Lavandonia) (resort-dst BiancaVilla) (distance 20))
)

;;*****************
;;* MODULE HOTEL  *
;;*****************

(defmodule HOTEL (export ?ALL))

(deftemplate HOTEL::hotel
  (slot name (default ?NONE))
  (slot resort)
  (slot stars (type INTEGER) (range 1 4))
  (slot empty (type INTEGER) (range 0 ?VARIABLE))
  (slot capacity (type INTEGER) (range 0 ?VARIABLE))
)
  

;;;;;;;; FACTS ;;;;;;;;;;;;

(deffacts HOTEL::hotel-list
  (hotel (name YesHotel) (resort BiancaVilla) (stars 3) (empty 8) (capacity 20))
  (hotel (name BhaHotel) (resort BiancaVilla) (stars 1) (empty 12) (capacity 40))
  (hotel (name MammaHotel) (resort Lavandonia) (stars 4) (empty 7) (capacity 40))
  (hotel (name BubbaHotel) (resort Lavandonia) (stars 3) (empty 10) (capacity 20))
  (hotel (name FuocoHotel) (resort MonteFatuo) (stars 3) (empty 15) (capacity 25))
  (hotel (name MerdaHotel) (resort MonteFatuo) (stars 1) (empty 20) (capacity 30))
)


;;*****************
;;* MODULE PATH *
;;*****************
 
(defmodule PATH (export ?ALL))

(deftemplate PATH::path
    (multislot resorts-list)
    (slot length (type INTEGER))
    (slot total-distance (type INTEGER))
)


;;*****************
;;* MODULE TRIP   *
;;*****************

(defmodule TRIP (export ?ALL))

(deftemplate TRIP::duration
    (multislot days (type INTEGER))
    (slot length (type INTEGER))
)

(deftemplate TRIP::trip
    (slot id (default-dynamic (gensym*)))
    (multislot resorts-list)
    (multislot hotels-list (default NULL NULL NULL NULL NULL))
    (multislot days (type INTEGER))
    (multislot costs (type INTEGER) (default 0 0 0 0 0))
)

  
;;****************
;;* MODULE INIT  *
;;****************

(defmodule INIT  (import COMMON ?ALL) (import RESORT ?ALL) (import HOTEL ?ALL) (import PATH ?ALL) (import TRIP ?ALL))  


(defrule INIT::check-already-done-init
    (declare (salience 10000))
    (already-done-init)
=>
    (pop-focus)
)

(defrule INIT::assert-already-done-init
    (declare (salience -10000))
    (not (already-done-init))
=>
    (assert (already-done-init))
)

(defrule INIT::build-symmetric-route
    (route (resort-src ?r1) (resort-dst ?r2) (distance ?d))
    (not (route (resort-src ?r2) (resort-dst ?r1) (distance ?d)))
=>
    (assert (route (resort-src ?r2) (resort-dst ?r1) (distance ?d)))
)
  

(defrule INIT::build-singleton-path
    (resort (name ?r))
=>
    (assert (path (resorts-list ?r) (length 1) (total-distance 0)))
)

(defrule INIT::build-path
    (path (resorts-list $?rs ?lr) (length ?len) (total-distance ?td))
    (test (< ?len 5))
    (route (resort-src ?lr) (resort-dst ?nr) (distance ?d)) 
    (test (eq (member$ ?nr (create$ ?rs ?lr)) FALSE))
=>
    (assert (path (resorts-list ?rs ?lr ?nr) (length (+ ?len 1)) (total-distance (+ ?td ?d))))
)


(defrule INIT::remove-suboptimal-distance-path
    ?p1 <- (path (resorts-list $?rs1) (total-distance ?d1))
    ?p2 <- (path (resorts-list $?rs2) (total-distance ?d2))
    (test (and (neq ?p1 ?p2) (<= ?d1 ?d2)))
    (test (subsetp ?rs1 ?rs2))
    (test (subsetp ?rs2 ?rs1))
=>
    (retract ?p2)
)


(defrule INIT::define-duration-unit
    (dv-numeric (description trip-duration) (value ?d))
=>
    (assert (duration-unit (max 1 (div ?d 10))))
)


(defrule INIT::generate-basic-duration
    (dv-numeric (description trip-duration) (value ?d))
=>
    (assert (duration (days ?d) (length 1)))  
)

(defrule INIT::generate-longer-duration
    (duration-unit ?u)
    (duration (days $?ds ?d) (length ?len))
    (test (< (length$ (create$ ?ds ?d)) 5))
    (test (> ?d ?u))
=>
    (assert (duration (days ?ds (- ?d ?u) ?u) (length (+ ?len 1))))
)

(defrule INIT::permutate-duration
    (duration-unit ?u)
    (duration (days $?dl ?d1 ?d2 $?dr) (length ?len))
    (test (> ?d1 ?u))
=>
    (assert (duration (days ?dl (- ?d1 ?u) (+ ?d2 ?u) ?dr) (length ?len)))  
)


;;**********************
;;* MODULE RATE-RESORT *
;;**********************
  
(defmodule RATE-RESORT (import COMMON ?ALL) (import RESORT ?ALL))
  
(defrule RATE-RESORT::rate-resort-uncertain
    (resort (name ?r))
=>
    (assert (dv (description visit-resort) (value ?r) (CF  0.0)))
)

(defrule RATE-RESORT::rate-resort-by-tourism-type
    (dv (description tourism-type-is) (value ?v) (CF ?cf))
    (resort (name ?rn))
    (resort-tourism (resort-name ?rn) (tourism-type ?v) (score ?s))
=>
    (bind ?new-cf (/ (* ?s ?cf) 5.0))
    (assert (dv (description visit-resort) (value ?rn) (CF ?new-cf)))
)

(defrule RATE-RESORT::rate-route
    (route (resort-src ?src) (resort-dst ?dst) (distance ?d))
    (dv-numeric (description the-max-distance-is) (value ?v))
=>
    (bind ?new-cf (min 0.6 (max -0.8 (/ (- ?v ?d) 50.0))))
    (assert (dv (description use-route) (value ?src ?dst) (CF ?new-cf))) 
)


;;*********************
;;* MODULE RATE-HOTEL *
;;*********************

(defmodule RATE-HOTEL (import COMMON ?ALL) (import HOTEL ?ALL))

(defrule RATE-HOTEL::rate-hotel-uncertain
    (hotel (name ?h) (resort ?r))
=>
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF  0.0)))
)

(defrule RATE-HOTEL::rate-hotel-by-stars
    (dv-numeric (description how-many-hotel-stars) (value ?s) (CF ?cf))
    (hotel (name ?hn) (resort ?r) (stars ?hs))
=>
    (bind ?new-cf (- 0.6 (* (abs (- ?hs ?s)) 0.4)))
    (assert (dv (description the-hotel-in ?r) (value ?hn) (CF ?new-cf)))
)

(defrule RATE-HOTEL::rate-hotel-by-availability
    (dv-numeric (description how-many-people) (value ?p))
    (hotel (name ?hn) (resort ?r) (empty ?a&:(> ?a ?p)) (capacity ?c))
=>
    (bind ?new-cf (* 0.7 (/ ?a ?c)))
    (assert (dv (description the-hotel-in ?r) (value ?hn) (CF ?new-cf)))
)

(defrule RATE-HOTEL::rate-hotel-by-availability-full
    (dv-numeric (description how-many-people) (value ?p))
    (hotel (name ?hn) (resort ?r) (empty ?a&:(< ?a ?p)))
=>
    (assert (dv (description the-hotel-in ?r) (value ?hn) (CF -1.0)))
)


;;********************
;;* MODULE RATE-PATH *
;;********************

(defmodule RATE-PATH (import COMMON ?ALL) (import PATH ?ALL))

(defrule RATE-PATH::rate-path-by-resorts
    (path (resorts-list $?rl ?r $?rr) (length ?len))
    (dv (description visit-resort) (value ?r) (CF ?rcf))
=> 
    (bind ?pcf (/ ?rcf ?len))
    (assert (dv (description the-path-is) (value ?rl ?r ?rr) (CF ?pcf)))
)


(defrule RATE-PATH::rate-path-by-routes
    (path (resorts-list $?rl ?rs ?rd $?rr) (length ?len))
    (dv (description use-route) (value ?rs ?rd) (CF ?rcf))
=>  
    (bind ?pcf (/ ?rcf ?len))
    (assert (dv (description the-path-is) (value ?rl ?rs ?rd ?rr) (CF ?pcf)))
)

(defrule RATE-PATH::rate-path-by-hotels
    (path (resorts-list $?rl ?r $?rr) (length ?len))
    (dv (description the-hotel-in ?r) (value ?h) (CF ?hcf))
    (not (dv (description the-hotel-in ?r) (value ?h2&~h) (CF ?hcf2&:(> ?hcf2 ?hcf)))) 
=> 
    (bind ?pcf (/ ?hcf ?len))
    (assert (dv (description the-path-is) (value ?rl ?r ?rr) (CF ?pcf)))
)

(defrule RATE-PATH::rate-path-by-length
    (path (resorts-list $?rl ?r $?rr) (length ?len))
    (dv-numeric (description the-path-length) (value ?pl))
=> 
    (bind ?pcf (- 0.7 (* (abs (- ?pl ?len)) 0.4)))
    (assert (dv (description the-path-is) (value ?rl ?r ?rr) (CF ?pcf)))
)


;;************************
;;* MODULE RATE-DURATION *
;;************************

(defmodule RATE-DURATION (import COMMON ?ALL) (import RESORT ?ALL) (import HOTEL ?ALL) (import PATH ?ALL) (import TRIP ?ALL))

  
(defrule RATE-DURATION::rate-duration
    (path (resorts-list $?rl ?r $?rr) (length ?len))
    (duration (days $?ds) (length ?len))
    (dv (description visit-resort) (value ?r) (CF ?rcf))
    (dv (description the-hotel-in ?r) (value ?h) (CF ?hcf))
    (not (dv (description the-resort in ?r) (value ?h2&~h) (CF ?hcf2&:(> ?hcf2 ?hcf))))
    (dv-numeric (description trip-duration) (value ?td))
=>
    (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?d (nth ?index ?ds))
    (bind ?min-cf (min ?rcf ?hcf))
    (assert (dv (description the-path-days-for ?rl ?r ?rr) (value ?ds) (CF (* ?min-cf (/ ?d ?td)))))
) 

    
(defrule RATE-DURATION::plsstop
    (declare (salience -500))
=>
    (halt)
    (refresh refresh-test)
) 
  
defmodule PRINT-RESULTS

(defrule


 

  

  
