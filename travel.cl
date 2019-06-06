;;****************
;;* MODULE MAIN  *
;;****************

(deffacts MAIN::define-phase-sequence
(phase-sequence QUESTION QUESTION-INFERENCE INIT REFRESH RATE-RESORT RATE-HOTEL BUILD-AND-RATE-TRIP PRINT-RESULTS)
)

(defrule MAIN::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
)

;;******************
;;* MODULE COMMON  *
;;******************

(defmodule COMMON (export ?ALL))

(deftemplate COMMON::dv-iteration
    (slot number (type INTEGER))
)

(deftemplate COMMON::dv 
    (slot iteration (type INTEGER) (default -1)) ;;An iteration value of -1 is for non-basic dv only
    (multislot description)
    (multislot value)
    (slot CF (default 100.0))
    (slot basic (default FALSE))  ;;A basic dv must be reasserted at every iteration. A non-basic one must be removed
)

(deffacts COMMON::first-dv-iteration
    (dv-iteration (number 0))
)

;;;;;;;;; COMBINE CERTAINTIES ;;;;;;;;
  
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
;;* MODULE QUESTION *
;;********************

(defmodule QUESTION (export deftemplate preference))

(deftemplate QUESTION::preference
   (slot topic)
   (slot answer-value)
)

(deftemplate QUESTION::question
   (slot preference-topic (default ?NONE))
   (slot type (default closed))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE))
)
   

(deffacts QUESTION::questions-list
  (question (preference-topic temperature)
            (the-question "Posti caldi o freddi?")
            (valid-answers cool both warm))
  (question (preference-topic cost)
            (the-question "Viaggio economico o lussuoso?")
            (valid-answers cheap budget normal expensive very-expensive))
  (question (preference-topic culture)
            (the-question "Ti interessa la cultura?")
            (valid-answers no sometimes yes))
  (question (preference-topic people-number)
            (type range)
            (the-question "In quanti siete a viaggiare?")
            (valid-answers 1 10))
  (question (preference-topic max-distance)
            (type range)
            (the-question "Massima distanza? (compresa fra 25 e 100 km)")
            (valid-answers 25 100))
  (question (preference-topic trip-duration)
            (type range)
            (the-question "Quanti giorni? (compreso fra 3 e 20")
            (valid-answers 3 20))
) 

;; *********the ask function********
(deffunction QUESTION::ask-closed-question (?question ?allowed-values)
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
(deffunction QUESTION::ask-range-question (?question ?min ?max)
   (printout t ?question)
   (bind ?answer (read))
   (while (or (not (numberp ?answer)) (< ?answer ?min) (> ?answer ?max)) do
      (printout t ?question)
      (bind ?answer (read)))
   ?answer
)

(defrule QUESTION::ask-a-closed-question
   ?fact <- (question (already-asked FALSE)
                   (type closed)
                   (the-question ?q)
                   (preference-topic ?pt)
                   (valid-answers $?va))
=>
   (modify ?fact (already-asked TRUE))
   (assert (preference (topic ?pt) (answer-value (ask-closed-question ?q ?va))))
)

(defrule QUESTION::ask-a-range-question
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
 

(defmodule QUESTION-INFERENCE (import COMMON ?ALL) (import QUESTION ?ALL))

(defrule QUESTION-INFERENCE::temperature-warm
    (preference (topic temperature) (answer-value warm))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-tourism-type) (value sea) (CF 0.4) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-tourism-type) (value mountain) (CF -0.4) (basic TRUE))) 
)
    
(defrule QUESTION-INFERENCE::temperature-cool
    (preference (topic temperature) (answer-value cool))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-tourism-type) (value mountain) (CF 0.4) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-tourism-type) (value sea) (CF -0.4) (basic TRUE))) 
)

    
(defrule QUESTION-INFERENCE::culture-yes
    (preference (topic culture) (answer-value yes))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-tourism-type) (value cultural) (CF 0.8) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-tourism-type) (value religious) (CF 0.5) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::culture-no
    (preference (topic culture) (answer-value no))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-tourism-type) (value cultural) (CF -0.8) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-tourism-type) (value religious) (CF -0.5) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::cost-cheap
    (preference (topic cost) (answer-value cheap))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 1) (CF 0.6) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 2) (CF 0.2) (basic TRUE))) 
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 3) (CF -0.2) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 4) (CF -0.6) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-normal
    (preference (topic cost) (answer-value normal))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 1) (CF -0.2) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 2) (CF 0.6) (basic TRUE))) 
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 3) (CF 0.6) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 4) (CF -0.2) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-expensive
    (preference (topic cost) (answer-value expensive))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 1) (CF -0.6) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 2) (CF -0.2) (basic TRUE))) 
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 3) (CF 0.2) (basic TRUE)))
    (assert (dv (iteration ?i) (description the-optimal-hotel-stars) (value 4) (CF 0.6) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::people-number
    (preference (topic people-number) (answer-value ?v))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-people-number) (value ?v) (CF 1.0) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::max-distance
    (preference (topic max-distance) (answer-value ?v))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-max-route-distance) (value ?v) (CF 1.0) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::trip-duration
    (preference (topic trip-duration) (answer-value ?v))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-trip-duration) (value ?v) (CF 1.0) (basic TRUE)))
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
    (slot score (type INTEGER) (range 0 5))
) 
  
(deftemplate RESORT::route
    (slot resort-src (default ?NONE))
    (slot resort-dst (default ?NONE))
    (slot distance (type INTEGER) (range 1 ?VARIABLE))
) 

;;;;;;;;;; FACTS ;;;;;;;;;;;;;

(deffacts RESORT::resort-list 
    (resort (name BiancaVilla) (region Kanto))
    (resort (name Lavandonia) (region Kanto))
    (resort (name MonteFatuo) (region Kanto))
)
  
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
  
;;;;;;;;;;; FACTS ;;;;;;;;;;;;

(deffacts HOTEL::hotel-list
    (hotel (name YesHotel) (resort BiancaVilla) (stars 3) (empty 8) (capacity 20))
    (hotel (name BhaHotel) (resort BiancaVilla) (stars 1) (empty 12) (capacity 40))
    (hotel (name MammaHotel) (resort Lavandonia) (stars 4) (empty 7) (capacity 40))
    (hotel (name BubbaHotel) (resort Lavandonia) (stars 3) (empty 10) (capacity 20))
    (hotel (name FuocoHotel) (resort MonteFatuo) (stars 3) (empty 15) (capacity 25))
    (hotel (name MerdaHotel) (resort MonteFatuo) (stars 1) (empty 20) (capacity 30))
)


;;*****************
;;* MODULE TRIP   *
;;*****************

(defmodule TRIP (export ?ALL))

(deftemplate TRIP::path
    (multislot resorts)
    (slot length (type INTEGER))
    (slot total-distance (type INTEGER))
)

(deftemplate TRIP::duration
    (multislot days (type INTEGER))
    (slot length (type INTEGER))
)

(deftemplate TRIP::trip
    (slot trip-id (default-dynamic (gensym*)))
    (multislot resorts)
    (multislot hotels (default NULL NULL NULL NULL NULL))
    (multislot days (type INTEGER))
    (multislot costs (type INTEGER) (default 0 0 0 0 0))
    (slot length (type INTEGER))
)

  
;;****************
;;* MODULE INIT  *
;;****************

(defmodule INIT  (import COMMON ?ALL) (import RESORT ?ALL) (import HOTEL ?ALL) (import TRIP ?ALL))  


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
    (assert (path (resorts ?r) (length 1) (total-distance 0)))
)

(defrule INIT::build-path
    (path (resorts $?rs ?lr) (length ?len) (total-distance ?td))
    (test (< ?len 5))
    (route (resort-src ?lr) (resort-dst ?nr) (distance ?d)) 
    (test (eq (member$ ?nr (create$ ?rs ?lr)) FALSE))
=>
    (assert (path (resorts ?rs ?lr ?nr) (length (+ ?len 1)) (total-distance (+ ?td ?d))))
)


(defrule INIT::remove-suboptimal-distance-path
    ?p1 <- (path (resorts $?rs1) (total-distance ?d1))
    ?p2 <- (path (resorts $?rs2) (total-distance ?d2))
    (test (and (neq ?p1 ?p2) (<= ?d1 ?d2)))
    (test (subsetp ?rs1 ?rs2))
    (test (subsetp ?rs2 ?rs1))
=>
    (retract ?p2)
)


(defrule INIT::define-duration-unit
    (dv (description the-trip-duration) (value ?d))
    (dv-iteration (number ?i))
=>
    (assert (dv (iteration ?i) (description the-duration-unit) (value (max 1 (div ?d 7))) (CF 1.0) (basic TRUE)))
)


(defrule INIT::generate-singleton-duration
    (dv (description the-trip-duration) (value ?d))
=>
    (assert (duration (days ?d) (length 1)))  
)

(defrule INIT::generate-duration
    (dv (description the-duration-unit) (value ?u))
    (duration (days $?ds ?d) (length ?len))
    (test (< (length$ (create$ ?ds ?d)) 5))
    (test (> ?d ?u))
=>
    (assert (duration (days ?ds (- ?d ?u) ?u) (length (+ ?len 1))))
)

(defrule INIT::permutate-duration
    (dv (description the-duration-unit) (value ?u))
    (duration (days $?dl ?d1 ?d2 $?dr) (length ?len))
    (test (> ?d1 ?u))
=>
    (assert (duration (days ?dl (- ?d1 ?u) (+ ?d2 ?u) ?dr) (length ?len)))  
)


;;**********************
;;* MODULE REFRESH *
;;**********************

(defmodule REFRESH (import COMMON ?ALL) (import TRIP ?ALL))


(defrule REFRESH::reassert-basic-dv
    (dv-iteration (number ?i))
    ?fact <- (dv (iteration ?i) (description $?d) (value $?v) (CF ?c) (basic TRUE))
=>  
    (retract ?fact)
    (assert (dv (iteration (+ ?i 1)) (description ?d) (value ?v) (CF ?c) (basic TRUE)))
)

(defrule REFRESH::remove-derived-dv
    ?fact <- (dv (description $?d) (value $?v) (CF ?c) (basic FALSE))
=>  
    (retract ?fact)
)

(defrule REFRESH::remove-trip
    ?t <- (trip)
=>
    (retract ?t)
)

(defrule REFRESH::on-exit
    (declare (salience -1000))
    ?fact <- (dv-iteration (number ?i))
=>
    (retract ?fact)
    (assert (dv-iteration (number (+ ?i 1))))
    (pop-focus)
)


;;**********************
;;* MODULE RATE-RESORT *
;;**********************
  
(defmodule RATE-RESORT (import COMMON ?ALL) (import RESORT ?ALL))
  
(defrule RATE-RESORT::rate-resort-uncertain
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF  0.0)))
)

(defrule RATE-RESORT::rate-resort-by-tourism-type
    (dv (description the-tourism-type) (value ?t) (CF ?cf))
    (resort (name ?r))
    (resort-tourism (resort-name ?r) (tourism-type ?t) (score ?s))
=>
    (bind ?rcf (/ (* ?s ?cf) 5.0))
    (assert (dv (description the-resort) (value ?r) (CF ?rcf)))
)

(defrule RATE-RESORT::rate-route
    (route (resort-src ?src) (resort-dst ?dst) (distance ?d))
    (dv (description the-max-route-distance) (value ?v))
=>
    (bind ?rcf (min 0.7 (max -0.9 (/ (- ?v ?d) 75.0))))   ;; every 75 km we drecrease by 1 (capped at -0.9)
    (assert (dv (description use-route) (value ?src ?dst) (CF ?rcf))) 
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
    (dv (description the-optimal-hotel-stars) (value ?s) (CF ?cf))
    (hotel (name ?h) (resort ?r) (stars ?s))
=>
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF ?cf)))
)

(defrule RATE-HOTEL::rate-hotel-by-availability
    (dv (description the-people-number) (value ?p))
    (hotel (name ?h) (resort ?r) (empty ?e&:(> ?e ?p)) (capacity ?c))
=>
    (bind ?new-cf (* 0.7 (/ ?e ?c)))
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF ?new-cf)))
)

(defrule RATE-HOTEL::rate-hotel-by-availability-full
    (dv (description the-people-number) (value ?p))
    (hotel (name ?h) (resort ?r) (empty ?e&:(< ?e ?p)))
=>
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF -1.0)))
)

;;******************************
;;* MODULE BUILD-AND-RATE-TRIP *
;;******************************

(defmodule BUILD-AND-RATE-TRIP (import COMMON ?ALL) (import HOTEL ?ALL) (import TRIP ?ALL))

;;;;;; ON-ENTER AND ON-EXIT ;;;;;;;;

(defrule BUILD-AND-RATE-TRIP::on-enter
    (declare (salience 1000))
    (not (must-build-trip))
=>
    (assert (must-build-trip))
) 

(defrule BUILD-AND-RATE-TRIP::on-exit
    (declare (salience -1000))
    ?fact <- (must-build-trip)
=>
    (retract ?fact)
    (pop-focus)
) 

;;;;;;;;; RULES FOR BUILDING TRIPS ;;;;;;;;

(defrule BUILD-AND-RATE-TRIP::build-trip
    (declare (salience 600))
    (must-build-trip)
    (path (resorts $?rs) (length ?len))
    (duration (days $?ds) (length ?len))
=>
    (assert (trip (resorts ?rs) (days ?ds) (length ?len)))
)

(defrule BUILD-AND-RATE-TRIP::fill-trip-hotels-and-costs
    (declare (salience 500))
    ?t <- (trip (resorts $?rl ?r $?rr) (hotels $?hs) (days $?ds) (costs $?cs))
    (test (eq (nth (member$ ?r (create$ ?rl ?r ?rr)) ?cs) 0))
    (dv (description the-hotel-in ?r) (value ?h) (CF ?hcf))
    (not (dv (description the-hotel-in ?r) (value ?h2&~?h) (CF ?hcf2&:(> ?hcf2 ?hcf))))
    (hotel (name ?h) (resort ?r) (stars ?s))
    (dv (description the-people-number) (value ?p))
=>
    (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?daily-cost (+ 50 (* ?s 25)))
    (bind ?cost-all-people (* (div ?p 2) ?daily-cost))
    (bind ?cost-all-days (* (nth ?index ?ds) ?cost-all-people))
    (modify ?t (hotels (replace$ ?hs ?index ?index ?h)) (costs (replace$ ?cs ?index ?index ?cost-all-days)))
)

;;;;;;;;; RULES FOR RATING TRIPS ;;;;;;;;;;


(defrule BUILD-AND-RATE-TRIP::rate-trip-by-resorts
    (trip (trip-id ?id) (resorts $?rl ?r $?rr) (days $?ds) (length ?len))
    (dv (description the-resort) (value ?r) (CF ?rcf))
    (dv (description the-trip-duration) (value ?td))
=> 
    (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?d (nth ?index ?ds))
    (bind ?tcf (/ (* ?d ?rcf) (* ?td ?len)))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-routes
    (trip (trip-id ?id) (resorts $?rl ?rs ?rd $?rr) (length ?len))
    (dv (description use-route) (value ?rs ?rd) (CF ?rcf))
=>  
    (bind ?tcf (/ ?rcf (- ?len 1)))       ;;len resorts imply len-1 routes
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-hotels 
    (trip (trip-id ?id) (resorts $?rl ?r $?rr) (hotels $?hs) (days $?ds) (length ?len))
    (dv (description the-hotel-in ?r) (value ?h) (CF ?hcf))
    (test (eq ?h (nth (member$ ?r (create$ ?rl ?r ?rr)) ?hs))) 
    (dv (description the-trip-duration) (value ?td))
=>  (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?d (nth ?index ?ds))
    (bind ?tcf (/ (* ?d ?hcf) (* ?td ?len)))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-length
    (trip (trip-id ?id) (length ?len))
    (dv (description the-trip-length) (value ?tl))
=> 
    (bind ?tcf (- 0.7 (* (abs (- ?tl ?len)) 0.4)))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-total-cost
    (trip (trip-id ?id) (costs $?cs))
    (dv (description the-max-cost) (value ?mc))
=>
    (bind ?total-cost (+ (expand$ ?cs) 0))
    (bind ?tcf (min 0.7 (max -0.9 (/ (- ?total-cost ?mc) 100.0))))    ;;every 100 euros we decrease by 0.1
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)


;;(defrule BUILD-AND-RATE-TRIP::rate-trip-by-start-resort
;;)
;;
;;(defrule BUILD-AND-RATE-TRIP::rate-trip-by-end-resort
;;)


;;************************
;;* MODULE PRINT-RESULTS *
;;************************
  
(defmodule PRINT-RESULTS (import TRIP ?ALL))

(defrule PRINT-RESULTS::plsstop
    (declare (salience -1000))
=>
    (halt)
) 

 

  

  
