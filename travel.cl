


;;****************
;;* MODULE MAIN  *
;;****************

(deffacts MAIN::define-phase-sequence
(phase-sequence ASK-QUESTION QUESTION-INFERENCE INIT RATE-RESORT RATE-HOTEL BUILD-AND-RATE-TRIP PRINT-RESULTS INVALIDATE REFRESH)
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

(defglobal 
    ?*MAX-TOURISM-SCORE* = 5
    ?*MAX-TRIP-LENGTH* = 5
    ?*HOTEL-BASE-COST* = 50
    ?*HOTEL-ADDITIONAL-COST* = 25
    ?*MAX-ROUTE-DISTANCE-TOLERANCE* = 75
    ?*MAX-BUDGET-TOLERANCE* = 500
    ?*MIN-PRINT-CF* = 0.35
    ?*DURATION-UNIT-RATE* = 7
)

(deftemplate COMMON::iteration
    (slot number (type INTEGER))
)

(deftemplate COMMON::dv 
    (multislot description)
    (multislot value)
    (slot CF (default 1.0) (range -1.0 1.0))
    (slot basic (default FALSE))   ;;A basic dv must be reasserted at every iteration. A non-basic one must be removed
    (slot updated (default TRUE))  ;;Every dv is considered updated at creation; it is needed in phases INVALIDATE and REFRESH
)

(deffacts COMMON::first-iteration
    (iteration (number 0))
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

(defmodule QUESTION (export ?ALL))

(deftemplate QUESTION::preference
    (slot topic)
    (slot answer-value)
)

(deftemplate QUESTION::question
    (slot iteration (type INTEGER))  ;;the first iteration in which we are allowed to ask the question
    (slot type (default closed))
    (slot skippable (default TRUE))
    (slot preference-topic (default ?NONE))
    (slot the-question (default ?NONE))
    (multislot valid-answers)
    (multislot precursors)
    (slot always-repeat (default FALSE))
    (slot never-repeat (default FALSE))
)


;;;;;;;;;;;;;;; FACTS ;;;;;;;;;;;;;;;;;;

(deffacts QUESTION::questions-list
    (question (the-question "How many people want to go on vacation? (between 1 and 10 people) ") 
            (preference-topic people-number)
            (iteration 0) 
            (skippable FALSE)
            (type range)
            (valid-answers 1 10))
    (question (the-question "How many days? (between 5 and 30 days) ")
            (preference-topic trip-duration)
            (iteration 0)
            (skippable FALSE)
            (type range)
            (valid-answers 5 30))
    (question (the-question "Are you looking for a cheap trip or a luxurious and more expensive one? [cheap, normal, expensive] ")
            (preference-topic cost)
            (iteration 0) 
            (valid-answers cheap normal expensive))
    (question (the-question "Do you generally prefer cool or warm places? [cool, both, warm] ")            
            (preference-topic temperature)
            (iteration 0) 
            (valid-answers cool both warm))
    ;;--------------------------------------------
    (question (the-question "Would you like to visit more than one resort? [yes, no] ")
            (preference-topic trip-length-generic)
            (iteration 1) 
            (skippable FALSE)
            (valid-answers yes no))
    (question (the-question "How many places would you like to visit during your vacation? (between 2 and 5 resorts) ")
            (preference-topic trip-length)
            (iteration 1)
            (type range)
            (skippable FALSE)
            (precursors trip-length-generic is yes)
            (valid-answers 1 5))
    (question (the-question "What is the maximum distance that you are willing to travel between two resorts? (between 10 ad 100 km) ")
            (preference-topic max-distance)
            (iteration 1) 
            (type range)
            (precursors trip-length-generic is yes)
            (valid-answers 10 100))
    (question (the-question "Do you prefer to spend an equal amount of time in all the places you will visit? [yes, no] ")
            (preference-topic days-partitioning)
            (iteration 1) 
            (precursors trip-length-generic is yes)
            (valid-answers yes no))
    (question (the-question "Please insert the resort from which you want to start your trip, if any. Leave blank for no preference.")
            (preference-topic start-resort)
            (iteration 1) 
            (type open)
            (never-repeat TRUE)
            (precursors trip-length-generic is yes))
    (question (the-question "Please insert the resort where you want to end your trip, if any. Leave blank for no preference.")
            (preference-topic end-resort)
            (iteration 1) 
            (type open)
            (never-repeat TRUE)
            (precursors trip-length-generic is yes))
    ;;----------------------------------------------
    (question (the-question "Do you have a precise budget limit? [yes, no] ")
            (preference-topic budget-limit-generic)
            (iteration 2) 
            (valid-answers yes no))
    (question (the-question "Please insert your budget limit. [between 100 and 999999 euros] ")
            (preference-topic budget-limit)
            (skippable FALSE)
            (iteration 2) 
            (type range)
            (precursors budget-limit-generic is yes)
            (valid-answers 100 999999))
    (question (the-question "How much it is important for you to eat at good places? (between 1 and 5) ")
            (preference-topic food)
            (never-repeat TRUE)
            (iteration 2) 
            (type range)
            (valid-answers 1 5))
    (question (the-question "How much are you interested in places that are relevant from a religious point of view? (between 1 and 5) ")
            (preference-topic religion)
            (iteration 2) 
            (type range)
            (valid-answers 1 5))  
     ;;----------------------------------------------             
    (question (the-question "Do you value a resort more for its naturalistic beauty than for its attractions? [yes, no] ")
            (preference-topic naturalistic-value)
            (iteration 3) 
            (valid-answers yes no))   
    (question (the-question "Do you like to swim? [no, indifferent, yes] ")
            (preference-topic swim)
            (iteration 3) 
            (valid-answers no indifferent yes))    
    (question (the-question "When on vacation, do you prefer to relax or to be phisically active? [relax, both, active] ")
            (preference-topic sport)
            (iteration 3) 
            (valid-answers relax both active))
    (question (the-question "Do you like to visit museums, art shows, hystorical monuments, etc.? [yes, sometimes, rarely, no] ")
            (preference-topic culture)
            (iteration 3) 
            (valid-answers yes sometimes rarely no))
    ;;-----------------------------------------------
    (question (the-question "Please insert a resort you would like to avoid, if any. Leave blank for none. ")
            (preference-topic ban-resort)
            (type open)
            (always-repeat TRUE)
            (iteration 4) 
            (valid-answers relax both active))
    (question (the-question "Please insert a region you would like to avoid, if any. Leave blank for none. ")
            (preference-topic ban-region)
            (type open)
            (always-repeat TRUE)
            (iteration 4))
    (question (the-question "Please insert a resort you would like to favor, if any. Leave blank for none. ")
            (preference-topic favor-resort)
            (type open)
            (always-repeat TRUE)
            (iteration 4))
    (question (the-question "Please insert a region you would like to favor, if any. Leave blank for none. ")
            (preference-topic favor-region)
            (type open)
            (always-repeat TRUE)
            (iteration 4))
) 


;;***********************
;;* MODULE ASK-QUESTION *
;;***********************

(defmodule ASK-QUESTION (import COMMON ?ALL) (import QUESTION ?ALL))


(deffunction ASK-QUESTION::ask-question (?type ?skip ?question ?allowed-values)   
    (bind ?answer INVALID-ANSWER)
    (bind ?empty FALSE)
    (switch ?type
        (case closed then
            (while (not (member$ ?answer ?allowed-values)) do
                (printout t ?question)
                (bind ?answer (explode$ (readline)))
                (if (and (eq ?skip TRUE) (eq (length$ ?answer) 0)) then 
                    (bind ?answer nil) (bind ?empty TRUE) (break))
                (bind ?answer (nth 1 ?answer))
                (if (lexemep ?answer) then 
                    (bind ?answer (lowcase ?answer)))
            )
        )
        (case range then
            (bind ?min (nth 1 ?allowed-values))
            (bind ?max (nth 2 ?allowed-values)) 
            (while (or (not (integerp ?answer)) (< ?answer ?min) (> ?answer ?max)) do
                (printout t ?question)
                (bind ?answer (explode$ (readline)))
                (if (and (eq ?skip TRUE) (eq (length$ ?answer) 0)) then 
                    (bind ?answer nil) (bind ?empty TRUE) (break))
                (bind ?answer (nth 1 ?answer))
            )
        )
        (case open then
            (bind ?done FALSE)
            (while (not ?done)
                (printout t ?question)
                (bind ?answer (explode$ (readline)))
                (if (and (eq ?skip TRUE) (eq (length$ ?answer) 0)) then 
                    (bind ?answer nil) (bind ?empty TRUE)
                )
                (if (> (length$ ?answer) 0) then
                    (bind ?done TRUE) (bind ?answer (nth 1 (create$ ?answer))) 
                )       
            )
        )
    )
    (return (create$ ?answer ?empty))
)


(defrule ASK-QUESTION::precursor-is-satisfied
    ?f <- (question (precursors ?t is ?v $?rest))
    (preference (topic ?t) (answer-value ?v))
    (iteration (number ?i))
=> 
    (modify ?f (iteration ?i) (precursors ?rest))
)


(defrule ASK-QUESTION::ask-the-end-question
    (declare (salience 500))
    (iteration (number ?i))
    (test (> ?i 0))
=>
    (bind ?q "Are you happy with one of the suggested trips? [yes, no] ")
    (bind ?va (create$ yes no))
    (bind ?answer (ask-question closed FALSE ?q ?va))
    (if (eq ?answer yes) then 
        (printout t "Thank you for using our system. Have a good vacation!" crlf crlf)
        (halt)
        (reset))
)


(defrule ASK-QUESTION::ask-a-question
    (iteration (number ?i))
    ?fact <- (question (iteration ?i)
                    (type ?t)
                    (skippable ?s)
                    (always-repeat ?ar)
                    (never-repeat ?nr)
                    (precursors)
                    (the-question ?q)
                    (preference-topic ?pt)
                    (valid-answers $?va))
=>
    (bind ?answer-pair (ask-question ?t ?s ?q ?va))
    (bind ?answer (nth 1 ?answer-pair))
    (bind ?empty (nth 2 ?answer-pair))
    (printout t " answer value: " ?answer "--" crlf)   
    (if (not ?empty) then 
        (printout t "asserted" crlf)   
        (assert (preference (topic ?pt) (answer-value ?answer)))
    )
    (if (and (not ?nr) (or ?empty ?ar)) then
        (printout t "modified" crlf)   
        (modify ?fact (iteration (+ ?i 1)))  ;;ask again in next iteration
    )
)


;;*****************************
;;* MODULE QUESTION-INFERENCE *
;;*****************************
 

(defmodule QUESTION-INFERENCE (import COMMON ?ALL) (import QUESTION ?ALL))


;;------------ TEMPERATURE ------------

(defrule QUESTION-INFERENCE::temperature-warm
    (preference (topic temperature) (answer-value warm))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF 0.4) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value mountain) (CF -0.4) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::temperature-both
    (preference (topic temperature) (answer-value both))
=>
    (assert (dv (description the-tourism-type) (value mountain) (CF 0.1) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value sea) (CF 0.1) (basic TRUE))) 
)
    
(defrule QUESTION-INFERENCE::temperature-cool
    (preference (topic temperature) (answer-value cool))
=>
    (assert (dv (description the-tourism-type) (value mountain) (CF 0.4) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value sea) (CF -0.4) (basic TRUE))) 
)

;;------------ CULTURE ------------

(defrule QUESTION-INFERENCE::culture-yes
    (preference (topic culture) (answer-value yes))
=>
    (assert (dv (description the-tourism-type) (value cultural) (CF 0.7) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value religious) (CF 0.2) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::culture-sometimes
    (preference (topic culture) (answer-value sometimes))
=>
    (assert (dv (description the-tourism-type) (value cultural) (CF 0.3) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value religious) (CF 0.1) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::culture-rarely
    (preference (topic culture) (answer-value rarely))
=>
    (assert (dv (description the-tourism-type) (value cultural) (CF -0.3) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value religious) (CF -0.1) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::culture-no
    (preference (topic culture) (answer-value no))
=>
    (assert (dv (description the-tourism-type) (value cultural) (CF -0.7) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value religious) (CF -0.2) (basic TRUE)))
)

;;------------ FOOD ------------

(defrule QUESTION-INFERENCE::food
    (preference (topic food) (answer-value ?v))
=>
    (bind ?cf (* 0.6 (/ (- ?v 2.5) 2.5)))
    (assert (dv (description the-tourism-type) (value enogastronomy) (CF ?cf) (basic TRUE)))
)

;;------------ RELIGION ------------

(defrule QUESTION-INFERENCE::religion
    (preference (topic religion) (answer-value ?v))
=>
    (bind ?cf (* 0.6 (/ (- ?v 2.5) 2.5)))
    (assert (dv (description the-tourism-type) (value religious) (CF ?cf) (basic TRUE)))
)

;;------------ COST ------------

(defrule QUESTION-INFERENCE::cost-cheap
    (preference (topic cost) (answer-value cheap))
=>
    (assert (dv (description the-optimal-hotel-stars) (value 1) (CF 0.6) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 2) (CF 0.2) (basic TRUE))) 
    (assert (dv (description the-optimal-hotel-stars) (value 3) (CF -0.2) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 4) (CF -0.6) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-normal
    (preference (topic cost) (answer-value normal))
=>
    (assert (dv (description the-optimal-hotel-stars) (value 1) (CF -0.2) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 2) (CF 0.6) (basic TRUE))) 
    (assert (dv (description the-optimal-hotel-stars) (value 3) (CF 0.6) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 4) (CF -0.2) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-expensive
    (preference (topic cost) (answer-value expensive))
=>
    (assert (dv (description the-optimal-hotel-stars) (value 1) (CF -0.6) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 2) (CF -0.2) (basic TRUE))) 
    (assert (dv (description the-optimal-hotel-stars) (value 3) (CF 0.2) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 4) (CF 0.6) (basic TRUE)))
)

;;------------ NATURALISTIC ------------

(defrule QUESTION-INFERENCE::naturalistic-value-yes
    (preference (topic naturalistic-value) (answer-value yes))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF 0.2) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF 0.2) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value mountain) (CF 0.2) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value naturalistic) (CF 0.6) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::naturalistic-value-no
    (preference (topic naturalistic-value) (answer-value no))
=>
    (assert (dv (description the-tourism-type) (value naturalistic) (CF -0.4) (basic TRUE)))
)

;;------------ SPORT ------------

(defrule QUESTION-INFERENCE::sport-relax
    (preference (topic sport) (answer-value relax))
=>
    (assert (dv (description the-tourism-type) (value sportive) (CF -0.5) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value thermal) (CF 0.5) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::sport-both
    (preference (topic sport) (answer-value both))
=>
    (assert (dv (description the-tourism-type) (value sportive) (CF 0.1) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value thermal) (CF 0.1) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::sport-active
    (preference (topic sport) (answer-value active))
=>
    (assert (dv (description the-tourism-type) (value sportive) (CF 0.5) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value thermal) (CF -0.5) (basic TRUE)))
)

;;------------ SWIM ------------

(defrule QUESTION-INFERENCE::swim-yes
    (preference (topic swim) (answer-value yes))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF 0.5) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF 0.5) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::swim-no
    (preference (topic swim) (answer-value no))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF -0.5) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF -0.5) (basic TRUE)))
)

;;------------ BUDGET ------------

(defrule QUESTION-INFERENCE::budget-limit
    (preference (topic budget-limit) (answer-value ?v))
=>
    (assert (dv (description the-budget-limit) (value ?v) (CF 1.0) (basic TRUE)))
)

;;------------ PEOPLE NUM ------------

(defrule QUESTION-INFERENCE::people-number
    (preference (topic people-number) (answer-value ?v))
=>
    (assert (dv (description the-people-number) (value ?v) (CF 1.0) (basic TRUE)))
)

;;------------ MAX DISTANCE ------------

(defrule QUESTION-INFERENCE::max-distance
    (preference (topic max-distance) (answer-value ?v))
=>
    (assert (dv (description the-max-route-distance) (value ?v) (CF 1.0) (basic TRUE)))
)

;;------------ TRIP DURATION ------------

(defrule QUESTION-INFERENCE::trip-duration
    (preference (topic trip-duration) (answer-value ?v))
=>
    (assert (dv (description the-trip-duration) (value ?v) (CF 1.0) (basic TRUE)))
)

;;------------ TRIP LENGTH ------------

(defrule QUESTION-INFERENCE::trip-length-generic
    (preference (topic trip-length-generic) (answer-value no))
=>
    (assert (dv (description the-trip-length) (value 1) (CF 1.0) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::trip-length
    (preference (topic trip-length) (answer-value ?v))
=>
    (assert (dv (description the-trip-length) (value ?v) (CF 1.0) (basic TRUE)))
)

;;------------ START RESORT ------------

(defrule QUESTION-INFERENCE::start-resort
    (preference (topic start-resort) (answer-value ?r))
=>
    (assert (dv (description the-start-resort) (value ?r) (CF 1.0) (basic TRUE)))
)

;;------------ END RESORT ------------

(defrule QUESTION-INFERENCE::end-resort
    (preference (topic end-resort) (answer-value ?r))
=>
    (assert (dv (description the-end-resort) (value ?r) (CF 1.0) (basic TRUE)))
)

;;------------ DAYS PARTITIONING ------------

(defrule QUESTION-INFERENCE::days-partitioning
    (preference (topic days-partitioning) (answer-value yes))
=>
    (assert (dv (description the-days-partitioning) (value yes) (CF 1.0) (basic TRUE)))
)

;;------------ BAN RESORT ------------

(defrule QUESTION-INFERENCE::ban-resort
    (preference (topic ban-resort) (answer-value ?r))
=>
    (assert (dv (description the-banned-resort) (value ?r) (CF 0.7) (basic TRUE)))
)

;;------------ BAN REGION ------------

(defrule QUESTION-INFERENCE::ban-region
    (preference (topic ban-region) (answer-value ?r))
=>
    (assert (dv (description the-banned-region) (value ?r) (CF 0.7) (basic TRUE)))
)

;;------------ FAVOR RESORT ------------

(defrule QUESTION-INFERENCE::favor-resort
    (preference (topic favor-resort) (answer-value ?r))
=>
    (assert (dv (description the-favourite-resort) (value ?r) (CF 0.7) (basic TRUE)))
)

;;------------ FAVOR REGION ------------

(defrule QUESTION-INFERENCE::favor-region
    (preference (topic favor-region) (answer-value ?r))
=>
    (assert (dv (description the-favourite-region) (value ?r) (CF 0.7) (basic TRUE)))
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
    (slot score (type INTEGER) (range 1 5))
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
    (multislot hotels (default ND ND ND ND ND))
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
    (test (< ?len ?*MAX-TRIP-LENGTH*))
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
=>
    (assert (dv (description the-duration-unit) (value (max 1 (div ?d ?*DURATION-UNIT-RATE*))) (CF 1.0) (basic TRUE)))
)


(defrule INIT::generate-singleton-duration
    (dv (description the-trip-duration) (value ?d))
=>
    (assert (duration (days ?d) (length 1)))  
)

(defrule INIT::generate-duration
    (dv (description the-duration-unit) (value ?u))
    (duration (days $?ds ?d) (length ?len))
    (test (< (length$ (create$ ?ds ?d)) ?*MAX-TRIP-LENGTH*))
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
    (bind ?rcf (/ (* ?s ?cf) ?*MAX-TOURISM-SCORE*))
    (assert (dv (description the-resort) (value ?r) (CF ?rcf)))
)


(defrule RATE-RESORT::rate-resort-by-banned-resorts
    (dv (description the-banned-resort) (value ?r) (CF ?cf))
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF -?cf)))
)

(defrule RATE-RESORT::rate-resort-by-banned-regions
    (dv (description the-banned-region) (value ?rg) (CF ?cf))
    (resort (name ?r) (region ?rg))
=>
    (assert (dv (description the-resort) (value ?r) (CF -?cf)))
)

(defrule RATE-RESORT::rate-resort-by-favourite-resorts
    (dv (description the-favourite-resort) (value ?r) (CF ?cf))
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF ?cf)))
)

(defrule RATE-RESORT::rate-resort-by-favourite-regions
    (dv (description the-favourite-region) (value ?rg) (CF ?cf))
    (resort (name ?r) (region ?rg))
=>
    (assert (dv (description the-resort) (value ?r) (CF ?cf)))
)


(defrule RATE-RESORT::rate-route
    (route (resort-src ?src) (resort-dst ?dst) (distance ?d))
    (dv (description the-max-route-distance) (value ?v))
=>
    (bind ?rcf (min 0.5 (max -0.9 (/ (- ?v ?d) ?*MAX-ROUTE-DISTANCE-TOLERANCE*)))) 
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
    (bind ?new-cf (* 0.8 (/ ?e ?c)))
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
    (bind ?daily-cost (+ ?*HOTEL-BASE-COST* (* ?s ?*HOTEL-ADDITIONAL-COST*)))
    (bind ?cost-all-people (* (max 1 (div ?p 2)) ?daily-cost))
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
    (bind ?tcf (- 0.6 (* (abs (- ?tl ?len)) (/ 1.2 (- ?*MAX-TRIP-LENGTH* 1)))))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-budget-limit
    (trip (trip-id ?id) (costs $?cs))
    (dv (description the-budget-limit) (value ?b))
=>
    (bind ?total-cost (+ (expand$ ?cs) 0))
    (bind ?tcf (min 0.5 (max -0.9 (/ (- ?total-cost ?b) ?*MAX-BUDGET-TOLERANCE*))))   
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)


(defrule BUILD-AND-RATE-TRIP::rate-trip-by-start-resort
    (dv (description the-start-resort) (value ?sr))
    (trip (trip-id ?id) (resorts ?sr $?rs))
=>
    (assert (dv (description the-trip) (value ?id) (CF 0.3)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-end-resort
    (dv (description the-end-resort) (value ?er))
    (trip (trip-id ?id) (resorts $?rs ?er))
=>
    (assert (dv (description the-trip) (value ?id) (CF 0.3)))
)


;;************************
;;* MODULE PRINT-RESULTS *
;;************************
  
(defmodule PRINT-RESULTS (import COMMON ?ALL) (import TRIP ?ALL))

(defrule PRINT-RESULTS::on-enter
    (declare (salience 500))
    (not (printed-trips ?p))
=>
    (assert (printed-trips 1))
)

(defrule PRINT-RESULTS::on-exit
    (declare (salience -500))
    ?fact <- (printed-trips ?p)
=>
    (retract ?fact)
    (pop-focus)
)


(defrule PRINT-RESULTS::results-header
   (declare (salience 10))
   (printed-trips 1)
   (iteration (number ?i))
   =>
   (printout t  crlf crlf)
   (printout t " >>>>>>>>>>>>>>>   SELECTED TRIPS (ITERATION " (+ ?i 1) ")  <<<<<<<<<<<<<<<"  crlf)
   (printout t  crlf)
)
   

(defrule PRINT-RESULTS::print-and-remove-best-trip 
  ?fact1 <- (printed-trips ?p)
  (test (<= ?p 5))
  ?fact2 <- (dv (description the-trip) (value ?tid) (CF ?tcf))	
  (not (dv (description the-trip) (value ?tid2&~?tid) (CF ?tcf2&:(> ?tcf2 ?tcf))))
  (test (> ?tcf ?*MIN-PRINT-CF*))
  (trip (trip-id ?tid) (resorts $?rs) (hotels $?hs) (days $?ds) (costs $?cs) (length ?len))
  =>
  (retract ?fact1)
  (assert (printed-trips (+ ?p 1)))
  (retract ?fact2)
  (bind ?total-cost (+ (expand$ ?cs) 0))
  (printout t  crlf)
  (bind ?l (+ ?len 1))
  (printout t " Trip suggestion " ?p " with certainty: " (round (* ?tcf 100)) "%" crlf)
  (printout t "  - Resorts to visit: " ?rs crlf)
  (printout t "  - Hotels: " (delete$ ?hs ?l 5 ) crlf)
  (printout t "  - Days partitioning: " ?ds crlf)
  (printout t "  - Daily costs: " (delete$ ?cs ?l 5 ) "  |  Total cost: " ?total-cost crlf) 
  (printout t  crlf)
  (printout t "       _________________________________________________" crlf)
  (printout t  crlf)
) 
   

;;*********************
;;* MODULE INVALIDATE *
;;*********************

(defmodule INVALIDATE (import COMMON ?ALL) (import TRIP ?ALL))

(defrule INVALIDATE::invalidate-basic-dv
    ?fact <- (dv (basic TRUE) (updated TRUE))
=>  
    (modify ?fact (updated FALSE))
)

(defrule INVALIDATE::remove-derived-dv
    ?fact <- (dv (basic FALSE))
=>  
    (retract ?fact)
)

(defrule INVALIDATE::remove-trip
    ?t <- (trip)
=>
    (retract ?t)
)


;;******************
;;* MODULE REFRESH *
;;******************

(defmodule REFRESH (import COMMON ?ALL))


(defrule REFRESH::refresh-basic-dv
    ?fact <- (dv (basic TRUE) (updated FALSE))
=>  
    (modify ?fact (updated TRUE))
)


(defrule REFRESH::plsstop
    (declare (salience -200))
    (iteration (number ?i))
=>
    (halt)
) 


(defrule REFRESH::on-exit
    (declare (salience -1000))
    ?fact <- (iteration (number ?i))
=>
    (retract ?fact)
    (assert (iteration (number (+ ?i 1))))  ;;increment iteration number
    (pop-focus)
)

