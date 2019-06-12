
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
    (question (the-question "Please insert the resort from which you want to start your trip, if any. Leave blank for no preference.")
            (preference-topic start-resort)
            (iteration 0) 
            (type open)
            (never-repeat TRUE)
            (precursors trip-length-generic is yes))
    (question (the-question "Please insert the resort where you want to end your trip, if any. Leave blank for no preference.")
            (preference-topic end-resort)
            (iteration 0) 
            (type open)
            (never-repeat TRUE)
            (precursors trip-length-generic is yes))
    (question (the-question "What is the maximum distance that you are willing to travel between two resorts? (between 10 ad 100 km) ")
            (preference-topic max-distance)
            (iteration 0) 
            (type range)
            (precursors trip-length-generic is yes)
            (valid-answers 10 100)) 
    (question (the-question "How many places would you like to visit during your vacation? (between 2 and 5 resorts) ")
            (preference-topic trip-length)
            (iteration 0)
            (type range)
            (skippable FALSE)
            (precursors trip-length-generic is yes)
            (valid-answers 2 5))
    (question (the-question "Would you like to visit more than one resort? [yes, no] ")
            (preference-topic trip-length-generic)
            (iteration 0) 
            (skippable FALSE)
            (valid-answers yes no))
    (question (the-question "Are you looking for a cheap trip or a luxurious and more expensive one? [cheap, normal, expensive] ")
            (preference-topic cost)
            (iteration 0) 
            (valid-answers cheap normal expensive))
    (question (the-question "Do you generally prefer cool or warm places? [cool, both, warm] ")            
            (preference-topic temperature)
            (iteration 0) 
            (valid-answers cool both warm))
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
    ;;----------------------------------------------
    (question (the-question "How much it is important for you to eat at good places? (between 0 and 5) ")
            (preference-topic food)
            (never-repeat TRUE)
            (iteration 1) 
            (type range)
            (valid-answers 0 5))
    (question (the-question "How much are you interested in places that are relevant from a religious point of view? (between 0 and 5) ")
            (preference-topic religion)
            (iteration 1) 
            (type range)
            (valid-answers 0 5))               
    (question (the-question "Do you value a resort more for its naturalistic beauty than for its attractions? [yes, no] ")
            (preference-topic naturalistic-value)
            (iteration 1) 
            (valid-answers yes no))   
    (question (the-question "Wolud you like to go to the beach, swim, and do other sea/lake related activities? [yes, indifferent, no] ")
            (preference-topic swim)
            (iteration 1) 
            (valid-answers no indifferent yes))  
    (question (the-question "Do you feel safe to swim in lakes? [yes, no] ")
            (preference-topic swim-lake)
            (iteration 1) 
            (precursors swim is yes)  
            (valid-answers yes no))  
    (question (the-question "When on vacation, do you prefer to relax or to be phisically active? [relax, both, active] ")
            (preference-topic sport)
            (iteration 1) 
            (valid-answers relax both active))
    (question (the-question "Do you like to visit museums, art shows, hystorical monuments, etc.? [yes, sometimes, rarely, no] ")
            (preference-topic culture)
            (iteration 1) 
            (valid-answers yes sometimes rarely no))
    (question (the-question "Do you have a precise budget limit? [yes, no] ")
            (preference-topic budget-limit-generic)
            (iteration 1) 
            (valid-answers yes no))
    (question (the-question "Please insert your budget limit. (between 100 and 999999 euros) ")
            (preference-topic budget-limit)
            (skippable FALSE)
            (iteration 1) 
            (type range)
            (precursors budget-limit-generic is yes)
            (valid-answers 100 999999))
    ;;-----------------------------------------------
    (question (the-question "Please insert a resort you would like to avoid, if any. Leave blank for none. ")
            (preference-topic ban-resort)
            (type open)
            (always-repeat TRUE)
            (iteration 2))
    (question (the-question "Please insert a region you would like to avoid, if any. Leave blank for none. ")
            (preference-topic ban-region)
            (type open)
            (always-repeat TRUE)
            (iteration 2))
    (question (the-question "Please insert a resort you would like to favor, if any. Leave blank for none. ")
            (preference-topic favor-resort)
            (type open)
            (always-repeat TRUE)
            (iteration 2))
    (question (the-question "Please insert a region you would like to favor, if any. Leave blank for none. ")
            (preference-topic favor-region)
            (type open)
            (always-repeat TRUE)
            (iteration 2))
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
    ;;(printout t " answer value: " ?answer "--" crlf)   
    (if (not ?empty) then 
        ;;(printout t "asserted" crlf)   
        (assert (preference (topic ?pt) (answer-value ?answer)))
    )
    (if (and (not ?nr) (or ?empty ?ar)) then
        ;;(printout t "modified" crlf)   
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
    (assert (dv (description the-tourism-type) (value cultural) (CF 0.6) (basic TRUE)))
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
    (assert (dv (description the-tourism-type) (value cultural) (CF 0.1) (basic TRUE)))
)

;;------------ FOOD ------------

(defrule QUESTION-INFERENCE::food
    (preference (topic food) (answer-value ?v&:(> ?v 0)))
=>
    (bind ?cf (* 0.6 (/ ?v 5)))
    (assert (dv (description the-tourism-type) (value enogastronomic) (CF ?cf) (basic TRUE)))
)

;;------------ RELIGION ------------

(defrule QUESTION-INFERENCE::religion
    (preference (topic religion) (answer-value ?v&:(> ?v 0)))
=>
    (bind ?cf (* 0.6 (/ ?v 5)))
    (assert (dv (description the-tourism-type) (value religious) (CF ?cf) (basic TRUE)))
)

;;------------ COST ------------

(defrule QUESTION-INFERENCE::cost-cheap
    (preference (topic cost) (answer-value cheap))
=>
    (assert (dv (description the-optimal-hotel-stars) (value 1) (CF 0.4) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 2) (CF 0.2) (basic TRUE))) 
    (assert (dv (description the-optimal-hotel-stars) (value 3) (CF -0.2) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 4) (CF -0.4) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-normal
    (preference (topic cost) (answer-value normal))
=>
    (assert (dv (description the-optimal-hotel-stars) (value 1) (CF -0.2) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 2) (CF 0.4) (basic TRUE))) 
    (assert (dv (description the-optimal-hotel-stars) (value 3) (CF 0.4) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 4) (CF -0.2) (basic TRUE))) 
)

(defrule QUESTION-INFERENCE::cost-expensive
    (preference (topic cost) (answer-value expensive))
=>
    (assert (dv (description the-optimal-hotel-stars) (value 1) (CF -0.4) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 2) (CF -0.2) (basic TRUE))) 
    (assert (dv (description the-optimal-hotel-stars) (value 3) (CF 0.2) (basic TRUE)))
    (assert (dv (description the-optimal-hotel-stars) (value 4) (CF 0.4) (basic TRUE)))
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

;;------------ SPORT ------------

(defrule QUESTION-INFERENCE::sport-relax
    (preference (topic sport) (answer-value relax))
=>
    (assert (dv (description the-tourism-type) (value thermal) (CF 0.5) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::sport-both
    (preference (topic sport) (answer-value both))
=>
    (assert (dv (description the-tourism-type) (value sportive) (CF 0.3) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value thermal) (CF 0.3) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::sport-active
    (preference (topic sport) (answer-value active))
=>
    (assert (dv (description the-tourism-type) (value sportive) (CF 0.5) (basic TRUE)))
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
    (assert (dv (description the-tourism-type) (value sea) (CF -0.1) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF -0.1) (basic TRUE)))
)

;;------------ SWIM-LAKE ------------

(defrule QUESTION-INFERENCE::swim-lake-no
    (preference (topic swim-lake) (answer-value no))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF 0.1) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF -0.6) (basic TRUE)))
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

;;------------ DAYS PARTITIONING ------------ UNUSED

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
    (resort (name Biancavilla) (region Kanto))
    (resort (name Lavandonia) (region Kanto))
    (resort (name Plumbeopoli) (region Kanto))
    (resort (name Smeraldopoli) (region Kanto))
    (resort (name Celestopoli) (region Kanto))
    (resort (name Aranciopoli) (region Kanto))
    (resort (name Fucsiapoli) (region Kanto))
    (resort (name Azzuropoli) (region Kanto))
    (resort (name Zafferanopoli) (region Kanto))
    (resort (name Isola_Cannella) (region Kanto))
    (resort (name Borgo_Foglianova) (region Johto))
    (resort (name Amarantopoli) (region Johto))
    (resort (name Fiorpescopoli) (region Johto))
    (resort (name Violapoli) (region Johto))
    (resort (name Azalina) (region Johto))
    (resort (name Mogania) (region Johto))
    (resort (name Fiordoropoli) (region Johto))
    (resort (name Olivinopoli) (region Johto))
    (resort (name Ebanopoli) (region Johto))
)
  
(deffacts RESORT::resort-tourism-list 
    (resort-tourism (resort-name Biancavilla) (tourism-type naturalistic) (score 3))
    (resort-tourism (resort-name Biancavilla) (tourism-type cultural) (score 3))
    (resort-tourism (resort-name Biancavilla) (tourism-type lake) (score 2))
    (resort-tourism (resort-name Smeraldopoli) (tourism-type naturalistic) (score 5))
    (resort-tourism (resort-name Smeraldopoli) (tourism-type lake) (score 3))
    (resort-tourism (resort-name Plumbeopoli) (tourism-type mountain) (score 3))
    (resort-tourism (resort-name Plumbeopoli) (tourism-type cultural) (score 4))
    (resort-tourism (resort-name Celestopoli) (tourism-type sea) (score 3))
    (resort-tourism (resort-name Celestopoli) (tourism-type sportive) (score 4))
    (resort-tourism (resort-name Celestopoli) (tourism-type enogastronomic) (score 3))
    (resort-tourism (resort-name Aranciopoli) (tourism-type sea) (score 5))
    (resort-tourism (resort-name Aranciopoli) (tourism-type sportive) (score 2))
    (resort-tourism (resort-name Aranciopoli) (tourism-type cultural) (score 1))
    (resort-tourism (resort-name Lavandonia) (tourism-type mountain) (score 3))
    (resort-tourism (resort-name Lavandonia) (tourism-type lake) (score 2))
    (resort-tourism (resort-name Lavandonia) (tourism-type religious) (score 5))
    (resort-tourism (resort-name Azzurropoli) (tourism-type religious) (score 3))
    (resort-tourism (resort-name Azzurropoli) (tourism-type enogastronomic) (score 3))
    (resort-tourism (resort-name Azzurropoli) (tourism-type sportive) (score 2))
    (resort-tourism (resort-name Fucsiapoli) (tourism-type cultural) (score 2))
    (resort-tourism (resort-name Fucsiapoli) (tourism-type sportive) (score 3))
    (resort-tourism (resort-name Fucsiapoli) (tourism-type sea) (score 2))
    (resort-tourism (resort-name Fucsiapoli) (tourism-type naturalistic) (score 3))
    (resort-tourism (resort-name Zafferanopoli) (tourism-type sportive) (score 4))
    (resort-tourism (resort-name Zafferanopoli) (tourism-type thermal) (score 3))
    (resort-tourism (resort-name Zafferanopoli) (tourism-type enogastronomic) (score 2))
    (resort-tourism (resort-name Isola_Cannella) (tourism-type thermal) (score 3))
    (resort-tourism (resort-name Isola_Cannella) (tourism-type sea) (score 5))
    (resort-tourism (resort-name Isola_Cannella) (tourism-type cultural) (score 3))
    (resort-tourism (resort-name Borgo_Foglianova) (tourism-type cultural) (score 3))
    (resort-tourism (resort-name Borgo_Foglianova) (tourism-type sea) (score 1))
    (resort-tourism (resort-name Fiorpescopoli) (tourism-type sea) (score 3))
    (resort-tourism (resort-name Fiorpescopoli) (tourism-type enogastronomic) (score 3))
    (resort-tourism (resort-name Violapoli) (tourism-type cultural) (score 4))
    (resort-tourism (resort-name Violapoli) (tourism-type religious) (score 4))
    (resort-tourism (resort-name Azalina) (tourism-type naturalistic) (score 3))
    (resort-tourism (resort-name Azalina) (tourism-type cultural) (score 2))
    (resort-tourism (resort-name Fiordoropoli) (tourism-type sea) (score 3))
    (resort-tourism (resort-name Fiordoropoli) (tourism-type sportive) (score 2))
    (resort-tourism (resort-name Fiordoropoli) (tourism-type enogastronomic) (score 3))
    (resort-tourism (resort-name Amarantopoli) (tourism-type cultural) (score 5))
    (resort-tourism (resort-name Amarantopoli) (tourism-type naturalistic) (score 3))
    (resort-tourism (resort-name Amarantopoli) (tourism-type religious) (score 3))
    (resort-tourism (resort-name Olivinopoli) (tourism-type sea) (score 3))
    (resort-tourism (resort-name Olivinopoli) (tourism-type enogastronomic) (score 4))
    (resort-tourism (resort-name Mogania) (tourism-type mountain) (score 3))
    (resort-tourism (resort-name Mogania) (tourism-type lake) (score 5))
    (resort-tourism (resort-name Ebanopoli) (tourism-type mountain) (score 5))
    (resort-tourism (resort-name Ebanopoli) (tourism-type thermal) (score 3))
)

(deffacts RESORT:route-list
    (route (resort-src Olivinopoli) (resort-dst Amarantopoli) (distance 45))
    (route (resort-src Olivinopoli) (resort-dst Fiordoropoli) (distance 35))
    (route (resort-src Amarantopoli) (resort-dst Mogania) (distance 15))
    (route (resort-src Amarantopoli) (resort-dst Violapoli) (distance 25))
    (route (resort-src Amarantopoli) (resort-dst Fiordoropoli) (distance 45))
    (route (resort-src Violapoli) (resort-dst Fiordoropoli) (distance 45))
    (route (resort-src Mogania) (resort-dst Ebanopoli) (distance 20))
    (route (resort-src Ebanopoli) (resort-dst Violapoli) (distance 50))
    (route (resort-src Fiordoropoli) (resort-dst Azalina) (distance 30))   
    (route (resort-src Fiordoropoli) (resort-dst Fiorpescopoli) (distance 45)) 
    (route (resort-src Azalina) (resort-dst Violapoli) (distance 65))
    (route (resort-src Azalina) (resort-dst Fiorpescopoli) (distance 55))
    (route (resort-src Violapoli) (resort-dst Fiorpescopoli) (distance 45))
    (route (resort-src Ebanopoli) (resort-dst Fiorpescopoli) (distance 55))
    (route (resort-src Ebanopoli) (resort-dst Violapoli) (distance 40))
    (route (resort-src Ebanopoli) (resort-dst Borgo_Foglianova) (distance 50))
    (route (resort-src Ebanopoli) (resort-dst Plumbeopoli) (distance 65))
    (route (resort-src Borgo_Foglianova) (resort-dst Smeraldopoli) (distance 55))
    (route (resort-src Borgo_Foglianova) (resort-dst Biancavilla) (distance 40))
    (route (resort-src Smeraldopoli) (resort-dst Plumbeopoli) (distance 25))
    (route (resort-src Smeraldopoli) (resort-dst Ebanopoli) (distance 55))
    (route (resort-src Smeraldopoli) (resort-dst Biancavilla) (distance 20))
    (route (resort-src Smeraldopoli) (resort-dst Azzurropoli) (distance 30))
    (route (resort-src Biancavilla) (resort-dst Isola_Cannella) (distance 15))
    (route (resort-src Biancavilla) (resort-dst Aranciopoli) (distance 45))
    (route (resort-src Isola_Cannella) (resort-dst Fucsiapoli) (distance 35))
    (route (resort-src Plumbeopoli) (resort-dst Celestopoli) (distance 35))
    (route (resort-src Plumbeopoli) (resort-dst Azzurropoli) (distance 30))
    (route (resort-src Celestopoli) (resort-dst Zafferanopoli) (distance 15))
    (route (resort-src Celestopoli) (resort-dst Lavandonia) (distance 35))
    (route (resort-src Celestopoli) (resort-dst Aranciopoli) (distance 25))
    (route (resort-src Zafferanopoli) (resort-dst Azzurropoli) (distance 10))
    (route (resort-src Zafferanopoli) (resort-dst Lavandonia) (distance 20))
    (route (resort-src Zafferanopoli) (resort-dst Aranciopoli) (distance 10))
    (route (resort-src Aranciopoli) (resort-dst Lavandonia) (distance 30))
    (route (resort-src Aranciopoli) (resort-dst Fucsiapoli) (distance 45))
    (route (resort-src Lavandonia) (resort-dst Fucsiapoli) (distance 55))
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
    (hotel (name OakInn) (resort Biancavilla) (stars 3) (empty 8) (capacity 20))
    (hotel (name TheHome) (resort Biancavilla) (stars 1) (empty 12) (capacity 40))
    (hotel (name GreenGreens) (resort Smeraldopoli) (stars 2) (empty 30) (capacity 40))
    (hotel (name TheCave) (resort  Plumbeopoli) (stars 3) (empty 12) (capacity 20))
    (hotel (name Broks) (resort  Plumbeopoli) (stars 1) (empty 12) (capacity 40))
    (hotel (name SweetMemory) (resort Lavandonia) (stars 4) (empty 7) (capacity 40))
    (hotel (name HolyHotel) (resort Lavandonia) (stars 3) (empty 10) (capacity 20))
    (hotel (name SeasideHouse) (resort Celestopoli) (stars 3) (empty 15) (capacity 25))
    (hotel (name Mistys) (resort Celestopoli) (stars 1) (empty 20) (capacity 30))
    (hotel (name SeaStar) (resort Celestopoli) (stars 4) (empty 10) (capacity 30))
    (hotel (name Excelsior) (resort Zafferanopoli) (stars 4) (empty 30) (capacity 100))
    (hotel (name Hyatt) (resort Zafferanopoli) (stars 2) (empty 40) (capacity 50))
    (hotel (name FisherInn) (resort Zafferanopoli) (stars 1) (empty 20) (capacity 50))
    (hotel (name FullHotel) (resort Aranciopoli) (stars 2) (empty 10) (capacity 50))
    (hotel (name EmptyHotel) (resort Aranciopoli) (stars 2) (empty 40) (capacity 60))
    (hotel (name EasyInn) (resort Fucsiapoli) (stars 2) (empty 45) (capacity 70))
    (hotel (name PlusHotel) (resort Fucsiapoli) (stars 4) (empty 10) (capacity 30))
    (hotel (name Diamond) (resort Isola_Cannella) (stars 4) (empty 20) (capacity 50))
    (hotel (name Pearl) (resort Isola_Cannella) (stars 3) (empty 15) (capacity 30))
    (hotel (name BlueHue) (resort Azzurropoli) (stars 2) (empty 10) (capacity 50))
    (hotel (name OneStar) (resort Azzurropoli) (stars 1) (empty 20) (capacity 40))
    (hotel (name NewBark) (resort Borgo_Foglianova) (stars 2) (empty 10) (capacity 20))
    (hotel (name OldShip) (resort Fiorpescopoli) (stars 3) (empty 2) (capacity 20))
    (hotel (name FreshInn) (resort Fiorpescopoli) (stars 2) (empty 10) (capacity 40))
    (hotel (name ThePeak) (resort Ebanopoli) (stars 2) (empty 10) (capacity 40))
    (hotel (name FreezeTop) (resort Ebanopoli) (stars 3) (empty 30) (capacity 50))
    (hotel (name LakeSide) (resort Mogania) (stars 3) (empty 35) (capacity 50))
    (hotel (name DragonTear) (resort Mogania) (stars 1) (empty 10) (capacity 20))
    (hotel (name MystInn) (resort Mogania) (stars 2) (empty 30) (capacity 70))
    (hotel (name TheDeal) (resort Amarantopoli) (stars 1) (empty 25) (capacity 40))
    (hotel (name Alpha) (resort Violapoli) (stars 1) (empty 25) (capacity 50))
    (hotel (name Beta) (resort Violapoli) (stars 4) (empty 25) (capacity 40))
    (hotel (name Gamma) (resort Violapoli) (stars 1) (empty 35) (capacity 40))
    (hotel (name TheCruise) (resort Fiordoropoli) (stars 2) (empty 35) (capacity 70))
    (hotel (name FlagHouse) (resort Fiordoropoli) (stars 1) (empty 10) (capacity 50))
    (hotel (name SlowPeace) (resort Azalina) (stars 2) (empty 35) (capacity 40))
    (hotel (name FullMoon) (resort Azalina) (stars 4) (empty 45) (capacity 60))
    (hotel (name TallWaves) (resort Olivinopoli) (stars 2) (empty 45) (capacity 70))
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

(defrule INIT::remove-wrong-length-path
    (declare (salience -200))
    ?p <- (path (length ?len))
    (dv (description the-trip-length) (value ?l))
    (test (> (abs (- ?l ?len)) 1))
=>
    (retract ?p)
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

(defrule INIT::remove-wrong-length-duration
    (declare (salience -200))
    ?d <- (duration (length ?len))
    (dv (description the-trip-length) (value ?l))
    (test (> (abs (- ?l ?len)) 1))
=>
    (retract ?d)
)

;; optional, to remove unbalanced durations and thus reduce the number of possibile combinations
(defrule INIT::remove-unbalanced-duration
    (declare (salience -200))
    (dv (description the-duration-unit) (value ?u))
    ?fact <- (duration (days $?dl ?d1 $?dc ?d2 $?dr) (length ?len))
    (test (> (abs (- ?d1 ?d2)) ?u))
=>
    (retract ?fact)  
)


;;**********************
;;* MODULE RATE-RESORT *
;;**********************
  
(defmodule RATE-RESORT (import COMMON ?ALL) (import RESORT ?ALL))
 

(defrule RATE-RESORT::rate-resort-by-tourism-type
    (dv (description the-tourism-type) (value ?t) (CF ?cf))
    (resort (name ?r))
    (resort-tourism (resort-name ?r) (tourism-type ?t) (score ?s))
=>
    (bind ?rcf (/ (* ?s ?cf) ?*MAX-TOURISM-SCORE*))
    (assert (dv (description the-resort) (value ?r) (CF ?rcf)))
)

(defrule RATE-RESORT::rate-resort-by-lack-of-interest
    (resort (name ?r))
    (not (and (resort-tourism (resort-name ?r) (tourism-type ?t) (score ?s))
              (dv (description the-tourism-type) (value ?t) (CF ?cf&:(> ?cf 0)))))
=>
    (assert (dv (description the-resort) (value ?r) (CF -0.3)))
)


(defrule RATE-RESORT::rate-resort-by-banned-resorts
    (dv (description the-banned-resort) (value ?r) (CF ?cf))
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF (* -0.5 ?cf))))
)

(defrule RATE-RESORT::rate-resort-by-banned-regions
    (dv (description the-banned-region) (value ?rg) (CF ?cf))
    (resort (name ?r) (region ?rg))
=>
    (assert (dv (description the-resort) (value ?r) (CF (* -0.3 ?cf))))
)

(defrule RATE-RESORT::rate-resort-by-favourite-resorts
    (dv (description the-favourite-resort) (value ?r) (CF ?cf))
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF (* 0.5 ?cf))))
)

(defrule RATE-RESORT::rate-resort-by-favourite-regions
    (dv (description the-favourite-region) (value ?rg) (CF ?cf))
    (resort (name ?r) (region ?rg))
=>
    (assert (dv (description the-resort) (value ?r) (CF (* 0.3 ?cf))))
)


(defrule RATE-RESORT::rate-route
    (route (resort-src ?src) (resort-dst ?dst) (distance ?d))
    (dv (description the-max-route-distance) (value ?v))
=>
    (bind ?rcf (min 0.3 (max -0.9 (/ (- ?v ?d) ?*MAX-ROUTE-DISTANCE-TOLERANCE*)))) 
    (assert (dv (description use-route) (value ?src ?dst) (CF ?rcf))) 
)


;;*********************
;;* MODULE RATE-HOTEL *
;;*********************

(defmodule RATE-HOTEL (import COMMON ?ALL) (import HOTEL ?ALL))

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
    (bind ?new-cf (* 0.4 (/ ?e ?c)))
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

;;(defrule BUILD-AND-RATE-TRIP::rate-trip-by-routes
;;    (trip (trip-id ?id) (resorts $?rl ?rs ?rd $?rr) (length ?len))
;;    (dv (description use-route) (value ?rs ?rd) (CF ?rcf))
;;=>  
;;    (bind ?tcf (/ ?rcf (- ?len 1)))       ;;len resorts imply len-1 routes
;;    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
;;)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-hotels 
    (trip (trip-id ?id) (resorts $?rl ?r $?rr) (hotels $?hs) (days $?ds) (length ?len))
    (dv (description the-hotel-in ?r) (value ?h) (CF ?hcf))
    (test (eq ?h (nth (member$ ?r (create$ ?rl ?r ?rr)) ?hs))) 
    (dv (description the-trip-duration) (value ?td))
=>  
    (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?d (nth ?index ?ds))
    (bind ?tcf (/ (* ?d ?hcf) (* ?td ?len)))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-length
    (trip (trip-id ?id) (length ?len))
    (dv (description the-trip-length) (value ?tl))
=> 
    (bind ?tcf (- 0.4 (* (abs (- ?tl ?len)) (/ 0.8 (- ?*MAX-TRIP-LENGTH* 1)))))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-budget-limit
    (trip (trip-id ?id) (costs $?cs))
    (dv (description the-budget-limit) (value ?b))
=>
    (bind ?total-cost (+ (expand$ ?cs) 0))
    (bind ?tcf (min 0.1 (max -0.9 (/ (- ?b ?total-cost) ?*MAX-BUDGET-TOLERANCE*))))   
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

(defrule BUILD-AND-RATE-TRIP::remove-suboptimal-trip
    (declare (salience -400))
    (trip (trip-id ?id1) (resorts $?rs))
    ?fact1 <- (trip (trip-id ?id2&~?id1) (resorts $?rs))
    (dv (description the-trip) (value ?id1) (CF ?cf1))
    ?fact2 <- (dv (description the-trip) (value ?id2) (CF ?cf2&:(<= ?cf2 ?cf1)))
=>
    (retract ?fact1)
    (retract ?fact2)
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
  (printout t " Trip suggestion " ?p " with certainty: " (round (* ?tcf 100)) "%" crlf)
  (printout t "  - Resorts to visit: " ?rs crlf)
  (printout t "  - Hotels: " (subseq$ ?hs 1 ?len ) crlf)
  (printout t "  - Days partitioning: " ?ds crlf)
  (printout t "  - Daily costs: " (subseq$ ?cs 1 ?len ) "  |  Total cost: " ?total-cost crlf) 
  (printout t  crlf)
  (printout t "       _____________________________________________________" crlf)
  (printout t  crlf)
) 
   

;;*********************
;;* MODULE INVALIDATE *
;;*********************

(defmodule INVALIDATE (import COMMON ?ALL) (import TRIP ?ALL))

(defrule INVALIDATE::plsstop
    (declare (salience 200))
    (iteration (number ?i))
=>
    (halt)
) 


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





(defrule REFRESH::on-exit
    (declare (salience -1000))
    ?fact <- (iteration (number ?i))
=>
    (retract ?fact)
    (assert (iteration (number (+ ?i 1))))  ;;increment iteration number
    (pop-focus)
)

