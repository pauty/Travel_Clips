;;****************
;;* MODULE MAIN  *
;;****************


(defmodule MAIN (export ?ALL))


(deftemplate MAIN::preference
   (slot topic)
   (slot answer-value)
)
  
  
(deftemplate MAIN::dv 
   (multislot description)
   (multislot value)
   (slot CF (default 100.0))
)

(deftemplate MAIN::dv-numeric
   (multislot description)
   (multislot value (type INTEGER))
   (slot CF (default 100.0))
)
   

(deffacts MAIN::control-information
(phase-sequence QUESTIONS QUESTION-INFERENCE RESORT HOTEL SUGGESTION SHOW)
)

(defrule MAIN::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
)

  
(defrule MAIN::combine-certainties-both-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description ?d) (value ?v) (CF ?C1&:(>= ?C1 0.0)))
    ?fact2 <- (dv (description ?d) (value ?v) (CF ?C2&:(>= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (- (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (CF ?C3))
)
    
(defrule MAIN::combine-certainties-both-negative
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description ?d) (value ?v) (CF ?C1&:(< ?C1 0.0)))
    ?fact2 <- (dv (description ?d) (value ?v) (CF ?C2&:(< ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (+ (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (CF ?C3))
)

(defrule MAIN::combine-certainties-negative-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description ?d) (value ?v) (CF ?C1))
    ?fact2 <- (dv (description ?d) (value ?v) (CF ?C2))
    (test (neq ?fact1 ?fact2))
    (< 0 (* ?C1 ?C2))
=>
    (retract ?fact1)
    (bind ?C3 (/ (+ ?C1 ?C2) (- 1 (min (abs(?C1) abs(?C2))))))
    (modify ?fact2 (CF ?C3))
)


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?question ?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)
   

;;********************
;;* MODULE QUESTIONS *
;;********************

(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate QUESTIONS::question
   (slot preference-topic (default ?NONE))
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
) 

(defrule QUESTIONS::ask-a-question
   (not (already-answered))
   ?f <- (question (already-asked FALSE)
                   (precursors)
                   (the-question ?the-question)
                   (preference-topic ?the-preference)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (preference (topic ?the-preference)
                       (answer-value (ask-question ?the-question ?valid-answers))))
   (assert (already-answered))
)
   

(defrule QUESTION::change-phase
    (already-answered)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
    (retract (already-answered))
)

;;*****************************
;;* MODULE QUESTION-INFERENCE *
;;*****************************
 

(defmodule QUESTION-INFERENCE (import MAIN ?ALL))

(defrule QUESTION-INFERENCE::temperature
    (preference ((topic temperature) (answer-value hot)))
=>
    (assert (dv (description tourism-type-is) (value sea) (cf 0.8)))
    (assert (dv (description tourism-type-is) (value mountain) (cf 0.5))) 
)
    
(defrule QUESTION-INFERENCE::temperature
    (preference ((topic temperature) (answer-value cool)))
=>
    (assert (dv (description tourism-type-is) (value mountain) (cf 0.8)))
    (assert (dv (description tourism-type-is) (value sea) (cf 0.5))) 
)
    
(defrule QUESTION-INFERENCE::interest
    (preference ((topic interest) (answer-value museum)))
=>
    (assert (dv (description tourism-type-is) (value cultural) (cf 0.8)))
    (assert (dv (description tourism-type-is) (value religious) (cf 0.5))) 
)
     

(defrule QUESTION-INFERENCE::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
)
  
  
;;*****************
;;* MODULE RESORT *
;;*****************
  
(defmodule RESORT (export ?ALL))
  
;;;;;;;;; TEMPLATES ;;;;;;;;; 

(deftemplate RESORT::resort
  (slot name (type STRING) (default ?NONE))
  (slot (type STRING) region (default any))
  
(deftemplate RESORT::resort-tourism
  (slot resort-name  (default ?NONE))
  (slot tourism-type (default ?NONE))
  (slot score (type INTEGER) (range 0 5))) 
  

(deftemplate RESORT::route
  (slot resort-src (default ?NONE))
  (slot resort-dst (default ?NONE))
  (slot distance (type INTEGER) (range 1 ?VARIABLE))) 
   
  
;;;;;;;;; FACTS ;;;;;;;;; 
  
(deffacts RESORT::resort-list 
  (resort (name BiancaVilla) (region Kanto))
  (resort (name Lavandonia) (region Kanto))
  (resort (name MonteFatuo) (region Kanto)))
  
(deffacts RESORT::resort-tourism-list 
  (resort-tourism (resort-name BiancaVilla) (tourism-type sea) (score 4))
  (resort-tourism (resort-name BiancaVilla) (tourism-type mountain) (score 2))
  (resort-tourism (resort-name Lavandonia) (tourism-type mare) (score 3))
  (resort-tourism (resort-name MonteFatuo) (tourism-type mountain) (score 4)))
  
;;;;;;;;; RULES ;;;;;;;;; 

(defrule RESORT::rate-resort
    (dv ((description tourism-type-is) (value ?v)))
    (resort (name ?rn))
    (resort-tourism (resort-name ?rn) (tourism-type ?v) (score ?s))
=>
    (assert (dv (description the-resort) (value ?rn) (cf (* ?s 0.2))))
)


(defrule RESORT::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
)


;;****************
;;* MODULE HOTEL *
;;****************
 

(defmodule HOTEL (export ?ALL) (import QUESTION-INFERENCE ?ALL))

;;;;;;;; TEMPLATES ;;;;;;;;;;;;

(deftemplate HOTEL::hotel
  (slot name (default ?NONE))
  (slot resort)
  (slot stars (type INTEGER) (range 1 4))
  (slot availability (type INTEGER) (range 0 ?VARIABLE)))


   
;;;;;;;; FACTS ;;;;;;;;;;;;

   
;;;;;;;; RULES ;;;;;;;;;;;;


(defrule HOTEL::rate-hotel-by-stars
    (dv-numeric ((description how-many-hotel-stars) (value ?s) (cf ?c)))
    (dv-numeric ((description how-many-people) (value ?p)))
    (hotel (name ?hn) (stars ?hs) (availability ?a&:(>= ?a ?p)))
=>
    (assert (dv (description the-hotel-in ?rn) (value ?hn) (cf ((* ?s 20)))) )
    
(defrule HOTEL::rate-hotel-by-cost
    (basic-attribute ((name hotel-cost) (value ?people-v) (cf ?c)))
    (basic-attribute ((name people-number) (value ?people-v)))
    (hotel (name ?hn) (stars ?hotel-stars) (availability ?av&:(>= ?av ?people-v)))
=>
    (assert (basic-attribute (name resort-name) (value ?rn) (cf (* ?s 20)))) )

  
(defrule HOTEL::rate-resort-hotel-assotiation
    (basic-attribute (name resort-name) (value ?resort-v) (cf ?resort-c))
    (basic-attribute (name hotel-name) (value ?hotel-v) (cf ?hotel-c))
=>
    (assert (resort-hotel-assotiation (resort-name ?resort-v) (hotel-name ?resort-v) (cf (min ?resort-c ?hotel-c)))) )


;;*****************
;;* MODULE TRAVEL *
;;*****************
 

(defmodule TRAVEL (export ?ALL) (import HOTEL ?ALL))
    
(deftemplate TRAVEL::travel
  (multislot resorts-to-visit )
  (slot total-cost (type INTEGER) (range 0 ?VARIABLE))
  (slot duration (type INTEGER) (range 0 ?VARIABLE)))  
  
(defrule RESORT::rate-travel
    (resort-hotel-assotiation ((name resort-name) (value ?resort-v1) (cf ?c1))) 
    (resort (name ?rn))
=>
    (assert (attribute (name resort-name) (value ?rn) (cf (* ?s 20)))) )
  

  
