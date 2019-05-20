;;****************
;;* MODULE MAIN  *
;;****************


(defmodule MAIN (export ?ALL))

  
(deftemplate MAIN::basic-attribute
   (slot name)
   (slot value)
   (slot certainty (default 100.0)))
   
   
(deftemplate MAIN::preference
   (slot name)
   (slot answer-value))


(deffacts MAIN::control-information
(phase-sequence QUESTIONS QUESTION-INFERENCE RESORT HOTEL SUGGESTION SHOW))

(defrule MAIN::change-phase
    (declare (salience -1000)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase)))


(defrule MAIN::combine-certainties ""
  (declare (salience 100)
           (auto-focus TRUE))
  ?rem1 <- (basic-attribute (name ?rel) (value ?val) (certainty ?per1))
  ?rem2 <- (basic-attribute (name ?rel) (value ?val) (certainty ?per2))
  (test (neq ?rem1 ?rem2))
  =>
  (retract ?rem1)
  (modify ?rem2 (certainty (/ (- (* 100 (+ ?per1 ?per2)) (* ?per1 ?per2)) 100))))


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
   (slot preference-name (default ?NONE))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE)))
   

(deffacts QUESTIONS::questions-list
  (question (preference-name temperature)
            (the-question "Posti caldi o freddi?")
            (valid-answers cool both warm))
  (question (preference-name cost)
            (the-question "Viaggio economico o lussuoso?")
            (valid-answers cheap budget normal expensive very-expensive))
  (question (preference-name culture)
            (the-question "Ti interessa la cultura?")
            (valid-answers no sometimes yes))
) 

(defrule QUESTIONS::ask-a-question
   (not (already-answered))
   ?f <- (question (already-asked FALSE)
                   (precursors)
                   (the-question ?the-question)
                   (preference-name ?the-preference)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (preference (name ?the-preference)
                       (answer-value (ask-question ?the-question ?valid-answers))))
   (assert (already-answered)))
   

(defrule QUESTION::change-phase
    (already-answered)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
    (retract (already-answered))

;;*****************************
;;* MODULE QUESTION-INFERENCE *
;;*****************************
 

(defmodule QUESTION-INFERENCE (import MAIN ?ALL))

(defrule QUESTION-INFERENCE::temperature
    (preference ((name temperature) (answer-value hot)))
=>
    (assert (basic-attribute (name tourism-type) (value sea) (certainty 80)))
    (assert (basic-attribute (name tourism-type) (value mountain) (certainty 50))) )
    
(defrule QUESTION-INFERENCE::temperature
    (preference ((name temperature) (answer-value cool)))
=>
    (assert (basic-attribute (name tourism-type) (value mountain) (certainty 80)))
    (assert (basic-attribute (name tourism-type) (value sea) (certainty 50))) )
    
(defrule QUESTION-INFERENCE::interest
    (preference ((name interest) (answer-value museum)))
=>
    (assert (basic-attribute (name tourism-type) (value cultural) (certainty 80)))
    (assert (basic-attribute (name tourism-type) (value natural) (certainty 50))) )
     

(defrule QUESTION-INFERENCE::change-phase
    (declare (salience -1000)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase)))
  
  
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
  (slot distance (type INTEGER) (range 0 ?VARIABLE))) 
   
  
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
    (basic-attribute ((name tourism-type) (value ?v)))
    (resort (name ?rn))
    (resort-tourism (resort-name ?rn) (tourism-type ?v) (score ?s))
=>
    (assert (basic-attribute (name resort-name) (value ?rn) (certainty (* ?s 20)))) )


(defrule RESORT::change-phase
    (declare (salience -1000)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase)))


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


(deftemplate HOTEL::resort-hotel-assotiation
   (slot resort-name)
   (slot hotel-name)
   (slot certainty (default 100.0)))  
   
;;;;;;;; FACTS ;;;;;;;;;;;;

   
;;;;;;;; RULES ;;;;;;;;;;;;


(defrule HOTEL::rate-hotel-by-stars
    (basic-attribute ((name hotel-stars) (value ?stars-v) (certainty ?c)))
    (basic-attribute ((name people-number) (value ?people-v)))
    (hotel (name ?hn) (stars ?hotel-stars) (availability ?av&:(>= ?av ?people-v)))
=>
    (assert (basic-attribute (name resort-name) (value ?rn) (certainty (* ?s 20)))) )
    
(defrule HOTEL::rate-hotel-by-cost
    (basic-attribute ((name hotel-cost) (value ?people-v) (certainty ?c)))
    (basic-attribute ((name people-number) (value ?people-v)))
    (hotel (name ?hn) (stars ?hotel-stars) (availability ?av&:(>= ?av ?people-v)))
=>
    (assert (basic-attribute (name resort-name) (value ?rn) (certainty (* ?s 20)))) )

  
(defrule HOTEL::rate-resort-hotel-assotiation
    (basic-attribute ((name resort-name) (value ?resort-v) (certainty ?resort-c))
    (basic-attribute (name hotel-name) (value ?hotel-v) (certainty ?hotel-c))
=>
    
    (assert (resort-hotel-assotiation (resort-name ?resort-v) (hotel-name ?resort-v) (certainty (min ?resort-c ?hotel-c)))) )


;;*****************
;;* MODULE TRAVEL *
;;*****************
 

(defmodule TRAVEL (export ?ALL) (import HOTEL ?ALL))
    
(deftemplate TRAVEL::travel
  (multislot resorts-to-visit )
  (slot total-cost (type INTEGER) (range 0 ?VARIABLE))
  (slot duration (type INTEGER) (range 0 ?VARIABLE)))  
  
(defrule RESORT::rate-travel
    (resort-hotel-assotiation ((name resort-name) (value ?resort-v1) (certainty ?c1))) 
    (resort (name ?rn))
=>
    (assert (attribute (name resort-name) (value ?rn) (certainty (* ?s 20)))) )
  

  
