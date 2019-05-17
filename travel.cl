(defmodule MAIN (export ?ALL))

  
(deftemplate MAIN::attribute
   (slot name)
   (slot value)
   (slot certainty (default 100.0)))
   
(deftemplate MAIN::preference
   (slot attribute-name)
   (slot answer-value))


(deffacts MAIN::control-information
(phase-sequence QUESTIONS INFERENCE SHOW))

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
  ?rem1 <- (attribute (name ?rel) (value ?val) (certainty ?per1))
  ?rem2 <- (attribute (name ?rel) (value ?val) (certainty ?per2))
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


(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate QUESTIONS::question
   (slot attribute (default ?NONE))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE)))
   

(deffacts QUESTIONS::question-attributes
  (question (attribute main-component)
            (the-question "Is the main component of the meal meat, fish, or poultry? ")
            (valid-answers meat fish poultry unknown))
) 

(defrule QUESTIONS::ask-a-question
   (not (already-answered))
   ?f <- (question (already-asked FALSE)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (preference (attribute-name ?the-attribute)
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
    

(defmodule INFERENCE)

 

(defrule INFERENCE::temperature
    (preference ((attribute-name ?n) (answer-value hot)))
=>
    (assert (attribute (name ?n) (value mare) (certainty 80))) )
    
    
 
(defrule INFERENCE::interest
    (preference ((attribute-name ?n) (answer-value museum)))
    (resort (name ?rn))
    (resort-quality (resort-name ?rn) (quality culture) (score ?s))
=>
    (assert (attribute (name ?n) (value ?rn) (certainty 80))) )
    
   

(defrule INFERENCE::change-phase
    (declare (salience -1000)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase)))
  
(defmodule RESORT (export ?ALL))
  
(deftemplate RESORT::resort
  (slot name (type STRING) (default ?NONE))
  (slot (type STRING) region (default any))
  
(deftemplate RESORT::resort-quality
  (slot city-name  (default ?NONE))
  (slot quality (default ?NONE))
  (slot score (type INTEGER) (range 0 10))) 
  
(deffacts RESORT::resort-list 
  (resort (name BiancaVilla) (region Kanto))
  (resort (name Lavandonia) (region Kanto))
  (resort (name MonteFatuo) (region Kanto)))
  
(deffacts RESORT::quality-list 
  (resort-quality (resort-name BiancaVilla) (quality mare) (score 7))
  (resort-quality (resort-name BiancaVilla) (quality montagna) (score 4))
  (resort-quality (resort-name Lavandonia) (quality mare) (score 6))
  (resort-quality (resort-name MonteFatuo) (quality culo) (score 7)))

  
(deftemplate RESORT::route
  (slot resort-src (default ?NONE))
  (slot resort-dst (default ?NONE))
  (slot distance (type INTEGER) (range 0 ?VARIABLE))) 

(defmodule HOTEL (export ?ALL))

(deftemplate HOTEL::hotel
  (slot name (default ?NONE))
  (slot stars (type INTEGER) (range 1 4))
  (slot cost (type INTEGER) (range 0 ?VARIABLE))
  (slot available (type INTEGER) (range 0 ?VARIABLE)))
  
  
(deftemplate INFERENCE::suggestion
  (multislot places)
  (slot total-cost (type INTEGER) (range 0 ?VARIABLE))
  (slot duration (type INTEGER) (range 0 ?VARIABLE)))
  

  
