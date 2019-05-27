;;****************
;;* MODULE MAIN  *
;;****************


(defmodule MAIN (export ?ALL))
  
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
(phase-sequence QUESTIONS QUESTION-INFERENCE RESORT HOTEL TRAVEL)
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
    ?fact1 <- (dv (description $?d) (value ?v) (CF ?C1&:(>= ?C1 0.0)))
    ?fact2 <- (dv (description $?d) (value ?v) (CF ?C2&:(>= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (- (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (CF ?C3))
)
    
(defrule MAIN::combine-certainties-both-negative
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description $?d) (value ?v) (CF ?C1&:(< ?C1 0.0)))
    ?fact2 <- (dv (description $?d) (value ?v) (CF ?C2&:(< ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (+ (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (CF ?C3))
)

(defrule MAIN::combine-certainties-negative-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (dv (description $?d) (value ?v) (CF ?C1))
    ?fact2 <- (dv (description $?d) (value ?v) (CF ?C2))
    (test (neq ?fact1 ?fact2))
    (test (< 0 (* ?C1 ?C2)))
=>
    (retract ?fact1)
    (bind ?C3 (/ (+ ?C1 ?C2) (- 1 (min (abs ?C1) (abs ?C2) ))))
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

(deftemplate MAIN::preference
   (slot topic)
   (slot answer-value)
)

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
   ?fact <- (question (already-asked FALSE)
                   (precursors)
                   (the-question ?q)
                   (preference-topic ?pt)
                   (valid-answers $?va))
   =>
   (modify ?fact (already-asked TRUE))
   (assert (preference (topic ?pt)
                       (answer-value (ask-question ?q ?va))))
   (assert (already-answered))
)
   

(defrule QUESTIONS::change-phase
    ?aa <- (already-answered)
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
    (retract ?aa)
)

;;*****************************
;;* MODULE QUESTION-INFERENCE *
;;*****************************
 

(defmodule QUESTION-INFERENCE (export ?ALL) (import QUESTIONS ?ALL))

(defrule QUESTION-INFERENCE::temperature1
    (preference (topic temperature) (answer-value hot))
=>
    (assert (dv (description tourism-type-is) (value sea) (CF 0.8)))
    (assert (dv (description tourism-type-is) (value mountain) (CF 0.5))) 
)
    
(defrule QUESTION-INFERENCE::temperature2
    (preference (topic temperature) (answer-value cool))
=>
    (assert (dv (description tourism-type-is) (value mountain) (CF 0.8)))
    (assert (dv (description tourism-type-is) (value sea) (CF 0.5))) 
)
    
(defrule QUESTION-INFERENCE::culture
    (preference (topic culture) (answer-value yes))
=>
    (assert (dv (description tourism-type-is) (value cultural) (CF 0.8)))
    (assert (dv (description tourism-type-is) (value religious) (CF 0.5))) 
)

(defrule QUESTION-INFERENCE::cost1
    (preference (topic cost) (answer-value cheap))
=>
    (assert (dv-numeric (description how-many-hotel-stars) (value 1) (CF 0.8)))
    (assert (dv-numeric (description tourism-type-is) (value 2) (CF 0.5))) 
)

(defrule QUESTION-INFERENCE::cost2
    (preference (topic cost) (answer-value normal))
=>
    (assert (dv-numeric (description how-many-hotel-stars) (value 2) (CF 0.8)))
)

(defrule QUESTION-INFERENCE::cost3
    (preference (topic cost) (answer-value expensive))
=>
    (assert (dv-numeric (description how-many-hotel-stars) (value 3) (CF 0.8)))
    (assert (dv-numeric (description how-many-hotel-stars) (value 4) (CF 0.5)))
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
  
(defmodule RESORT (export ?ALL) (import QUESTION-INFERENCE ?ALL))
  
;;;;;;;;; TEMPLATES ;;;;;;;;; 

(deftemplate RESORT::resort
  (slot name  (default ?NONE))
  (slot region (default any))
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
   
  
;;;;;;;;; FACTS ;;;;;;;;; 
  
(deffacts RESORT::resort-list 
  (resort (name BiancaVilla) (region Kanto))
  (resort (name Lavandonia) (region Kanto))
  (resort (name MonteFatuo) (region Kanto)))
  
(deffacts RESORT::resort-tourism-list 
  (resort-tourism (resort-name BiancaVilla) (tourism-type sea) (score 4))
  (resort-tourism (resort-name BiancaVilla) (tourism-type mountain) (score 1))
  (resort-tourism (resort-name BiancaVilla) (tourism-type cultural) (score 3))
  (resort-tourism (resort-name Lavandonia) (tourism-type mare) (score 3))
  (resort-tourism (resort-name Lavandonia) (tourism-type religious) (score 3))
  (resort-tourism (resort-name MonteFatuo) (tourism-type cultural) (score 3))
  (resort-tourism (resort-name MonteFatuo) (tourism-type mountain) (score 4))
)

(deffacts RESORTS:route-list
    (route (resort-src BiancaVilla) (resort-dst MonteFatuo) (distance 30))
    (route (resort-src MonteFatuo) (resort-dst Lavandonia) (distance 10))
    (route (resort-src Lavandonia) (resort-dst BiancaVilla) (distance 20))
)
  
;;;;;;;;; RULES ;;;;;;;;; 

(defrule RESORT::rate-resort
    (dv (description tourism-type-is) (value ?v))
    (resort (name ?rn))
    (resort-tourism (resort-name ?rn) (tourism-type ?v) (score ?s))
=>
    (assert (dv (description the-resort) (value ?rn) (CF (* ?s 0.2))))
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

(deffacts HOTEL::hotel-list
  (hotel (name YesHotel) (resort BiancaVilla) (stars 3) (availability 10))
  (hotel (name BhaHotel) (resort BiancaVilla) (stars 1) (availability 10))
  (hotel (name MammaHotel) (resort Lavandonia) (stars 4) (availability 10))
  (hotel (name BubbaHotel) (resort Lavandonia) (stars 3) (availability 10))
  (hotel (name FuocoHotel) (resort MonteFatuo) (stars 3) (availability 10))
  (hotel (name MerdaHotel) (resort MonteFatuo) (stars 1) (availability 10))
)
   
;;;;;;;; RULES ;;;;;;;;;;;;

(defrule HOTEL::rate-all-hotel
    (hotel (name ?hn) (resort ?r))
=>
    (assert (dv (description the-hotel-in ?r) (value ?hn) (CF 0.0)))
)


(defrule HOTEL::rate-hotel-by-stars
    (dv-numeric (description how-many-hotel-stars) (value ?s) (CF ?c))
    (dv-numeric (description how-many-people) (value ?p))
    (hotel (name ?hn) (resort ?rn) (stars ?hs) (availability ?a&:(>= ?a ?p)))
=>
    (assert (dv (description the-hotel-in ?rn) (value ?hn) (CF (* ?s 0.2))))
)
  


;;*****************
;;* MODULE TRAVEL *
;;*****************
 

(defmodule TRAVEL (export ?ALL) (import MAIN ?ALL) (import RESORT ?ALL) (import HOTEL ?ALL))
 
 
(deftemplate TRAVEL::travel
  (multislot resorts-to-visit )
  (slot total-cost (type INTEGER) (range 0 ?VARIABLE))
  (slot duration (type INTEGER) (range 0 ?VARIABLE)))  
  
(defrule TRAVEL::rate-travel
    (dv (description the-path-is) (value $?rs ?lr) (CF ?cf-path))
    (dv (description the-resort) (value ?nr) (CF ?cf-resort))
    (dv (description the-hotel-in ?nr) (value ?h) (CF ?cf-hotel))
    (not (dv (description the-hotel-in ?nr) (value ?h2) (CF ?cf-hotel2&:(> ?cf-hotel2 ?cf-hotel))))
    (route (resort-src ?lc) (resort-dst ?nc) (distance ?d&:(< ?d 100)))
    (dv (description the-path-is) (value $?old-path))
    (test (neq (subsetp (create$ $?old-path) (create$ $?rs ?lr ?nr)) TRUE))
    (test (neq (subsetp (create$ $?rs ?lr ?nr) (create$ $?old-path)) TRUE)) 
    ;;(not (and (subsetp (create$ $?old-path) (create$ $?rs ?lr ?nr)) (subsetp (create$ $?rs ?lr ?nr) ($?old-path)))) 
=>
    (bind ?new-cf (min ?cf-path ?cf-hotel ?cf-resort))
    (assert (dv (description the-path-is) (value $?rs ?lr ?nr) (CF ?new-cf))) 
)  

(defrule TRAVEL::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
)
  

  
