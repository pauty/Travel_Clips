
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
    ?*MAX-BUDGET-TOLERANCE* = 600
    ?*UNEVEN-DURATION-TOLERANCE* = 1
    ?*MIN-PRINT-CF* = 0.35
)

(deftemplate COMMON::iteration
    (slot number (type INTEGER))
)

(deftemplate COMMON::dv 
    (slot dv-id (default-dynamic (gensym*)))
    (multislot description)
    (multislot value)
    (slot CF (type FLOAT) (range -1.0 1.0) (default 0.0) )
    (slot basic (default FALSE))   ;; A non-basic dv must be removed at the end of iteration
)

;;---------- FACTS ------------

(deffacts COMMON::first-iteration
    (iteration (number 0))
)

;;---------- COMBINE CERTAINTIES ------------
  
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


;;---------- FACTS ------------

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
            (skippable FALSE)
            (iteration 0) 
            (valid-answers cheap normal expensive))
    (question (the-question "Do you generally prefer cool or warm places? [cool, both, warm] ")            
            (preference-topic temperature)
            (iteration 0) 
            (skippable FALSE)
            (valid-answers cool both warm))
    (question (the-question "How many people want to go on vacation? (between 1 and 10 people) ") 
            (preference-topic people-number)
            (iteration 0) 
            (skippable FALSE)
            (type range)
            (valid-answers 1 10))
    (question (the-question "How long is your ideal vacation? (between 5 and 30 days) ")
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
    (question (the-question "Would you like to go trekking, skiing, or do other mountain related activities? [yes, indifferent, no] ")
            (preference-topic mountain)
            (iteration 1) 
            (valid-answers no indifferent yes))  
    (question (the-question "Would you like to go to the beach, swim, or do other sea/lake related activities? [yes, indifferent, no] ")
            (preference-topic beach)
            (iteration 1) 
            (valid-answers no indifferent yes))  
    (question (the-question "Do you feel safe to swim in lakes? [yes, no] ")
            (preference-topic swim-in-lake)
            (iteration 1) 
            (precursors beach is yes)  
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

;;---------- FACTS ------------

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
    (resort (name Altopiano_Blu) (region Kanto))
    (resort (name Borgo_Foglianova) (region Johto))
    (resort (name Amarantopoli) (region Johto))
    (resort (name Fiorpescopoli) (region Johto))
    (resort (name Violapoli) (region Johto))
    (resort (name Azalina) (region Johto))
    (resort (name Mogania) (region Johto))
    (resort (name Fiordoropoli) (region Johto))
    (resort (name Olivinopoli) (region Johto))
    (resort (name Fiorlisopoli) (region Johto))
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
    (resort-tourism (resort-name Celestopoli) (tourism-type sea) (score 2))
    (resort-tourism (resort-name Celestopoli) (tourism-type sportive) (score 4))
    (resort-tourism (resort-name Celestopoli) (tourism-type enogastronomic) (score 3))
    (resort-tourism (resort-name Aranciopoli) (tourism-type sea) (score 4))
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
    (resort-tourism (resort-name Fiorpescopoli) (tourism-type sea) (score 4))
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
    (resort-tourism (resort-name Olivinopoli) (tourism-type sea) (score 4))
    (resort-tourism (resort-name Olivinopoli) (tourism-type enogastronomic) (score 4))
    (resort-tourism (resort-name Mogania) (tourism-type mountain) (score 3))
    (resort-tourism (resort-name Mogania) (tourism-type lake) (score 5))
    (resort-tourism (resort-name Ebanopoli) (tourism-type mountain) (score 5))
    (resort-tourism (resort-name Ebanopoli) (tourism-type thermal) (score 3))
    (resort-tourism (resort-name Fiorlisopoli) (tourism-type sea) (score 4))
    (resort-tourism (resort-name Fiorlisopoli) (tourism-type sportive) (score 4))
    (resort-tourism (resort-name Altopiano_Blu) (tourism-type mountain) (score 3))
    (resort-tourism (resort-name Altopiano_Blu) (tourism-type sportive) (score 4))
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
    (route (resort-src Fiorlisopoli) (resort-dst Olivinopoli) (distance 25))
    (route (resort-src Fiorlisopoli) (resort-dst Fiordoropoli) (distance 25))
    (route (resort-src Fiorlisopoli) (resort-dst Azalina) (distance 40))
    (route (resort-src Altopiano_Blu) (resort-dst Plumbeopoli) (distance 15))
    (route (resort-src Altopiano_Blu) (resort-dst Smeraldopoli) (distance 45))
    (route (resort-src Altopiano_Blu) (resort-dst Ebanopoli) (distance 40))
    (route (resort-src Altopiano_Blu) (resort-dst Borgo_Foglianova) (distance 60))

)

;;*****************
;;* MODULE HOTEL  *
;;*****************

(defmodule HOTEL (export ?ALL))

(deftemplate HOTEL::hotel
    (slot name (default ?NONE))
    (slot resort (default ?NONE))
    (slot stars (type INTEGER) (range 1 4))
    (slot empty (type INTEGER) (range 0 ?VARIABLE))
    (slot capacity (type INTEGER) (range 1 ?VARIABLE))
)
  
;;---------- FACTS ------------

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
    (hotel (name TravelEnd) (resort Fiorlisopoli) (stars 2) (empty 15) (capacity 50))
    (hotel (name SafariRest) (resort Fiorlisopoli) (stars 3) (empty 45) (capacity 60))
    (hotel (name ChampionInn) (resort Altopiano_Blu) (stars 3) (empty 15) (capacity 50))
)


;;*****************
;;* MODULE TRIP   *
;;*****************

(defmodule TRIP (export ?ALL))

(deftemplate TRIP::path
    (slot path-id (default-dynamic (gensym*)))
    (multislot resorts)
    (slot length (type INTEGER))
    (slot total-distance (type INTEGER))
)

(deftemplate TRIP::banned-path
    (slot path-id)
)

(deftemplate TRIP::average-resort-cf
    (slot value)
)

(deftemplate TRIP::duration
    (multislot days (type INTEGER))
    (slot length (type INTEGER))
    (slot target (type INTEGER))
)

(deftemplate TRIP::trip
    (slot trip-id (default-dynamic (gensym*)))
    (multislot resorts)
    (multislot hotels (default ND ND ND ND ND))
    (multislot days (type INTEGER))
    (multislot costs (type INTEGER) (default 0 0 0 0 0))
    (slot length (type INTEGER))
)
