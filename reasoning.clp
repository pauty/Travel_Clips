
;;****************
;;* MODULE MAIN  *
;;****************

(deffacts MAIN::define-phase-sequence
    (phase-sequence ASK-QUESTION QUESTION-INFERENCE INIT RATE-RESORT RATE-HOTEL BUILD-AND-RATE-TRIP PRINT-RESULTS INVALIDATE)
)

(defrule MAIN::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (focus ?next-phase)
    (retract ?list)
    (assert (phase-sequence ?other-phases ?next-phase))
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
    (iteration (number ?i))
    ?f <- (question (precursors ?t is ?v $?rest))
    (preference (topic ?t) (answer-value ?v))
=> 
    (modify ?f (iteration ?i) (precursors ?rest))
)

(defrule ASK-QUESTION::print-welcome-message
    (declare (salience 500))
    (iteration (number 0))
=>
    (printout t crlf)
    (printout t "            ***************************************" crlf )
    (printout t "            *             TRAVEL EXPERT           *" crlf )
    (printout t "            ***************************************" crlf crlf)
    (printout t "Welcome to our expert system. We will ask you some questions and try to suggest you the best trip." crlf)
    (printout t "If you want to skip a question, leave it blank. Please note that some questions cannot be skipped." crlf)
    (printout t "      _____________________________________________________" crlf crlf)
)

(defrule ASK-QUESTION::ask-the-end-question
    (declare (salience 500))
    (iteration (number ?i))
    (test (> ?i 0))
=>
    (bind ?q "Are you happy with one of the suggested trips? [yes, no] ")
    (bind ?va (create$ yes no))
    (bind ?answer (nth 1 (ask-question closed FALSE ?q ?va)))
    (if (eq ?answer yes) then 
        (printout t "Thank you for using our expert system. Have a good vacation!" crlf crlf)
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

;;------------ MOUNTAIN ------------

(defrule QUESTION-INFERENCE::mountain-yes
    (preference (topic mountain) (answer-value yes))
=>
    (assert (dv (description the-tourism-type) (value mountain) (CF 0.5) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::mountain-no
    (preference (topic mountain) (answer-value no))
=>
    (assert (dv (description the-tourism-type) (value mountain) (CF -0.1) (basic TRUE)))
)

;;------------ BEACH ------------

(defrule QUESTION-INFERENCE::beach-yes
    (preference (topic beach) (answer-value yes))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF 0.5) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF 0.5) (basic TRUE)))
)

(defrule QUESTION-INFERENCE::beach-no
    (preference (topic beach) (answer-value no))
=>
    (assert (dv (description the-tourism-type) (value sea) (CF -0.1) (basic TRUE)))
    (assert (dv (description the-tourism-type) (value lake) (CF -0.1) (basic TRUE)))
)

;;------------ SWIM-IN-LAKE ------------

(defrule QUESTION-INFERENCE::swim-in-lake-no
    (preference (topic swim-in-lake) (answer-value no))
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

;;------------ BAN RESORT ------------

(defrule QUESTION-INFERENCE::ban-resort
    (preference (topic ban-resort) (answer-value ?r))
=>
    (assert (dv (description the-banned-resort) (value ?r) (CF 1.0) (basic TRUE)))
)

;;------------ BAN REGION ------------

(defrule QUESTION-INFERENCE::ban-region
    (preference (topic ban-region) (answer-value ?r))
=>
    (assert (dv (description the-banned-region) (value ?r) (CF 1.0) (basic TRUE)))
)

;;------------ FAVOR RESORT ------------

(defrule QUESTION-INFERENCE::favor-resort
    (preference (topic favor-resort) (answer-value ?r))
=>
    (assert (dv (description the-favourite-resort) (value ?r) (CF 1.0) (basic TRUE)))
)

;;------------ FAVOR REGION ------------

(defrule QUESTION-INFERENCE::favor-region
    (preference (topic favor-region) (answer-value ?r))
=>
    (assert (dv (description the-favourite-region) (value ?r) (CF 1.0) (basic TRUE)))
)

  
;;****************
;;* MODULE INIT  *
;;****************

(defmodule INIT  (import COMMON ?ALL) (import RESORT ?ALL) (import HOTEL ?ALL) (import TRIP ?ALL))  

;;---------- ROUTES ------------

(defrule INIT::build-symmetric-route
    (route (resort-src ?r1) (resort-dst ?r2) (distance ?d))
    (not (route (resort-src ?r2) (resort-dst ?r1)))
=>
    (assert (route (resort-src ?r2) (resort-dst ?r1) (distance ?d)))
)

(defrule INIT::rate-route
    (route (resort-src ?src) (resort-dst ?dst) (distance ?d))
    (dv (description the-max-route-distance) (value ?v))
=>
    (bind ?rcf (min 0.1 (max -0.9 (/ (- ?v ?d) ?*MAX-ROUTE-DISTANCE-TOLERANCE*)))) 
    (assert (dv (description use-route) (value ?src ?dst) (CF ?rcf) (basic TRUE)))
)

;;---------- PATHS ------------

(defrule INIT::build-singleton-path
    (resort (name ?r))
=>
    (assert (path (resorts ?r) (length 1) (total-distance 0)))
)

(defrule INIT::build-path
    (path (resorts $?rs ?lr) (length ?len) (total-distance ?td))
    (dv (description the-trip-length) (value ?tl))
    (test (< ?len (min (+ ?tl 1) ?*MAX-TRIP-LENGTH*)))
    (route (resort-src ?lr) (resort-dst ?nr) (distance ?d)) 
    (test (eq (member$ ?nr (create$ ?rs ?lr)) FALSE))
=>
    (assert (path (resorts ?rs ?lr ?nr) (length (+ ?len 1)) (total-distance (+ ?td ?d))))
)

(defrule INIT::remove-suboptimal-distance-path
    (declare (salience 100))
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

;;---------- DURATIONS ------------

(defrule INIT::base-duration
    (dv (description the-trip-duration) (value ?d))
    (dv (description the-trip-length) (value ?l))
=>
    (bind ?dur (div ?d ?l))
    (assert (duration (days ?dur) (length 1) (target ?l)))
    (if (> ?l 1) then
        (bind ?tl (- ?l 1))
        (bind ?dur (div ?d ?tl))
        (assert (duration (days ?dur) (length 1) (target ?tl)))
    )
    (if (< ?l ?*MAX-TRIP-LENGTH*) then
        (bind ?tl (+ ?l 1))
        (bind ?dur (div ?d ?tl))
        (assert (duration (days ?dur) (length 1) (target ?tl)))
    )
)

(defrule INIT::complete-base-durations
    ?fact <- (duration (days $?ds ?d) (length ?l) (target ?t&:(< ?l ?t)))
=>
    (modify ?fact (days ?ds ?d ?d) (length (+ ?l 1)))
)

(defrule INIT::increment-invalid-duration
    (duration (days $?dl ?d $?dr) (length ?l) (target ?t))
    (test (eq ?t ?l))
    (dv (description the-trip-duration) (value ?td))
    (test (< (+ (expand$ (create$ ?dl ?d ?dr)) 0) ?td))
=>
    (assert (duration (days ?dl (+ ?d 1) ?dr) (length ?l) (target ?l)))
)

(defrule INIT::remove-invalid-duration
    (declare (salience -200))
    (dv (description the-trip-duration) (value ?d))
    ?fact <- (duration (days $?ds))
    (test (< (+ (expand$ (create$ ?ds)) 0) ?d))
=>
    (retract ?fact)  
)

(defrule INIT::remove-unbalanced-duration
    (declare (salience -200))
    ?fact <- (duration (days $?dl ?d1 $?dc ?d2 $?dr))
    (test (> (abs (- ?d1 ?d2)) ?*UNEVEN-DURATION-TOLERANCE*))
=>
    (retract ?fact)  
)


;;**********************
;;* MODULE RATE-RESORT *
;;**********************
  
(defmodule RATE-RESORT (import COMMON ?ALL) (import RESORT ?ALL))

(defrule RATE-RESORT::rate-resort-by-tourism-type
    (iteration (number ?i))
    (dv (description the-tourism-type) (value ?t) (CF ?cf))
    (resort (name ?r))
    (resort-tourism (resort-name ?r) (tourism-type ?t) (score ?s))
=>
    (bind ?rcf (/ (* ?s ?cf) ?*MAX-TOURISM-SCORE*))
    (assert (dv (description the-resort) (value ?r) (CF ?rcf)))
)

(defrule RATE-RESORT::rate-resort-by-lack-of-interest
    (iteration (number ?i))
    (resort (name ?r))  
    (not (and (resort-tourism (resort-name ?r) (tourism-type ?t) (score ?s))
              (dv (description the-tourism-type) (value ?t) (CF ?cf&:(> ?cf 0)))))
=>
    (assert (dv (description the-resort) (value ?r) (CF -0.3)))
)

(defrule RATE-RESORT::rate-resort-by-banned-resorts
    (iteration (number ?i))
    (dv (description the-banned-resort) (value ?r))
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF -0.8)))
)

(defrule RATE-RESORT::rate-resort-by-banned-regions
    (iteration (number ?i))
    (dv (description the-banned-region) (value ?rg))
    (resort (name ?r) (region ?rg))
=>
    (assert (dv (description the-resort) (value ?r) (CF -0.5)))
)

(defrule RATE-RESORT::rate-resort-by-favourite-resorts
    (iteration (number ?i))
    (dv (description the-favourite-resort) (value ?r))
    (resort (name ?r))
=>
    (assert (dv (description the-resort) (value ?r) (CF 0.8)))
)

(defrule RATE-RESORT::rate-resort-by-favourite-regions
    (iteration (number ?i))
    (dv (description the-favourite-region) (value ?rg))
    (resort (name ?r) (region ?rg))
=>
    (assert (dv (description the-resort) (value ?r) (CF 0.5)))
)

;;*********************
;;* MODULE RATE-HOTEL *
;;*********************

(defmodule RATE-HOTEL (import COMMON ?ALL) (import HOTEL ?ALL))

(defrule RATE-HOTEL::rate-hotel-by-stars
    (iteration (number ?i))
    (dv (description the-optimal-hotel-stars) (value ?s) (CF ?cf))
    (hotel (name ?h) (resort ?r) (stars ?s))
=>
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF ?cf)))
)

(defrule RATE-HOTEL::rate-hotel-by-availability
    (iteration (number ?i))
    (dv (description the-people-number) (value ?p))
    (hotel (name ?h) (resort ?r) (empty ?e&:(> ?e ?p)) (capacity ?c))
=>
    (bind ?new-cf (* 0.4 (/ ?e ?c)))
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF ?new-cf)))
)

(defrule RATE-HOTEL::rate-hotel-by-availability-full
    (iteration (number ?i))
    (dv (description the-people-number) (value ?p))
    (hotel (name ?h) (resort ?r) (empty ?e&:(< ?e ?p)))
=>
    (assert (dv (description the-hotel-in ?r) (value ?h) (CF -1.0)))
)


;;******************************
;;* MODULE BUILD-AND-RATE-TRIP *
;;******************************

(defmodule BUILD-AND-RATE-TRIP (import COMMON ?ALL) (import HOTEL ?ALL) (import TRIP ?ALL))

;;---------- RULES FOR BUILDING TRIPS ------------

(defrule BUILD-AND-RATE-TRIP::compute-average-resort-cf
    (declare (salience 500))
    (iteration (number ?i))
=>
    (bind ?sum 0)
    (bind ?count 0)
    (do-for-all-facts ((?f dv)) (eq ?f:description (create$ the-resort))
        (bind ?sum (+ ?sum ?f:CF))
        (bind ?count (+ ?count 1)))
    (printout t "average: " (/ ?sum ?count) crlf)
    (assert (average-resort-cf (value (/ ?sum ?count))))
)

(defrule BUILD-AND-RATE-TRIP::path-pruning-strict
    (declare (salience 400))
    (average-resort-cf (value ?a))
    (path (path-id ?id) (resorts $?rl ?r $?rr))
    (or (dv (description the-resort) (value ?r) (CF ?cfr&:(< ?cfr ?a)))
        (not (dv (description the-hotel-in ?r) (CF ?cfh&:(>= ?cfh 0.2)))))
=>
    (assert (banned-path (path-id ?id)))
)

;;(defrule BUILD-AND-RATE-TRIP::path-pruning-loose
;;    (declare (salience 400))
;;    (average-resort-cf (value ?a))
;;    (path (path-id ?id) (resorts $?rl ?r1 $?rm ?r2 $?rr))
;;    (or (dv (description the-resort) (value ?r1) (CF ?cfr1&:(< ?cfr1 ?a)))
;;        (not (dv (description the-hotel-in ?r1) (CF ?cfh1&:(>= ?cfh1 0.2)))))
;;    (or (dv (description the-resort) (value ?r2) (CF ?cfr2&:(< ?cfr2 ?a)))
;;        (not (dv (description the-hotel-in ?r1) (CF ?cfh2&:(>= ?cfh2 0.2)))))
;;=>
;;    (assert (banned-path (path-id ?id)))
;;)

(defrule BUILD-AND-RATE-TRIP::build-trip
    (declare (salience 300))
    (iteration (number ?i))
    (path (path-id ?id) (resorts $?rs) (length ?len))
    (not (banned-path (path-id ?id)))
    (duration (days $?ds) (length ?len))
=>
    (assert (trip (resorts ?rs) (days ?ds) (length ?len)))
)

(defrule BUILD-AND-RATE-TRIP::fill-trip-hotels-and-costs
    (declare (salience 200))
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


;;---------- RULES FOR RATING TRIPS ------------

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-resorts
    (trip (trip-id ?id) (resorts $?rl ?r $?rr) (days $?ds) (length ?len))
    (dv (description the-resort) (value ?r) (CF ?rcf))
    (dv (description the-trip-duration) (value ?td))
=> 
    (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?d (nth ?index ?ds))
    (bind ?tcf (/ (* ?d ?rcf) ?td))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-hotels 
    (trip (trip-id ?id) (resorts $?rl ?r $?rr) (hotels $?hs) (days $?ds) (length ?len))
    (dv (description the-hotel-in ?r) (value ?h) (CF ?hcf))
    (test (eq ?h (nth (member$ ?r (create$ ?rl ?r ?rr)) ?hs))) 
    (dv (description the-trip-duration) (value ?td))
=>  
    (bind ?index (member$ ?r (create$ ?rl ?r ?rr)))
    (bind ?d (nth ?index ?ds))
    (bind ?tcf (/ (* ?d ?hcf) ?td))
    (assert (dv (description the-trip) (value ?id) (CF ?tcf)))
)

(defrule BUILD-AND-RATE-TRIP::rate-trip-by-routes
    (trip (trip-id ?id) (resorts $?rl ?rs ?rd $?rr) (length ?len))
    (dv (description use-route) (value ?rs ?rd) (CF ?rcf))
=>  
    (bind ?tcf (/ ?rcf (- ?len 1)))       ;;len resorts imply len-1 routes
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
    (bind ?tcf (min 0.1 (max -0.8 (/ (- ?b ?total-cost) ?*MAX-BUDGET-TOLERANCE*))))   
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

(defrule PRINT-RESULTS::results-header
   (declare (salience 500))
   (iteration (number ?i))
=>
   (printout t  crlf crlf)
   (printout t " >>>>>>>>>>>>>>>   SELECTED TRIPS (ITERATION " (+ ?i 1) ")  <<<<<<<<<<<<<<<"  crlf)
   (printout t  crlf)
   (assert (printed-trips 0))
)
   

(defrule PRINT-RESULTS::print-and-remove-best-trip 
  ?fact1 <- (printed-trips ?p)
  (test (< ?p 5))
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
  (printout t " Trip suggestion " (+ ?p 1) " with certainty: " (/ (round (* ?tcf 1000)) 10) "%" crlf)
  (printout t "  - Resorts to visit: " ?rs crlf)
  (printout t "  - Hotels: " (subseq$ ?hs 1 ?len ) crlf)
  (printout t "  - Days partitioning: " ?ds crlf)
  (printout t "  - Daily costs: " (subseq$ ?cs 1 ?len ) "  |  Total cost: " ?total-cost crlf) 
  (printout t  crlf)
  (printout t "      _____________________________________________________" crlf)
  (printout t  crlf)
) 

(defrule PRINT-RESULTS::test-no-good-trip
    (declare (salience -500))
    (printed-trips 0)
=>
    (printout t " --- Sorry, no trip compatible with your requests was found. ---" crlf crlf)
)

(defrule PRINT-RESULTS::on-exit
    (declare (salience -1000))
    ?fact <- (printed-trips ?p)
=>
    (retract ?fact)
    (pop-focus)
)


;;*********************
;;* MODULE INVALIDATE *
;;*********************

(defmodule INVALIDATE (import COMMON ?ALL) (import TRIP ?ALL))

;;--------- REMOVE DERIVED (NON-BASIC) DV ---------

(defrule INVALIDATE::remove-derived-dv
    ?fact <- (dv (basic FALSE))
=>  
    (retract ?fact)
)

;;--------- REMOVE TRIPS AND RELATED INFORMATION ---------

(defrule INVALIDATE::remove-trip
    ?t <- (trip)
=>
    (retract ?t)
)

(defrule INVALIDATE::remove-average-resort-cf
    ?f <- (average-resort-cf)
=>
    (retract ?f)
)

(defrule INVALIDATE::remove-banned-path
    ?f <- (banned-path)
=>
    (retract ?f)
)

(defrule INVALIDATE::on-exit
    (declare (salience -1000))
    ?fact <- (iteration (number ?i))
=>
    (retract ?fact)
    (assert (iteration (number (+ ?i 1))))  ;;increment iteration number
    (pop-focus)
)

(defrule INVALIDATE::plsstop
    (declare (salience 400))
    (iteration (number ?i))
=>
    (halt)
) 

