;; Developer Details Template ;;

(deftemplate developer-details
    (slot first-name)
    (slot last-name)
    (slot months-of-experience (default 0))
    (slot leadership-hours (default 0)))

;; Functions ;;
(deffunction ask-question (?question)
   (printout t ?question)
   (bind ?answer (read)))

(deffunction ask-restricted-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
    
   (while (not (member$ ?answer ?allowed-values)) do
        (printout t "Answer must be any of these: " ?allowed-values ". Please try again " crlf)
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer) 
            then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-q (?question)
   (bind ?response (ask-restricted-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
        then yes 
    else no))

(deffunction ask-value-question (?question)
    (printout t ?question)
    (bind ?answer (read))
    (while (not (eq (type ?answer) INTEGER)) do
        (printout t "--- Answer must be a whole number. Please try again -- " crlf)
        (printout t ?question)
        (bind ?answer (read)))
    ?answer)

(defrule get-dev-details "Get developer details"
    (not (developer-level ?))
   =>
    (assert
        (developer-details 
            (first-name (ask-question "What is the developer's first name? "))
            (last-name (ask-question "What is the developer's last name? "))
            (months-of-experience (ask-value-question "How many months of experience does this developer have? "))
            (leadership-hours (ask-value-question "How many leadership hours does this developer have? ")))
    ))
    
(defrule print-dev-details "Print developer details"
    (developer-details
        (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    =>
    (printout t crlf "Details obtained." crlf)
    (printout t "---------------------------------------" crlf)
    (printout t "First Name: " ?first-name crlf)
    (printout t "Last Name: " ?last-name crlf)
    (printout t "Months of Experience: " ?experience crlf)
    (printout t "Leadership Hours: " ?hours crlf)
    (printout t "---------------------------------------" crlf))

(defrule system-banner ""
    (declare (salience 10))
    =>
    (printout t crlf)
    (printout t "------------------------------------------------------" crlf)
    (printout t "        Developer Level Expert System" crlf)
    (printout t "------------------------------------------------------"  crlf)
    (printout t "This system is designed to help the managers at Devnations easily know the level of the developers within the company."  crlf)
    (printout t "Answer the questions provided "  crlf)
    (printout t "------------------------------------------------------")
    (printout t crlf crlf))



(defrule check-dev-level-0
    (not (developer-level ?))
    (developer-details (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    (test (eq ?experience 0))
    =>
    (assert (done))
    (assert (developer-level ?first-name ?last-name dev-level-0 "a pre-entry level developer")))

(defrule check-dev-level-1
    (not (developer-level ?))
    (developer-details (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    (test (> ?experience 0))
    (test (< ?experience 10))
    =>
    (assert (done))
    (assert (developer-level ?first-name ?last-name dev-level-1 "an entry level developer")))


(defrule check-dev-level-2
    (not (developer-level ?))
    (developer-details (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    (test (>= ?experience 10))
    (test (< ?experience 20))
    (test (>= ?hours 40))
    =>
    (assert (done))
    (assert (developer-level ?first-name ?last-name dev-level-2 "a mid level developer")))

(defrule check-dev-level-3
    (not (developer-level ?))
    (developer-details (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    (test (>= ?experience 20))
    (test (< ?experience 40))
    (test (>= ?hours 90))
    =>
    (assert (done))
    (assert (developer-level ?first-name ?last-name dev-level-3 "a senior level developer")))

(defrule check-dev-level-4
    (not (developer-level ?))
    (developer-details (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    (test (> ?experience 40))
    (test (>= ?hours 150))
    =>
    (assert (done))
    (assert (developer-level ?first-name ?last-name dev-level-4 "a top level developer")))

(defrule check-dev-level-unknown
    (not (developer-level ?))
    (not (done))
    (developer-details (first-name ?first-name)
        (last-name ?last-name)
        (months-of-experience ?experience)
        (leadership-hours ?hours))
    =>
    (assert (developer-level ?first-name ?last-name unknown "The developer's details do not align with any level")))

(defrule print-result ""
    (declare (salience 10))
    (developer-level ?first-name ?last-name ?dev-level ?level-definition)
    =>
    (printout t crlf crlf)
    (printout t "---------------------RESULT--------------------")
    (printout t crlf crlf)
    (printout t "The level of the developer " ?first-name " " ?last-name " is " ?dev-level ": " ?level-definition "." crlf))