(defclass TRIP
    (is-a USER)
    (role concrete)
    (slot first_step)
)

(defmessage-handler TRIP get-total_cost ()
    (bind ?result 0)
    (bind ?x ?self:first_step)
    (while (neq (send ?x get-next_step) nil) do
        (bind ?result (+ ?result (send ?x get-cost)))
        (bind ?destination_lodging_stay (send ?x get-destination_lodging_stay))
        (if (neq ?destination_lodging_stay nil) then
            (bind ?result (+ ?result (send ?destination_lodging_stay get-cost)))
            (bind ?x (send ?x get-next_step))
        )
    )
    (return (+ ?result (send ?x get-cost)))
)

(defclass TRIP_PART
    (is-a USER)
    (role concrete)
    (multislot begin_date (visibility public))
    (multislot end_date (visibility public))
    (slot cost (visibility public))
)

(defclass LODGING_STAY
    (is-a TRIP_PART)
    (slot arriving_travel_step)
    (slot departing_travel_step)
)

(defmessage-handler LODGING_STAY get-place ()
    (return (send ?self:arriving_travel_step get-destination))
)

(defclass TRAVEL_STEP
    (is-a TRIP_PART)
    (slot means (default airplane))
    (slot destination)
    (slot next_step)
    (slot previous_step)
    (slot origin_lodging_stay)
    (slot destination_lodging_stay)
)

(defmessage-handler TRAVEL_STEP get-origin ()
    (if (eq ?self:previous_step nil) then
        (return newark)
    else
        (return (send ?self:previous_step get-destination))
    )
)

(defmessage-handler TRAVEL_STEP put-previous_step after (?prev)
    (if (neq ?prev nil) then
        (if (neq ?self:begin_date (send ?prev get-end_date)) then
            (bind ?lodging_stay (make-instance of LODGING_STAY
                    (begin_date (send ?prev get-begin_date))
                    (end_date ?self:end_date)
                    (arriving_travel_step ?prev)
                    (departing_travel_step ?self)
                    (cost 124.75)
                )
            )
            (send ?self put-origin_lodging_stay ?lodging_stay)
            (send ?prev put-next_step ?self)
            (send ?prev put-destination_lodging_stay ?lodging_stay)
        )
    )
)

(defmessage-handler TRAVEL_STEP get-self ()
    (return ?self)
)
