(** Allocations: periods of time when a person is assigned to a project *)

type rate = Rate of float
(** Allocation per day, in hourse *)

type simple_allocation = 
  { start_date : CalendarLib.Date.t 
  ; days : CalendarLib.Date.Period.t (** [days >=0] must be true *)
  ; rate : rate
  }
(** A contiguous range of [days] days, including [start_date], togethre with a rate. *) 

type allocation = simple_allocation list 
                  (** Conceptually, a map from days to rates, implemented as the
                      sum of simple allocations.  *)

                  
