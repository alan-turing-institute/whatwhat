(* A module for dealing with allocations *)

type rate = Rate of float

type simple_allocation = 
  { start_date : CalendarLib.Date.t 
  ; days : CalendarLib.Date.Period.t 
  ; rate : rate
  }

type allocation = simple_allocation list
