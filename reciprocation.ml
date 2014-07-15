
"given a key and preference list, find the item in the preference list with
 the key and return its value"
let rec find_agent_helper k pref =
  match pref with
    |(agent, value)::t -> if (String.compare agent k) = 0 then value
      else find_agent_helper k t
    | []-> failwith "this shouldn't happen"

"find agent's value for k"
let rec find_agent_value k agent many =
  match many with
    |(agent', pref)::t -> 
       if (String.compare agent agent') = 0 
       then find_agent_helper k pref
       else find_agent_value k agent t
    |[]-> failwith "this shouldn't happen either"

"return the new key, preference tuple for a key"
let rec one_to_many (k, pref) many tl = 
  match pref with
    |(agent, value)::t -> 
       let value' = (find_agent_value k agent many) + value 
       in one_to_many (k, t) many ((agent, value')::tl)
    |[] ->(k, tl)

let rec new_preferences_helper a e tl =
  match a with 
    | h::t -> new_preferences_helper t e ((one_to_many h e [])::tl)
    | [] -> tl

let new_preferences a e =
  new_preferences_helper a e []

let rec reciprocation a e cycles = 
  if cycles > 0 
  then 
    let a' = new_preferences a e
    and e' = new_preferences e a
    in reciprocation a' e' (cycles - 1)
  else (a, e)

let rec reciprocation_ray a e cycles =
  if cycles > 0
  then 
    let a' = new_preferences a e
    in let e' = new_preferences e a'
    in reciprocation_ray a' e' (cycles - 1)
  else (a, e)









