module Synthesis

let abelar x =
    x > 12 && x <3097 && x%12 = 0

let area x y =
    match x<0.0 || y<0.0 with
    |true -> failwith "lols" 
    |false ->  x * y * 0.5

let zollo x =
    match x<0 with
    |true -> x * -1
    |false -> x * 2

let min x y =
    match x<y with
    |true -> x
    |false -> y

let max x y =
    match x>y with
    |true -> x
    |false -> y

let ofTime a b c =
    a*60*60 + b*60 + c

let toTime x =
    match x>0 with
    |true -> x/3600, (x%3600)/60, ((x%3600)%60)
    |false -> 0,0,0  

let digits x =
    let rec REEEEEEE wow count =
    match wow = 0 with hi
     

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"