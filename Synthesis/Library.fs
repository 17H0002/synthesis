﻿module Synthesis

open System.Security.Principal

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
    let rec funkymonkey v =
        match (v>9 || v < -9) with
        |false -> 1
        |true -> 1 + funkymonkey (v/10)
    funkymonkey x
    
     

let minmax (a,b,c,d) =
    (min (min a b) (min c d),max (max a b) (max c d))
    
    

let isLeap x =
    match x < 1582 with
    |true -> failwith "<1582"
    |false ->     match ((x % 4 = 0) && ((x % 100 <> 0) || (x % 400 = 0) )) with
                    |true -> true
                    |false -> false


let month month =
    match month with
    |1 -> ("January", 31)
    |2 -> ("February", 28)
    |3 -> ("March", 31)
    |4 -> ("April", 30)
    |5 -> ("May", 31)
    |6 -> ("June", 30)
    |7 -> ("July", 31)
    |8 -> ("August", 31)
    |9 -> ("September", 30)
    |10 -> ("October", 31)
    |11 -> ("November", 30)
    |12 -> ("December", 31)
    |_ -> failwith "style" //what black magic will lose me a line???

let toBinary x =
    match x < 0 with
       |true -> failwith "REEEEE"
       |false -> match x=0 with
                    |true -> "0"
                    |false -> let rec funkymonkey v =
                                match (v = 0) with
                                   |true -> ""
                                   |false -> match v%2=0 with
                                                |true -> funkymonkey(v/2) + "0"
                                                |false ->  funkymonkey(v/2) + "1" 
                              funkymonkey x
    

let bizFuzz x =
        let rec zebrastripes (gg,y,u,z) = match gg=1 || gg<1  with
                                            |true -> y,u,z
                                            |false -> match gg%3 = 0 with
                                                        |true -> match gg % 5 = 0 with
                                                                    |true -> zebrastripes (gg-1,y+1,u+1,z+1)
                                                                    |false -> zebrastripes (gg-1,y+1,u,z)
                                                        |false ->  match gg % 5 = 0 with 
                                                                    |true -> zebrastripes (gg-1,y,u+1,z)
                                                                    |false -> zebrastripes (gg-1,y,u,z)
        zebrastripes(x,0,0,0)



let monthDay d y =
    let rec findMonth x u = 
        match x<=(snd (month u)),isLeap y,d>365 && d < 367,u = 2,d = 60 with
            |false,true,_,true,true -> "February"
            |_,true,true,_,_ -> "December"
            |true,false,_,_,_ -> fst (month u)                                            
            |false,false,_,_,_ -> findMonth (x- snd (month u)) (u+1) 
            |false,true,_,true,_ -> findMonth (x - 29) (u+1)
            |false,true,_,_,_ -> findMonth (x- snd (month u)) (u+1)
            |true,true,_,_,_ -> fst (month u)
            |_,_,_,_,_ -> "Someone pass me another tuple, I have some patching to do..."
      //MWUHAHAHAHAHAHHAHAHAHAHAHAHAHAHAHAHHAAHHAHAHAHAHAHAHAHAHAHAHAHAHAHAHHAAHHAHAHAHAHAHAHA  
    match d > 0 && y > 1581 && ((d<=365) || (d = 366 && (isLeap y = true))) with
        |false -> failwith "somtin"
        |true -> findMonth d 1 

             
    
                                            


let coord (x,y) =
    let rec calculate guess i n =
        match i with
        |10 -> guess
        |_ -> 
            let g = (guess + n/guess) /2.0
            calculate g (i+1) n
    let sqrt n =
        match n <0.0 with
        |true -> failwith "k"
        |false -> calculate (n/2.0) 0 n
    let straightisgreat (x,y) (a,b) =
        (sqrt(((x-a)*(x-a))+((y-b)*(y-b))))
    let contains (x,y) (a,b) q w =
         x>a && y<b && x<a+q && y>b-w 

    straightisgreat

