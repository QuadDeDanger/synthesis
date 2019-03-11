module Synthesis

let abelar a =
 match a>12 && a<3097 && a%12=0 with 
  |true->true
  |false->false
    

let area b h  =
 match b<0.0 || h<0.0 with
  |true-> failwith "Not implemented"
  |_ -> 0.5*b*h

    

let zollo a =
 match a<0 with
  |true -> a*(-1)
  |_ -> a*2
    

let min a b =
 match a<b with
  |true -> a
  |_ -> b
    

let max a b =
 match a>b with
  |true-> a
  |_->b
   

let ofTime h m s = h*3600 + m*60 + s

    

let toTime s =
 let h=(s%86400)/3600
 let m=(s%3600)/60
 let seconds= (s%60)
 match s<=0 with
  |true -> (0,0,0)
  |_-> (h, m, seconds)

 
  
     
let digits a =
 let rec CountDigits i acc =
  match i=0  with
   |true->acc
   |_->CountDigits(i/10) (1+acc)
 match a<>0 with 
 |false -> 1
 |_->CountDigits a 0



let minmax (a, b, c, d) = min a b |> min c |> min d, max a b |> max c |> max d
  
  



let isLeap a =
 match a<1582 with
  |true->  failwith "The input year is less than 1582"
  |false->
    match ((a % 4 = 0)  && (a % 100 = 0) && (a % 400 = 0)) || a%4=0 && a%100<>0   with
      |true->true
      |false->false
   


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