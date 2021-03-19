(* Part 2 *)

(* Question 1 *)

(* Integer Inputs *)

let rec powrec product x n =
    if n = 0 then
        1
    else if n = 1 then
        product
    else
        powrec (product * x) x (n - 1)
;;

let pow x n =
    powrec x x n
;;

(* Float Inputs *)

let rec float_powrec (product: float) (x: float) n =
    if n = 0 then
        float 1
    else if n = 1 then
        product
    else
        float_powrec (product *. x) x (n - 1)
;;

let float_pow (x: float) n =
    float_powrec x x n
;;

(* Question 2 *)

let rec compress_rec curhead curlist retlist =
    if curlist = [] then
        retlist
    else if curhead = (List.hd curlist) then
        compress_rec (List.hd curlist) (List.tl curlist) retlist
    else
        compress_rec (List.hd curlist) (List.tl curlist) (List.append retlist [List.hd curlist])
;;

let compress inlist =
    compress_rec "" inlist []
;;

(* Question 3 *)

let rec remove_if_rec curlist pred retlist =
    if curlist = [] then
        retlist
    else if pred (List.hd curlist) then
        remove_if_rec (List.tl curlist) pred retlist
    else
        remove_if_rec (List.tl curlist) pred (List.append retlist [List.hd curlist])
;;

let remove_if inlist pred =
    remove_if_rec inlist pred []
;;

(* Question 4 *)

let rec slice_rec curlist i j cind retlist =
    if curlist = [] then
        retlist
    else if cind = j then
        retlist
    else if i >= j then
        retlist
    else if cind >= j then
        retlist
    else if cind >= i then
        slice_rec (List.tl curlist) i j (cind + 1) (List.append retlist [List.hd curlist])
    else
        slice_rec (List.tl curlist) i j (cind + 1) retlist
;;

let slice inlist i j =
    slice_rec inlist i j 0 []
;;

(* Question 5 *)

let rec checkequiv efunc curterm curbuckets retlist matched =
    if curbuckets = [] then
        if matched = false then
            List.append retlist [[curterm]]
        else
            retlist
    else if (efunc curterm (List.hd(List.hd curbuckets))) then
        checkequiv efunc curterm (List.tl curbuckets)
        (List.append retlist [(List.append (List.hd curbuckets) [curterm])])
        true
    else
        checkequiv efunc curterm (List.tl curbuckets) (List.append retlist [List.hd curbuckets]) matched

;;

let rec equivs_rec efunc inlist retlist =
    if inlist = [] then
        retlist
    else
        equivs_rec efunc (List.tl inlist) (checkequiv efunc (List.hd inlist) retlist [] false)

;;

let equivs efunc inlist =
    equivs_rec efunc inlist []

;;

(* Question 6 *)

let rec checkprime x track =
    if float_of_int track > sqrt(float_of_int x) then
        true
    else if (x mod track) = 0 then
        false
    else
        checkprime x (track + 1)

;;

(*Assuming insum is always even*)

let rec goldbackpair_rec insum curcheck =
    if curcheck > (insum / 2) then
        (-1,-1) (*This should never happen according to the Goldbach's conjecture*)
    else if (checkprime curcheck 2) && (checkprime (insum - curcheck) 2) then
        (curcheck, insum - curcheck)
    else
        goldbackpair_rec insum (curcheck + 1)
;;

let goldbackpair insum =
    goldbackpair_rec insum 3 (*could start with 2, but insum is even and therefore insum - 2 is never prime*)
;;

(* Question 7 *)

let rec equiv_on f g lst =
    if lst = [] then
        true
    else if f(List.hd lst) <> g(List.hd lst) then
        false
    else
        equiv_on f g (List.tl lst)
;;

(* Question 8 *)

let rec pwfilter_rec cmpf t1 curlist retlist counter =
    if  counter mod 2 = 0 && curlist = [] then
        retlist
    else if curlist = [] then
        List.append retlist [t1]
    else if counter mod 2 = 0 then
        pwfilter_rec cmpf (List.hd curlist) (List.tl curlist) retlist (counter + 1)
    else
        pwfilter_rec cmpf t1 (List.tl curlist) (List.append retlist [cmpf t1 (List.hd curlist)]) (counter + 1)
;;

let pairwisefilter cmp lst =
    pwfilter_rec cmp (List.hd lst) lst [] 0
;;

(* Question 9 *)

let make_product ptup =
    fun x -> int_of_float(float_of_int(x) ** float_of_int(snd ptup)) * (fst ptup)
;;

let poly_transform plist =
    List.map make_product plist
;;

let rec poly_create_fun input colist psum =
    if colist = [] then
        psum
    else
        poly_create_fun input (List.tl colist) (psum + ((List.hd colist) input))
;;

let polynomial inlist =
    fun x -> poly_create_fun x (poly_transform inlist) 0
;;
