open Prop_def;;
let rec fnc prop = match prop with

(Var x) -> Var x;
|IMPLIQ(x,y) -> fnc(left_right_side_OU (NEG(x)) y);
|Vrai -> Vrai
|Faux -> Faux
|NEG ( NEG ( x ) ) -> fnc x ;
|NEG (OU(x,y)) -> fnc (ET(NEG(x),NEG(y)));
|NEG (ET(x,y)) -> fnc(OU(NEG(x),NEG(y)));
|NEG (IMPLIQ(x,y)) -> fnc(ET(x,NEG(y)));
|NEG (Vrai) -> fnc Faux;
|NEG (Faux) -> fnc Vrai;
|NEG (x) -> left_right_side_NEG x;
|OU(x,NEG(y)) -> if x=y then Vrai else (left_right_side_OU x (NEG(y))) ;
|OU(Vrai,_) -> Vrai;
|OU(_,Vrai) -> Vrai;
|OU(Faux,x)-> fnc x;
|OU(x,Faux)-> fnc x;
|OU(x,ET(y,z)) -> (left_right_side_ET (OU(x,y)) (OU (x,z))) ;
|OU(ET(y,z),x) -> (left_right_side_ET (OU(x,y)) (OU (x,z))) ;
|OU(x,y) -> if (x=y) then fnc x else left_right_side_OU x y ;
|ET(x,y) -> if (x=y) then fnc x else (left_right_side_ET x y) ;
|EQUIV(x,y) -> if(x=y) then Vrai else fnc(left_right_side_ET (IMPLIQ(x,y)) (IMPLIQ(y,x)))
and

left_right_side_ET x y= 
  let left_side = (fnc x) in let right_side = fnc y in  ( ET(left_side,right_side))
  
and 
left_right_side_OU x y= 
  let left_side = (fnc x) in let right_side = fnc y in  ( OU(left_side,right_side))

and 
left_right_side_NEG x = 
  let left_side = (fnc x) in  (NEG(left_side))

  