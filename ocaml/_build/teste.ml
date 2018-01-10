open List
open Printf
open Hashtbl
open Set

let es = print_string "Example?";;
let string = read_line();;
let test string =
  if(string = "one")then
    (
      T.addConstantConstraint ("inc","inc");
      T.addConstantConstraint ("dec","dec");
      T.addConstantConstraint ("ide","ide");
      T.addConstantConstraint ("foo","foo");
      T.addSubsetConstraint ("ide","f");
      T.addConditionalConstraint ("ide","f","n","k");
      T.addConditionalConstraint ("ide","f","return i+1","f(n)");
      T.addConditionalConstraint ("dec","f","n","j");
      T.addConditionalConstraint ("dec","f","return j-1","f(n)");
      T.addConditionalConstraint ("inc","f","n","i");
      T.addConditionalConstraint ("inc","f" ,"return k","f(n)");
      T.addConstantConstraint ("main()","main()");
      T.addSubsetConstraint ("x","n");
      T.addSubsetConstraint ("inc","f");
      T.addSubsetConstraint ("r","foo(x,inc)");
      T.addSubsetConstraint ("x","n");
      T.addSubsetConstraint ("dec","f");
      T.addSubsetConstraint ("r","foo(x,dec)") )
  else(
    T.addConstantConstraint ("posf:1:1","posf:1:1");
      T.addConstantConstraint ("negf:5:1","negf:5:1");
      T.addConstantConstraint ("main()","main()");
      T.addSubsetConstraint ("posf:1:1","f:10:27");
      T.addSubsetConstraint ("negf:5:1","f:10:27");
      T.addConditionalConstraint ("posf:1:1","f:10:27","4:25:15","b:1:6");
      T.addConditionalConstraint ("posf:1:1","f:10:27","return 5;:2:5","f(4)");
      T.addConditionalConstraint ("negf:5:1","f:10:27","4:25:15","c:5:6") ;
    T.addConditionalConstraint ("negf:5:1","f:10:27","return -5;:6:5","f(4)"));;


test string;;


let k = T.getsolution () ;;
let k1 = T.showhash ;;


let getsol f = Hashtbl.iter (fun s s1  ->
    if((T.ConjuntoSetT.is_empty s1)=false)then(
    print_string "[[ ";
    print_string s;
    print_string" ]]={";
    T.ConjuntoSetT.iter (fun v -> print_string " " ;print_string v ;print_string " ")s1; print_string"}\n"
  ))f;;

  let getcont f = Hashtbl.iter (fun s s1  ->
      print_string "[[ ";
      print_string s;
      print_string" ]]={";
      T.ConjuntoSetT.iter (fun v -> print_string " " ;print_string v ;print_string " ")s1; print_string"}\n"
    )f;;



  print_string "constraints:\n";
  getcont k;
print_string "Solution is:\n";
  getsol k;;
