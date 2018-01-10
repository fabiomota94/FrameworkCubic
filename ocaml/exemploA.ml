open List
open Printf
open Hashtbl
open Set

let es = print_string "Example?";;
let string = read_line();;
let test string =
  if(string = "cfa")then
    (
      Framework_Cubica.addConstantConstraint ("inc","inc");
      Framework_Cubica.addConstantConstraint ("dec","dec");
      Framework_Cubica.addConstantConstraint ("ide","ide");
      Framework_Cubica.addConstantConstraint ("foo","foo");
      Framework_Cubica.addSubsetConstraint ("ide","f");
      Framework_Cubica.addConditionalConstraint ("ide","f","n","k");
      Framework_Cubica.addConditionalConstraint ("ide","f","return i+1","f(n)");
      Framework_Cubica.addConditionalConstraint ("dec","f","n","j");
      Framework_Cubica.addConditionalConstraint ("dec","f","return j-1","f(n)");
      Framework_Cubica.addConditionalConstraint ("inc","f","n","i");
      Framework_Cubica.addConditionalConstraint ("inc","f" ,"return k","f(n)");
      Framework_Cubica.addConstantConstraint ("main()","main()");
      Framework_Cubica.addSubsetConstraint ("x","n");
      Framework_Cubica.addSubsetConstraint ("inc","f");
      Framework_Cubica.addSubsetConstraint ("r","foo(x,inc)");
      Framework_Cubica.addSubsetConstraint ("x","n");
      Framework_Cubica.addSubsetConstraint ("dec","f");
      Framework_Cubica.addSubsetConstraint ("r","foo(x,dec)") )
  else(
    Framework_Cubica.addConstantConstraint ("posf:1:1","posf:1:1");
      Framework_Cubica.addConstantConstraint ("negf:5:1","negf:5:1");
      Framework_Cubica.addConstantConstraint ("main()","main()");
      Framework_Cubica.addSubsetConstraint ("posf:1:1","f:10:27");
      Framework_Cubica.addSubsetConstraint ("negf:5:1","f:10:27");
      Framework_Cubica.addConditionalConstraint ("posf:1:1","f:10:27","4:25:15","b:1:6");
      Framework_Cubica.addConditionalConstraint ("posf:1:1","f:10:27","return 5;:2:5","f(4)");
      Framework_Cubica.addConditionalConstraint ("negf:5:1","f:10:27","4:25:15","c:5:6") ;
    Framework_Cubica.addConditionalConstraint ("negf:5:1","f:10:27","return -5;:6:5","f(4)"));;


test string;;


let k = Framework_Cubica.getsolution () ;;
let k1 = Framework_Cubica.showhash ;;


let getsol f = Hashtbl.iter (fun s s1  ->
    if((Framework_Cubica.ConjuntoSetT.is_empty s1)=false)then(
    print_string "[[ ";
    print_string s;
    print_string" ]]={";
    Framework_Cubica.ConjuntoSetT.iter (fun v -> print_string " " ;print_string v ;print_string " ")s1; print_string"}\n"
  ))f;;

  let getcont f = Hashtbl.iter (fun s s1  ->
      print_string "[[ ";
      print_string s;
      print_string" ]]={";
      Framework_Cubica.ConjuntoSetT.iter (fun v -> print_string " " ;print_string v ;print_string " ")s1; print_string"}\n"
    )f;;



  print_string "constraints:\n";
  getcont k;
print_string "Solution is:\n";
  getsol k;;
