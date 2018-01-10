module Node =
  struct
    type t = {
      succ : ConjuntoSetV.t ;
      tokenSol : ConjuntoSetT.t;
      conditionals : (int,PardeV.t) Hashtbl.t;
      vars : ConjuntoSetV.t;
      token : ConjuntoSetV.elt;
    }
    let create x = {
      succ = ConjuntoSetV.empty;
      tokenSol = ConjuntoSetT.empty;
      conditionals = Hashtbl.create 10;
      vars = ConjuntoSetV.singleton x;
      token =  x;
    }
    let compare  = Pervasives.compare
(* cada set necessita sempre de ser devolvida apos alterado*)
    let addvar bs x = let newvars = ConjuntoSetV.add x bs.vars in
      {succ = bs.succ;tokenSol= bs.tokenSol ; conditionals = bs.conditionals ; vars = newvars;token = bs.token}
    let addsuc bs x = let newsucc = ConjuntoSetV.add x bs.succ in
      {succ = newsucc;tokenSol= bs.tokenSol ; conditionals = bs.conditionals ; vars = bs.vars;token = bs.token}
    let settokensol node s =  {succ = node.succ;tokenSol= s ; conditionals = node.conditionals ; vars = node.vars;token = node.token}
    let setsucc node s =  {succ = s;tokenSol= node.tokenSol ; conditionals = node.conditionals ; vars = node.vars;token = node.token}
    let getsucc x = x.succ
    let getTokenSol y = y.tokenSol
    let getconditionais w = w.conditionals
    let getvars v = v.vars
    let var k = k.vars
    let getoken z = z.token
(*remove todas as keys na hashtbl mesmo que já tenham sido sobrepostas*)
    let rec remove_all tbl key =
      if Hashtbl.mem tbl key then begin
        Hashtbl.remove tbl key;
        remove_all tbl key
      end
    let condgetorupdate tabela nr x = try
        let newpar = PardeV.add x (Hashtbl.find tabela nr) in
        Hashtbl.replace tabela nr newpar
      with Not_found ->
        begin
          let s = PardeV.singleton x in
          Hashtbl.add tabela nr s
        end
  end
