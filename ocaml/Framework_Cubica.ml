  open List
  open Printf
  open Hashtbl
  open Set



  type t = string
  type v = string

  module V_ord =
  struct
    type t = v
    let compare  = Pervasives.compare
  end

  module T_ord =
  struct
    type t = string
    let compare  = Pervasives.compare
  end

module I_ord =
struct
  type t = int
  let compare  = Pervasives.compare
end

  module ConjuntosetInt = Set.Make (I_ord)
  module ConjuntoSetV = Set.Make(V_ord)
  module ConjuntoSetT = Set.Make(T_ord)


  module ParOrdered = struct
    type t = ConjuntoSetV.elt * ConjuntoSetV.elt
    let compare = compare
  end
  module PardeV = Set.Make(ParOrdered)



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
module ConjuntoSetNode = Set.Make(Node)

let lasttknid = ref (-1) ;;

  let cycleElimination = true;;
(*Função que incrementa a medida que tokens são introduzidos na hashtbl tokenToInt*)
  let inc () =
    lasttknid:= !lasttknid + 1;
    (*print_int(!lasttknid);*)
    !lasttknid;;




  let nexttokenid () =
    inc();;


  let varToNode = Hashtbl.create 100;;
  let tokenToInt = Hashtbl.create 100;;



(*Função que devolve o int do token ou atualiza o que procuramos caso não exista ele adiciona a hashtbl , basicamente não vai haver sobreposição de chaves*)
  let getTokenInt (t1) =
    try
      Hashtbl.find tokenToInt t1;
    with Not_found ->
      begin
        Hashtbl.add tokenToInt t1 (nexttokenid () ); Hashtbl.find tokenToInt t1;
      end




(*Função que devolve o node da variavel ou atualiza o que procuramos caso não exista ele adiciona a hashtbl , basicamente não vai haver sobreposição de chaves*)
  let getOrPutNode (valor) =
    try
      Hashtbl.find varToNode valor;
    with Not_found ->
      begin
        let novo = Node.create valor in
        Hashtbl.add varToNode valor novo;novo;
      end;;

  let replaceNodeOnVarToNode x node = Hashtbl.replace varToNode x node
  (*Sempre que é alterado um node da Hashtbl voltamos a guardar pois alguns dos campos são set e devolvem um novo conjunto e é necessário voltar a guardar esse node*)

  let getVarToNode v =  (*Função que procura apenas se existe o par da chave V introduzida o Node que corresponde ao Var que é pedido*)
  try
    Hashtbl.find varToNode v;
  with Not_found ->
    begin
      failwith "NotFound"
    end;;


(*funcao que tenta encontrar um cyclo do node from ao node too no grafo *)
let detectPath (from,too) =
    let visited = ConjuntoSetNode.empty in
    let rec detectPathRec (current:ConjuntoSetNode.elt) =
      begin
        if (current = too) then (* se isto acontecer significa que temos dois nodos iguais o que cria ciclo*)
      let l = [current] in
        l;
    else
      begin
      let visited2 = ConjuntoSetNode.add current visited in
      let filtro = ConjuntoSetNode.empty in
      (*Node a node vamos procurar por um ciclo desse node e devolver para ser resolvido caso não exista devolve uma lista vazia*)
      let currentsuc = (Node.getsucc current) in
      let filtro2 = ConjuntoSetV.fold (fun s acc -> ConjuntoSetNode.add (getVarToNode s) acc ) currentsuc filtro in
      let tmp2 = ConjuntoSetNode.filter (fun s -> not (ConjuntoSetNode.mem s visited2)) filtro2 in
      if (not (ConjuntoSetNode.is_empty tmp2))then
        let el =  ConjuntoSetNode.max_elt tmp2 in
        let cycleVisited = detectPathRec el in
        if(cycleVisited  = []) then (*Quando este conjunto não é vazio significa que encontrou ciclo e é devolvido em forma de lista para ser resolvido*)
        List.append [current] cycleVisited
        else []
      else []
    end
  end in
  detectPathRec from



(* Estas funções vão juntar nodes que sejam iguais ou que apontem para o mesmo sitio na lista recebida para formar apenas um node com todos os atributos a fim de não criar ciclo*)
  let oldNodeAddWithFirst s node =
    let suc1 = Node.getsucc s in
    let k = ConjuntoSetV.fold (fun s acc -> Node.addsuc acc s) suc1 node in
    replaceNodeOnVarToNode (Node.getoken k) k;k


  let oldNodeAddHashes s node =
    let c1 = Node.getconditionais s in
    Hashtbl.iter (fun s1 s2 -> try begin
        let hashdenode = Node.getconditionais node in
        let set = Hashtbl.find hashdenode s1 in
        let setunion = PardeV.union set s2 in
        Hashtbl.replace hashdenode s1 setunion
      end
        with Not_found -> () ) c1

  let oldNodeaddSol s node =
    let s1 = Node.getTokenSol s in
    let newsucc = ConjuntoSetT.union (Node.getTokenSol node) s1 in
    let update = Node.settokensol node newsucc in
    Hashtbl.iter (fun s1 s2 ->
        if(s2 = node) then
          replaceNodeOnVarToNode s1 update) varToNode

  let oldnodevars s node =
    let v1 = Node.getvars s in
    ConjuntoSetV.iter (fun v ->
        replaceNodeOnVarToNode v node ;
        let neww = Node.addsuc node v in
        let update = Node.setsucc node (Node.getsucc neww) in
        replaceNodeOnVarToNode v update
      ) v1



(*Basicamente esta função resolve o ciclo recebido na função anterior*)
let rec tailforeach l node = List.iter (fun s -> oldNodeAddHashes s (oldNodeAddWithFirst s node); oldNodeaddSol s node;oldnodevars s node) l
  and collapseCycle lista  = match lista with
      x :: tl ->
      let last = List.rev tl in (*ver se funciona sem o rev *)
      tailforeach last x
    | [] -> ()



  let getListofSets tabela nr =
    let l = Hashtbl.find_all tabela nr in
    if ( l = []) then
      []
    else
        l

(*Esta função faz a correspondecia de um Token para a sua Variavel , Um novo node é criado caso esse token não contenha essa variavel*)
  let rec addeachPardeVonSet l = PardeV.iter (fun s1 -> addSubsetConstraint(s1)) l
  and addSubsetConstraint ((x,y)) =
    let nx = getOrPutNode(x) in
    let ny = getOrPutNode(y) in
    if (nx <> ny) then begin
      let k = Node.addsuc nx y in
      replaceNodeOnVarToNode x k ;

      addAndPropagateBits(Node.getTokenSol k,y);
      if(cycleElimination) then
        (collapseCycle(detectPath(ny,nx)))
    end
  and addAndPropagateBits (s,x) =
    let node = getOrPutNode(x) in
    let old = Node.getTokenSol node in
    let newtoken = ConjuntoSetT.union old s in
    if(newtoken <> old) then
      let newtoken2 = ConjuntoSetT.union (Node.getTokenSol node) s in
      let novo2 = Node.settokensol node newtoken2 in
      replaceNodeOnVarToNode x novo2 ;
       let diff = ConjuntoSetT.diff newtoken old in
       let cond = Node.getconditionais novo2 in
       ConjuntoSetT.iter (fun s -> getSetOfListdeSets (getListofSets cond (getTokenInt s) ) ) diff ;
       ConjuntoSetT.iter (fun s -> (Node.remove_all cond (getTokenInt s) ) )diff ;
       ConjuntoSetV.iter (fun s -> addAndPropagateBits (newtoken,s)) (Node.getsucc novo2) ;
  and getSetOfListdeSets l = List.iter (fun (s2) -> addeachPardeVonSet(s2)) l




  let addConstantConstraint (t,x) =
    let bs = ConjuntoSetT.empty in
    let newbs = ConjuntoSetT.add t bs in
    addAndPropagateBits(newbs,x)  ;;





  let addConditionalConstraint(t,x,y,z) =
    let xn = getOrPutNode(x) in
    let h = Node.getTokenSol xn in
    let validacao = ConjuntoSetT.mem t h in
    if (validacao) then(
        addSubsetConstraint(y,z))
    else if (y <> z) then
         let condi = Node.getconditionais xn in
         let numero = getTokenInt t in
         Node.condgetorupdate condi numero (y,z)


           (* devolve a solução em forma de hashtbl onde faz correspondecia da variavel para os sets de token da mesma*)
let getsolution () =
  let solution = Hashtbl.create 100 in
  Hashtbl.iter (fun v _ -> Hashtbl.add solution v (
    let node = getOrPutNode v in
    let tsol = Node.getTokenSol node in
      tsol) )varToNode;solution
        (*      ConjuntoSetT.fold (fun s acc ->
                ConjuntosetInt.add (getTokenInt s) acc) tsol ConjuntosetInt.empty) ) varToNode ;solution*)
let showhash =
   varToNode
