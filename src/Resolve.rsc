module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(AForm f)
  = { <l, x> | /ref(id(str x), src=loc l) := f };

Def defs(AForm f)
  = { <x, q.src> | /q:questionExpr(_, id(str x), _, _) := f }
  + { <x, q.src> | /q:question(_, id(str x), _) := f };
