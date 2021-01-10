module Check

import AST;
import Resolve;
import Message; // see standard library
import Set;
import IO;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv env = {};
  for (/q:questionExpr(str name, AId var, AType \type, _) := f) {
    env += { <q.src, var.name, name, getType(\type)> };
  }
  for (/q:question(str name, AId var, AType \type) := f) {
    env += { <q.src, var.name, name, getType(\type)> };
  }
  return env; 
}

Type getType(AType t) {
  switch(t) {
    case booleanType(): return tbool();
    case integerType(): return tint();
    case stringType(): return tstr();
    default : return tunknown();	
  }
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | AQuestion q <- f.questions);

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(q:questionExpr(str label, id(str name), AType \type, AExpr e), TEnv tenv, UseDef useDef)
  = { error("Expression type <expType> does not match declared type <decType>", e.src) | decType != expType }
  + { error("Duplicate question with a different type", q.src) | size((tenv<name,\type>)[name]) > 1 }
  + { warning("<labelCount> occurences of <label> detected", q.src) | labelCount > 1 }
  + { warning("Different labels for occurences of same question", q.src) | size((tenv<name, label>)[name]) > 1 }
  + check(e, tenv, useDef)
    when expType := typeOf(e, tenv, useDef),
         decType := getType(\type),
         labelCount := size((tenv<label,def>)[label]);
         
set[Message] check(q:question(str label, id(str name), _), TEnv tenv, UseDef useDef)
  = { error("Duplicate question with a different type", q.src) | size((tenv<name,\type>)[name]) > 1 }
  + { warning("<labelCount> occurences of <label> detected", q.src) | labelCount > 1}
  + { warning("Different labels for occurences of same question", q.src) | size((tenv<name, label>)[name]) > 1 }
    when labelCount := size((tenv<label,def>)[label]);

set[Message] check(q:block(list[AQuestion] questions), TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | AQuestion q <- questions);

set[Message] check(q:ifElseQ(AExpr e, list[AQuestion] ifQs, list[AQuestion] elseQs), TEnv tenv, UseDef useDef)
  = { error("Condition needs to be boolean. <typeOf(e, tenv, useDef)> is not tbool()", e.src) | typeOf(e, tenv, useDef) != tbool() }
  + ( {} | it + check(q, tenv, useDef) | AQuestion q <- ifQs)
  + ( {} | it + check(q, tenv, useDef) | AQuestion q <- elseQs)
  + check(e, tenv, useDef);

set[Message] check(q:ifQ(AExpr e, list[AQuestion] ifQs), TEnv tenv, UseDef useDef)
  = { error("Condition needs to be boolean. <typeOf(e, tenv, useDef)> is not tbool()", e.src) | typeOf(e, tenv, useDef) != tbool() }
  + ( {} | it + check(q, tenv, useDef) | AQuestion q <- ifQs)
  + check(e, tenv, useDef);  

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(ref(AId x), TEnv tenv, UseDef useDef)
  = { error("Undeclared question", x.src) | useDef[x.src] == {} };

set[Message] check(parenthExpr(AExpr e), TEnv tenv, UseDef useDef)
  = check(e, tenv, useDef);

set[Message] check(ex:negExpr(AExpr e), TEnv tenv, UseDef useDef)
  = { error("- not supported for <t>", ex.src) | t != tint() }
  + check(e, tenv, useDef)
    when t := typeOf(e, tenv, useDef);

set[Message] check(ex:notExpr(AExpr e), TEnv tenv, UseDef useDef)
  = { error("! not supported for <t>", ex.src) | t != tbool() }
  + check(e, tenv, useDef)
    when t := typeOf(e, tenv, useDef);

set[Message] check(ex:divExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("/ not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:mulExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("* not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:subExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("- not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:addExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("+ not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:andExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("&& not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tbool() && rhsType == tbool()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:orExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("|| not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tbool() && rhsType == tbool()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:gtExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("\> not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
   when lhsType := typeOf(lhs, tenv, useDef),
        rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:ltExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("\< not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:leqExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("\<= not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:geqExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("\>= not supported between <lhsType> and <rhsType>", ex.src) | !(lhsType == tint() && rhsType == tint()) }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:eqExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("== not supported between <lhsType> and <rhsType>", ex.src) | lhsType != rhsType }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

set[Message] check(ex:neqExpr(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef)
  = { error("!= not supported between <lhsType> and <rhsType>", ex.src) | lhsType != rhsType }
  + check(lhs, tenv, useDef)
  + check(rhs, tenv, useDef)
    when lhsType := typeOf(lhs, tenv, useDef),
         rhsType := typeOf(rhs, tenv, useDef);

default set[Message] check(_, _, _) = {};

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case boolExpr(_): return tbool();
    case intExpr(_): return tint();
    case strExpr(_): return tstr();
    case parenthExpr(AExpr ex): return typeOf(ex, tenv, useDef);
    case negExpr(AExpr ex): return typeOf(ex, tenv, useDef) == tint() ? tint() : tunknown();
    case notExpr(AExpr ex): return typeOf(ex, tenv, useDef) == tbool() ? tbool() : tunknown();
    case divExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tint() : tunknown();
    case mulExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tint() : tunknown();
    case subExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tint() : tunknown();
    case addExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tint() : tunknown();
    case andExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool() ? tbool() : tunknown();
    case orExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool() ? tbool() : tunknown();
    case gtExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tbool() : tunknown();
    case ltExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tbool() : tunknown();
    case leqExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tbool() : tunknown();
    case geqExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint() ? tbool() : tunknown();
    case eqExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef) ? tbool() : tunknown();
    case neqExpr(AExpr lhs, AExpr rhs): return typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef) ? tbool() : tunknown();
    // etc.
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

