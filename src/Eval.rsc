module Eval

import AST;
import Resolve;
import IO;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv venv = ();
  for (/questionExpr(_, AId var, AType \type, _) := f) {
    switch (\type) {
      case booleanType():
        venv += (var.name: vbool(false));
      case integerType():
        venv += (var.name: vint(0));
      case stringType():
        venv += (var.name: vstr(""));
    }
  }
  for (/question(_, AId var, AType \type) := f) {
    switch (\type) {
      case booleanType():
        venv += (var.name: vbool(false));
      case integerType():
        venv += (var.name: vint(0));
      case stringType():
        venv += (var.name: vstr(""));
    }
  }
  return venv;
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv){ 
  for (AQuestion q <- f.questions) {
    venv = eval(q, inp, venv);
  }
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  if (questionExpr(str _, AId b, AType _, AExpr d) := q) {
    venv += (b.name: eval(d, venv));
  } else if (question(str _, AId b, AType _) := q) {
    if (inp.question == b.name) {
      venv += (b.name: inp.\value);
    }
  } else if (block(list[AQuestion] a) := q) {
    for (AQuestion x <- a) {
      venv = eval(x, inp, venv);
    }
  } else if (ifElseQ(AExpr a, list[AQuestion] b, list[AQuestion] c) := q) {
    if (eval(a, venv).b) {
      for (AQuestion x <- b) {
        venv = eval(x, inp, venv);
      }
    } else {
      for (AQuestion x <- c) {
        venv = eval(x, inp, venv);
      }
    }
  } else if (ifQ(AExpr a, list[AQuestion] b) := q) {
    if (eval(a, venv).b) {
      for (AQuestion x <- b) {
        venv = eval(x, inp, venv);
      }
      
    }
  }  
  return venv;
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(AId x): return venv[x.name];
    case boolExpr(bool x): return vbool(x);
    case intExpr(int x): return vint(x);
    case strExpr(str x): return vstr(x);
    case parenthExpr(AExpr x): return eval(x, venv);
    case negExpr(AExpr x): return vint(-eval(x, venv).n);
    case notExpr(AExpr x): return vbool(!eval(x, venv).b);
    case divExpr(AExpr l, AExpr r): return vint(eval(l, venv).n / eval(r, venv).n);
    case mulExpr(AExpr l, AExpr r): return vint(eval(l, venv).n * eval(r, venv).n);
    case subExpr(AExpr l, AExpr r): return vint(eval(l, venv).n - eval(r, venv).n);
    case addExpr(AExpr l, AExpr r): return vint(eval(l, venv).n + eval(r, venv).n);
    case andExpr(AExpr l, AExpr r): return vbool(eval(l, venv).b && eval(r, venv).b);
    case orExpr(AExpr l, AExpr r): return vbool(eval(l, venv).b || eval(r, venv).b);
    case gtExpr(AExpr l, AExpr r): return vbool(eval(l, venv).n > eval(r, venv).n);
    case ltExpr(AExpr l, AExpr r): return vbool(eval(l, venv).n < eval(r, venv).n);
    case leqExpr(AExpr l, AExpr r): return vbool(eval(l, venv).n <= eval(r, venv).n);
    case geqExpr(AExpr l, AExpr r): return vbool(eval(l, venv).n >= eval(r, venv).n);
    case eqExpr(AExpr l, AExpr r): return vbool(eval(l, venv) == eval(r, venv));
    case neqExpr(AExpr l, AExpr r): return vbool(eval(l, venv) != eval(r, venv));
    default: throw "Unsupported expression <e>";
  }
}

