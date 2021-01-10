module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = questionExpr(str question, AId var, AType \type, AExpr expr)
  | question(str question, AId var, AType \type)
  | block(list[AQuestion] questions)
  | ifElseQ(AExpr expr, list[AQuestion] ifQs, list[AQuestion] elseQs)
  | ifQ(AExpr expr, list[AQuestion] ifQs)
  ;

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | boolExpr(bool boolExpr)
  | intExpr(int intExpr)
  | strExpr(str strExpr)
  | parenthExpr(AExpr expr)
  | notExpr(AExpr expr)
  | negExpr(AExpr expr)
  | divExpr(AExpr lhs, AExpr rhs)
  | mulExpr(AExpr lhs, AExpr rhs)
  | subExpr(AExpr lhs, AExpr rhs)
  | addExpr(AExpr lhs, AExpr rhs)
  | andExpr(AExpr lhs, AExpr rhs)
  | orExpr(AExpr lhs, AExpr rhs)
  | gtExpr(AExpr lhs, AExpr rhs)
  | ltExpr(AExpr lhs, AExpr rhs)
  | leqExpr(AExpr lhs, AExpr rhs)
  | geqExpr(AExpr lhs, AExpr rhs)
  | eqExpr(AExpr lhs, AExpr rhs)
  | neqExpr(AExpr lhs, AExpr rhs)
  ;

data AId(loc src = |tmp:///|)
  = id(str name)
  ;

data AType(loc src = |tmp:///|)
  = booleanType()
  | integerType()
  | stringType()
  ;
