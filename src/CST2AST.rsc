module CST2AST

import Syntax;
import AST;

import ParseTree;
import String; // toInt()

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("<f.name>", [cst2ast(q) | q <- f.questions], src=f@\loc); 
}

AQuestion cst2ast(Question q) {
  switch (q) {
  	case (Question)`<Str a> <Id b> : <Type c> = <Expr d>`: return questionExpr("<a>", id("<b>", src=q@\loc), cst2ast(c), cst2ast(d), src=q@\loc);
  	case (Question)`<Str a> <Id b> : <Type c>`: return question("<a>", id("<b>", src=q@\loc), cst2ast(c), src=q@\loc);	
    case (Question)`{ <Question* a> }`: return block([cst2ast(x) | x <- a], src=q@\loc);
  	case (Question)`if ( <Expr a> ) { <Question* b> } else { <Question* c> }`: return ifElseQ(cst2ast(a), [cst2ast(x) | x <- b], [cst2ast(x) | x <- c], src=q@\loc);
  	case (Question)`if ( <Expr a> ) { <Question* b> }`: return ifQ(cst2ast(a), [cst2ast(x) | x <- b], src=q@\loc);
    default: throw "Unhandled question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=x@\loc);
    case (Expr)`<Bool x>`: return boolExpr((Bool)`true` := x ? true : false, src=x@\loc);
    case (Expr)`<Int x>`: return intExpr(toInt("<x>"), src=x@\loc);
    case (Expr)`<Str x>`: return strExpr("<x>", src=x@\loc);
    case (Expr)`( <Expr x> )`: return parenthExpr(cst2ast(x), src=e@\loc);
    case (Expr)`- <Expr x>`: return negExpr(cst2ast(x), src=e@\loc);
    case (Expr)`! <Expr x>`: return notExpr(cst2ast(x), src=e@\loc);
    case (Expr)`<Expr a> / <Expr b>`: return divExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> * <Expr b>`: return mulExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> - <Expr b>`: return subExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> + <Expr b>`: return addExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> && <Expr b>`: return andExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> || <Expr b>`: return orExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> \> <Expr b>`: return gtExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> \< <Expr b>`: return ltExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> \<= <Expr b>`: return leqExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> \>= <Expr b>`: return geqExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> == <Expr b>`: return eqExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    case (Expr)`<Expr a> != <Expr b>`: return neqExpr(cst2ast(a), cst2ast(b), src=e@\loc);
    // etc.
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
    case (Type)`boolean`: return booleanType(src=t@\loc);
    case (Type)`integer`: return integerType(src=t@\loc);
    case (Type)`string`: return stringType(src=t@\loc);
    default: throw "Unhandled type: <t>";
  }
}
