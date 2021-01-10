module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id name "{" Question* questions "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str question Id var ":" Type type "=" Expr expr	// computed question
  | Str question Id var ":" Type type				// question
  | "{" Question* questions "}"
  | "if" "(" Expr expr ")" "{" Question* ifQs "}" "else" "{" Question* elseQs "}"
  | "if" "(" Expr expr ")" "{" Question* ifQs "}"
  ; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Bool
  | Int
  | Str
  | "(" Expr expr ")"	// 
  > right (				// | or <
    "-" Expr expr
  | "!" Expr expr
  )
  > left (
    Expr lhs "/" Expr rhs
  | Expr lhs "*" Expr rhs
  > Expr lhs "-" Expr rhs
  | Expr lhs "+" Expr rhs
  > Expr lhs "&&" Expr rhs
  > Expr lhs "||" Expr rhs
  )
  > non-assoc (
    gt: Expr lhs "\>" Expr rhs
  | lt: Expr lhs "\<" Expr rhs
  | leq: Expr lhs "\<=" Expr rhs
  | geq: Expr lhs "\>=" Expr rhs
  > eq: Expr lhs "==" Expr rhs
  | neq: Expr lhs "!=" Expr rhs
  )
  ;
  
syntax Type
  = "boolean"
  | "integer"
  | "string"
  ;  
  
lexical Str = [\"] ![\"]* [\"]; // slightly simplified;

lexical Int 
  = [1-9][0-9]*
  | [0]
  ;

lexical Bool = "true" | "false";



