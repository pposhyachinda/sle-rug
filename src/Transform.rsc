module Transform

import Syntax;
import Resolve;
import AST;
import IO;
import List;
import ParseTree;


/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f)
  = form(f.name, ( [] | it + flatten(boolExpr(true), q) | q <- f.questions ));

list[AQuestion] flatten(AExpr e, AQuestion x) {
  switch (x) {
    case q:questionExpr(_, _, _, _):
      return [ ifQ(e, [q]) ];
    case q:question(_, _, _):
      return [ ifQ(e, [q]) ];
    case block(list[AQuestion] qs):
      return ( [] | it + flatten(e, q) | q <- qs );
    case ifElseQ(AExpr a, list[AQuestion] bs, list[AQuestion] cs):
      return ( [] | it + flatten(andExpr(e, a), b) | b <- bs )
      + ( [] | it + flatten(andExpr(e, notExpr(a)), c) | c <- cs );
    case ifQ(AExpr a, list[AQuestion] bs):
      return ( [] | it + flatten(andExpr(e, a), b) | b <- bs );
    default:
      return [];
  }
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  set[loc] toRename = {useOrDef};
  
  if (useOrDef in useDef.use, <useOrDef, loc d> <- useDef) {
    toRename += {d} + { u | <loc u, d> <- useDef};
  }
  
  if (useOrDef in useDef.def) {
    toRename += { u | <loc u, useOrDef> <- useDef};
  }
  
  return visit (f) {
    case Id x => [Id]newName
      when x@\loc in toRename
    case q:(Question)`<Str a> <Id b> : <Type c> = <Expr d>`
      => (Question)`<Str a> <Id nn> : <Type c> = <Expr d>`
        when q@\loc in toRename,
        Id nn := [Id]newName
    case q:(Question)`<Str a> <Id b> : <Type c>`
      => (Question)`<Str a> <Id nn> : <Type c>`
        when q@\loc in toRename,
        Id nn := [Id]newName
  }
} 
 
 
 

