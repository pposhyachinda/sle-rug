module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library
import String;
import Check;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
  //writeFile(f.src[extension="html"].top, form2html(f));
}

HTML5Node form2html(AForm f) =
  html(
    script(src(reactImport1)),
    script(src(reactImport2)),
    body(
      div(html5attr("id", "test_form")),
      script(src(f.src[extension="js"].file))
    )
  );

str reactImport1 = "https://unpkg.com/react@17/umd/react.development.js\" crossorigin";
str reactImport2 = "https://unpkg.com/react-dom@17/umd/react-dom.development.js\" crossorigin";

//str form2html(AForm f) =
//  "\<html\>
//  '\<script src=\"https://unpkg.com/react@17/umd/react.development.js\" crossorigin\>\</script\>
//  '\<script src=\"https://unpkg.com/react-dom@17/umd/react-dom.development.js\" crossorigin\>\</script\>
//  '\<body\>
//  '\<div id=\"test_form\"\>\</div\>
//  '\<script src=\"<f.src[extension="js"].file>\"\>\</script\>
//  '\</body\>
//  '\</html\>";

str form2js(AForm f) =
  "class <className> extends React.Component {
  '  constructor(props) {
  '    super(props);
  '    this.state = {
  '    <for (<a, b> <- tenv<name, \type>) {>
  '      <a>: <defaultValJs(b)>,
  '    <}>
  '    }
  '  }
  '
  '  render() {
  '    return React.createElement(\"form\", {}, [
  '      \"<f.name>\",
  '    <for (q <- f.questions) {>
  '      <question2js(q)>,
  '    <}>
  '    ]);
  '  }
  '}
  '
  'const domContainer = document.querySelector(\'#test_form\');
  'ReactDOM.render(React.createElement(<className>), domContainer);
  '"
    when
      tenv := collect(f),
      className := formatName(f.name);

str formatName(str name)
  = size(name) > 1
  ? toUpperCase(name[0]) + name[1..]
  : toUpperCase(name);

str defaultValJs(tbool()) = "false";
str defaultValJs(tint()) = "0";
str defaultValJs(tstr()) = "\"\"";

str question2js(q:questionExpr(str a, AId b, AType c, AExpr d)) =
  "React.createElement(\"div\", {}, [
  '  React.createElement(\"label\", {
  '    for: \"<b.name>\"
  '  }, <a>),
  '  React.createElement(\"input\", {
  '    id: \"<b.name>\",
  '    type: \"<type2js(c)>\",
  '    value: <expr2js(d)>,
  '    readonly: \"true\"
  '  })
  '])";
  
str question2js(q:question(str a, AId b, booleanType())) =
  "React.createElement(\"div\", {}, [
  '  React.createElement(\"label\", {
  '    for: \"<b.name>\"
  '  }, <a>),
  '  React.createElement(\"input\", {
  '    id: \"<b.name>\",
  '    type: \"checkbox\",
  '    value: this.state.<b.name>,
  '    onChange: () =\> this.setState({<b.name>: !this.state.<b.name>})
  '  })
  '])";
  
str question2js(q:question(str a, AId b, integerType())) =
  "React.createElement(\"div\", {}, [
  '  React.createElement(\"label\", {
  '    for: \"<b.name>\"
  '  }, <a>),
  '  React.createElement(\"input\", {
  '    id: \"<b.name>\",
  '    type: \"number\",
  '    value: this.state.<b.name>,
  '    onChange: (e) =\> this.setState({<b.name>: e.target.value})
  '  })
  '])";

str question2js(q:question(str a, AId b, stringType())) =
  "React.createElement(\"div\", {}, [
  '  React.createElement(\"label\", {
  '    for: \"<b.name>\"
  '  }, <a>),
  '  React.createElement(\"input\", {
  '    id: \"<b.name>\",
  '    type: \"text\",
  '    value: this.state.<b.name>,
  '    onChange: (e) =\> this.setState({<b.name>: e.target.value})
  '  })
  '])";
  
str question2js(q:block(list[AQuestion] as)) =
  "React.createElement(\"div\", {}, [
  '<for (a <- as) {>
  '  <question2js(a)>,
  '<}>
  '])";
  
str question2js(q:ifElseQ(AExpr a, list[AQuestion] bs, list[AQuestion] cs)) =
  "(<expr2js(a)>
  '  ? React.createElement(\"div\", {}, [
  '    <for (b <- bs) {>
  '      <question2js(b)>,
  '    <}>
  '    ])
  '  : React.createElement(\"div\", {}, [
  '    <for (c <- cs) {>
  '      <question2js(c)>,
  '    <}>
  '    ])
  ')";

str question2js(q:ifQ(AExpr a, list[AQuestion] bs)) =
  "(<expr2js(a)>
  '  ? React.createElement(\"div\", {}, [
  '    <for (b <- bs) {>
  '      <question2js(b)>,
  '    <}>
  '    ])
  '  : null
  ')";
  
str question2js(_) = "";
  
str expr2js(AExpr e) {
  switch (e) {
    case ref(id(str a)):return "this.state.<a>";
    case boolExpr(bool a): return "<a>";
    case intExpr(int a): return "<a>";
    case strExpr(str a): return a;
    case parenthExpr(AExpr a): return "(<expr2js(a)>)";
    case notExpr(AExpr a): return "!<expr2js(a)>";
    case negExpr(AExpr a): return "-<expr2js(a)>";
    case divExpr(AExpr a, AExpr b): return "Math.floor(<expr2js(a)> / <expr2js(b)>)";
    case mulExpr(AExpr a, AExpr b): return "<expr2js(a)> * <expr2js(b)>";
    case subExpr(AExpr a, AExpr b): return "<expr2js(a)> - <expr2js(b)>";
    case addExpr(AExpr a, AExpr b): return "<expr2js(a)> + <expr2js(b)>";
    case andExpr(AExpr a, AExpr b): return "<expr2js(a)> && <expr2js(b)>";
    case orExpr(AExpr a, AExpr b): return "<expr2js(a)> || <expr2js(b)>";
    case gtExpr(AExpr a, AExpr b): return "<expr2js(a)> \> <expr2js(b)>";
    case ltExpr(AExpr a, AExpr b): return "<expr2js(a)> \< <expr2js(b)>";
    case leqExpr(AExpr a, AExpr b): return "<expr2js(a)> \<= <expr2js(b)>";
    case geqExpr(AExpr a, AExpr b): return "<expr2js(a)> \>= <expr2js(b)>";
    case eqExpr(AExpr a, AExpr b): return "<expr2js(a)> == <expr2js(b)>";
    case neqExpr(AExpr a, AExpr b): return "<expr2js(a)> != <expr2js(b)>";
  }
  return "";
}

str type2js(AType t) {
  switch (t) {
    case booleanType(): return "checkbox";
    case integerType(): return "number";
    case stringType(): return "text";
    default: throw "no type associated";
  }
}

