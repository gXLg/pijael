const fs = require("fs");

class Token {
  constructor(value, type){
    this.value = value;
    this.type = type;
  }
}

class Node {
  constructor(visitor, tokens, type){
    this.v = args => visitor(args, ...tokens);
    this.tokens = tokens;
    this.type = type;
  }
}

function toString(tokens){
  const res = [];
  [].concat(tokens).forEach(t => {
    if(!isNaN(t.valueOf() - 0))
      res.push(t.valueOf());
  });
  return Buffer.from(res).toString();
}

class Byte {
  constructor(value){
    this.value = (parseInt(value) + 256) % 256;
    if(isNaN(this.value)) this.value = 0;
  }

  valueOf(){
    return this.value;
  }
}

class Lexer {
  constructor(regExp, whitespace){
    this.regExp = regExp;
    this.whitespace = whitespace;
  }

  lex(code){
    const tokens = [];
    if(code.search(this.whitespace) == 0)
      code = code.replace(this.whitespace, "");
    while(code.length){
      let was = false;
      for(let type in this.regExp){
        const regex = this.regExp[type];
        if(code.search(regex) == 0){
          const found = code.match(regex)[0];
          code = code.replace(found, "");
          if(code.search(this.whitespace) == 0)
            code = code.replace(this.whitespace, "");
          let token;
          if(type == "num")
            token = new Token(new Byte(found), type);
          else
            token = new Token(found, type);
          tokens.push(token);
          was = true;
          break;
        }
      }
      if(!was)
        throw new Error("Unknown token at " + code.slice(0, 20));
    }
    tokens.push(new Token(null, "end"));
    return tokens;
  }
}

class Parser {
  constructor(tokens){
    this.tokens = tokens;
    this.n();

    this.private = [{ }];

    this.vars = {
      "open": new Node(
        args => {
          return new Byte(
            fs.openSync(
              toString(args[0]), toString(args[1])
            )
          );
        }, [], "built-in"
      ),
      "put": new Node(
        args => {
          const b = toString(args[0]);
          fs.writeSync(args[1].valueOf(), b);
          return new Byte(b.length);
        }, [], "built-in"
      ),
      "close": new Node(
        args => {
          fs.closeSync(args[0].valueOf());
          return new Byte(0);
        }, [], "built-in"
      )
    };
  }

  n(){
    this.current = this.tokens.splice(0, 1)[0];
    if(this.current.type == "cmt") this.n();
    return this.current;
  }

  parse(){
    if(this.current.type == "end") return;

    const res = this.lines();

    if(this.current.type != "end")
      throw new Error("Unparsed at " + this.current.type);

    return res;
  }

  // n; n
  lines(){
    const lines = [];
    let res = this.pipe();
    lines.push(res);
    while(
      this.current.type != "end" &&
      ["lin"].includes(this.current.type)
    ){
      this.n();
      lines.push(this.pipe());
      res = new Node(
        (args, ...a) => a.reduce((b, c) => [b.v(args), c][1]).v(args),
        lines, "lines"
      );
    }
    /*return new Node(
      (args, a) => {
        this.private.push({ });
        const ret = a.v(args);
        this.private.pop();
        return ret;
      },
      [res], "lines-wrapper"
    );*/
    return res;
  }

  pipe(){
    let res = this.body();
    while(
      this.current.type != "end" &&
      ["pip"].includes(this.current.type)
    ){
      this.n();
      res = new Node(
        (args, a, b) => b.v([].concat(a.v(args))),
        [res, this.body()], "pipe"
      );
    }
    return res;
  }

  body(){
    if(this.current.type == "bdl"){
      this.n();
      const res = this.lines();
      if(this.current.type != "bdr")
        throw new Error("Not closed body bracket");
      this.n();
      return new Node(
        (args, a) => a.v().v(args), [res], "body"
      );
    } else {
      return this.list();
    }
  }

  // n,n,n
  list(){
    const list = [];
    let res = this.declare();
    list.push(res);
    while(
      this.current.type != "end" &&
      ["del"].includes(this.current.type)
    ){
      this.n();
      list.push(this.declare());
      res = new Node(
        (args, ...a) => a.map(b => b.v(args)),
        list, "list"
      );
    }
    return res;
  }

  declare(){
    let res = this.assign();
    if(
      this.current.type != "end" &&
      ["dec"].includes(this.current.type)
    ){
     this.n();
     res = new Node(
       (args, a, b) => {
         const name = a.tokens[0].value;
         const val = new Node(
           (args, c) => args ? c.v(args) : c, [b], "function"
         );
         if(name[0] == "_")
           this.private[this.private.length - 1][name] = val;
         else
           this.vars[name] = val;
         return args ? b.v(args) : b;
       },
       [res, this.pipe()], "declaration"
     );
    }
    return res;
  }

  assign(){
    let res = this.iterate();
    while(
      this.current.type != "end" &&
      ["ass"].includes(this.current.type)
    ){
      this.n();
      res = new Node(
        (args, a, b) => {
          const r = b.v(args);
          const name = a.tokens[0].value;
          const val = new Node(
            (args, c) => c, [r], "num"
          );
          if(name[0] == "_")
            this.private[this.private.length - 1][name] = val;
          else
            this.vars[name] = val;
          return r;
        },
        [res, this.assign()], "assign"
      );
    }
    return res;
  }

  iterate(){
    let res = this.exist();
    while(
      this.current.type != "end" &&
      ["ite"].includes(this.current.type)
    ){
      this.n();
      res = new Node(
        (args, a, b) =>
          [].concat(a.v(args).map((e, i) =>
            b.v([].concat(e).concat(new Byte(i))))),
        [res, this.pipe()], "iterator"
      );
    }
    return res;
  }

  exist(){
    let res = this.push();
    while(
      this.current.type != "end" &&
      ["exi"].includes(this.current.type)
    ){
      this.n();
      res = new Node(
        (args, a, b) => {
            let ret = new Byte(0);
            while(a.v(args) - 0)
              ret = b.v([].concat(ret));
            return ret;
        },
        [res, this.pipe()], "existence"
      );
    }
    return res;
  }

  push(){
    let res = this.expr();
    while(
      this.current.type != "end" &&
      ["pus"].includes(this.current.type)
    ){
      this.n();
      res = new Node(
        (args, a, b) =>
          [].concat(a.v(args)).concat(b.v(args)),
        [res, this.pipe()], "push"
      );
    }
    return res;
  }

  // n+n n-n
  expr(){
    let res = this.term();
    while(
      this.current.type != "end" &&
      ["add", "sub"].includes(this.current.type)
    ){
      const token = this.current;
      const type = token.type;
      this.n();
      res = new Node(
        (args, a, b) => type == "add" ?
          new Byte(a.v(args) + b.v(args)) :
          new Byte(a.v(args) - b.v(args)),
        [res, this.term()], type
      );
    }
    return res;
  }

  // n*n n/n
  term(){
    let res = this.factor();
    while(
      this.current.type != "end" &&
      ["mul", "div"].includes(this.current.type)
    ){
      const token = this.current;
      const type = token.type;
      this.n();
      res = new Node(
        (args, a, b) => type == "mul" ?
          new Byte(a.v(args) * b.v(args)) :
          new Byte(a.v(args) / b.v(args)),
        [res, this.factor()], type
      );
    }
    return res;
  }

  // (n) +n -n n
  factor(){
    const token = this.current;
    if(token.type == "brl"){
      this.n();
      const res = this.lines();
      if(this.current.type != "brr")
        throw new Error("Not closed bracket");
      this.n();
      return res;
    } else if(token.type == "num"){
      this.n();
      return new Node((args, a) => a.value, [token], "num");
    } else if(token.type == "lit"){
      this.n();
      return new Node(
        (args, a) => {
          const name = a.value;
          let ret;
          this.private.push({ });
          if(name[0] == "_")
            ret = this.private[this.private.length - 2][name]?.v(args);
          else
            ret = this.vars[name]?.v(args);
          this.private.pop();
          return ret ?? new Byte(0);
        },
        [token], "lit"
      );
    } else if(["add", "sub"].includes(token.type)){
      this.n();
      const type = token.type;
      return new Node(
        (args, a) => type == "add" ?
          new Byte(a.v(args)) :
          new Byte(- a.v(args)),
        [this.factor()], type);
    } else if(["arg"].includes(token.type)){
      this.n();
      return new Node(
        (args, a) => args?.[a.v(args) - 0] ?? new Byte(0),
        [this.factor()], "arg"
      );
    } else if(this.current.type == "end"){
      return;
    } else {
      throw new Error("Unexpected token " + token.type);
    }
  }
}

const reg = {
  "cmt": /\/\*.*?\*\//ms,
  "num": /[0-9]+/,
  "brl": /\(/,
  "brr": /\)/,
  "add": /\+/,
  "sub": /\-/,
  "mul": /\*/,
  "div": /\//,
  "del": /\,/,
  "lin": /;/,
  "arg": /\$/,
  "pip": /\|/,
  "ass": /\=/,
  "bdl": /\[/,
  "bdr": /\]/,
  "lit": /[a-zA-Z_][0-9a-zA-Z_]*/,
  "dec": /\:/,
  "ite": /\#/,
  "exi": /\@/,
  "pus": /\</
};

const ws = /[\t \n]*/;

/*const code = `

if_equal: $0 - $1 | $0 / $0;

a = 54;
b = 54;

[(_:
  x = 69
), (_:
  x = 169
) | $(a, b | if_equal)];

mod: ($0 - ($0 / $1) * $1);

_1 = 48 + (x, 10 | mod);
x = x / 10;
_10 = 48 + (x, 10 | mod);
x = x / 10;
_100 = 48 + (x, 10 | mod);

(_100, _10, _1, 10), 1 | put

`;*/

const code = `

unique: (

  (_l = $0) # (
    _n = $(_a = 0);
    _l # _a = _a + 1 - ($0 - _n | $0 / $0);
    _u = _u + 1 / _a
  ); _u
);


list = (0, 4, 1, 2, 3, 4, 6, 0, 1, 2, 5, 9, 6);
list, 0 | unique

`;


const lexer = new Lexer(reg, ws);
const tokens = lexer.lex(code);
const parser = new Parser(tokens);
const tree = parser.parse();
const result = tree.v();
console.log(result);