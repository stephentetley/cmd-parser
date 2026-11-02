/*
 *  Copyright 2026 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

//  Translation of Luyu Chung and Lionel Parreaux's Pratt expression parser to Flix

pub mod PrattParser.ExpressionParser2 {

    use PrattParser.ExpressionParser.Token
    use PrattParser.ExpressionParser.LiteralKind
    use PrattParser.ExpressionParser.BracketKind
    use PrattParser.ExpressionParser.Operator
    use PrattParser.ExpressionParser.Tree


    pub def operatorTable(): Map[String, Operator] = 
        let ops = 
            ("+" , { name = "+" , lbp = 3, rbp = 4, unary = true }) ::
            ("-" , { name = "-" , lbp = 3, rbp = 4, unary = true }) :: 
            ("*" , { name = "*" , lbp = 5, rbp = 6, unary = false }) ::
            ("/" , { name = "/" , lbp = 5, rbp = 6, unary = false }) ::
            ("**", { name = "**", lbp = 8, rbp = 7, unary = false }) ::
            Nil;
        List.toMap(ops)

    pub def op(name: String): Option[Operator] = 
        Map.get(name, operatorTable())


    pub def prefixPower(): Int32 = 9


    pub eff Parse {
        def peek(): Option[Token]
        def consume(): Unit
    }

    def handle(tokens: List[Token], f: a -> b \ ef): a -> b \ ef - Parse = x -> region rc {
        let currentTokens = Ref.fresh(rc, tokens); 
        run {
            f(x)
        } with handler Parse {
            def peek(k) = { 
               let xs = Ref.get(currentTokens);
                k(List.head(xs))
            }
            def consume(k) = {
                let xs = Ref.get(currentTokens);
                match xs {
                    case _ :: rest => Ref.put(rest, currentTokens)
                    case _ => ()
                };
                k()
            }
        }
    }

    pub def runParse(tokens: List[Token], f: Unit -> a \ ef): a \ ef - Parse =
        handle(tokens, f)()

    pub def parse(tokens: List[Token]): Tree = 
        def go() = {
            let result = expr(0);
            match Parse.peek() {
                case Some(token) => Tree.Error(Some(result), "Unexpected token ${token}")
                case None => result
            }
        };
        runParse(tokens, go)


    def exprCont(acc: Tree, bpc: Int32): Tree \ Parse = match Parse.peek() {
        case Some(Token.Symbol(name)) => match op(name)  {
                    case Some({ lbp, rbp | _}) if lbp > bpc => {
                        Parse.consume();
                        exprCont(Tree.Application(Tree.Reference(name), acc :: expr(rbp) :: Nil), bpc)
                    }
                    case _ => acc
                }
                case _ => acc
            }

    def expr(bp: Int32): Tree \ Parse = {
        match Parse.peek() {
            case Some(Token.Literal(kind, literal)) => {
                Parse.consume();
                exprCont(Tree.Literal(kind, literal), bp)
            }
            case Some(Token.Identifier(name)) => {
                Parse.consume();
                exprCont(Tree.Reference(name), bp)
            }
            case Some(Token.Symbol(name)) => match op(name) {
                case Some({ unary | _}) if unary => {
                    Parse.consume();
                    exprCont(Tree.Application(Tree.Reference(name), expr(prefixPower()) :: Nil), bp)
                }
                case _ => Tree.Error(None, "Unexpected token ${Token.Symbol(name)}")
            }
            case Some(Token.Open(BracketKind.Round)) => {
                Parse.consume();
                let tree = expr(0);
                match Parse.peek() {
                    case Some(Token.Close(BracketKind.Round)) => {
                        Parse.consume();
                        exprCont(tree, bp)
                    }
                    case _ => Tree.Error(Some(tree), "missing closing parenthesis")
                }
            }
            case Some(token) => Tree.Error(None, "Unexpected token ${token}")
            case None => Tree.Error(None, "Unexpected end of input")
        }
    }


}

