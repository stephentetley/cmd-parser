/*
 *  Copyright 2026 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

//  Translation of Luyu Chung and Lionel Parreaux's Pratt expression parser to Flix

pub mod PrattParser.ExpressionParser {


    pub enum LiteralKind with Eq, Order, ToString {
        case Integer
        case Float
        case Character
        case String
    }

    pub enum BracketKind with Eq, Order, ToString {
        case Angle
        case Round
        case Square
        case Curly
        case BeginEnd
    }


    pub enum Token with Eq, Order, ToString {
        case Literal(LiteralKind, String)
        case Identifier(String)
        case Symbol(String)
        case Open(BracketKind)
        case Close(BracketKind)
    }

    pub enum Tree with Eq, Order, ToString {
        case Literal(LiteralKind, String)
        case Reference(String)
        case Application(Tree, List[Tree])
        case Error(Option[Tree], String)
        case Empty
    }

    pub type alias Operator = { name = String, lbp = Int32, rbp = Int32, unary = Bool}


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


    pub def parse(tokens: List[Token]): Tree = region rc {
        let currentTokens = Ref.fresh(rc, tokens);
        def peek() = { 
            let xs = Ref.get(currentTokens);
            List.head(xs)
        };
        def consume() = {
            let xs = Ref.get(currentTokens);
            match xs {
                case _ :: rest => Ref.put(rest, currentTokens)
                case _ => ()
            }
        };
        def expr(bp: Int32) = {
            def exprCont(acc: Tree, bpc: Int32) = match peek() {
                case Some(Token.Symbol(name)) => match op(name)  {
                    case Some({ lbp, rbp | _}) if lbp > bpc => {
                        consume();
                        exprCont(Tree.Application(Tree.Reference(name), acc :: expr(rbp) :: Nil), bpc)
                    }
                    case _ => acc
                }
                case _ => acc
            };
            match peek() {
                case Some(Token.Literal(kind, literal)) => {
                    consume();
                    exprCont(Tree.Literal(kind, literal), bp)
                }
                case Some(Token.Identifier(name)) => {
                    consume();
                    exprCont(Tree.Reference(name), bp)
                }
                case Some(Token.Symbol(name)) => match op(name) {
                    case Some({ unary | _}) if unary => {
                        consume();
                        exprCont(Tree.Application(Tree.Reference(name), expr(prefixPower()) :: Nil), bp)
                    }
                    case _ => Tree.Error(None, "Unexpected token ${Token.Symbol(name)}")
                }
                case Some(Token.Open(BracketKind.Round)) => {
                    consume();
                    let tree = expr(0);
                    match peek() {
                        case Some(Token.Close(BracketKind.Round)) => {
                            consume();
                            exprCont(tree, bp)
                        }
                        case _ => Tree.Error(Some(tree), "missing closing parenthesis")
                    }
                }
                case Some(token) => Tree.Error(None, "Unexpected token ${token}")
                case None => Tree.Error(None, "Unexpected end of input")
            }
        };
        let result = expr(0);
        match peek() {
            case Some(token) => Tree.Error(Some(result), "Unexpected token ${token}")
            case None => result
        }
    }



}

