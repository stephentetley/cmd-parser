/*
 *  Copyright 2026 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */


pub mod Test01 {
    use PrattParser.ExpressionParser.Token
    use PrattParser.ExpressionParser.LiteralKind

    @Test 
    pub def test01(): Unit \ IO = 
        let xs = 
            Token.Literal(LiteralKind.Integer, "1") ::
            Token.Symbol("*") :: 
            Token.Literal(LiteralKind.Integer, "2") ::
            Nil ;
        let ans = PrattParser.ExpressionParser.parse(xs);
        println(ans)


}