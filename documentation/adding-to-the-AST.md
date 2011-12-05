In this annotated walkthrough, I'll illustrate how to add features to Pony; specifically, I'll walk through the process of adding new syntactic and semantic structures, with a zesty dash of pretty-printing on top.

Specifically, we'll focus on adding GCC inline assembly syntax to Pony. Syntactically, GCC inline assembly takes the following form:
    asm volatile? (code : input_operands? : output_operands? : clobbered_registers?)
Yes, this insane. No, I do not know why they chose colons over commas. Ask the GCC maintainers, if you can lure them down from the Mountains of Madness.

Regardless, let's add this syntax to Pony. The first step is to add a node in the syntax tree that accommodates this syntax. We'll be adding this node to the `CStatement` data type in Language/C/AST.hs:

    data CStatement
       +    -- | The GCC syntax for inline assembly.
       +    = AsmStmt (Maybe TypeQualifier) CExpr (Maybe CExpr) (Maybe CExpr) (Maybe CExpr)

(The lines prefixed by a plus sign are those that are added.)
As you can see, this is a straightforward representation of the assembly syntax in an ADT. The optional syntactic elements (suffixed with a question-mark in the original syntax) are wrapped in a Maybe type to indicate that they are optional.

The next thing we'll do is make `asm` a reserved keyword in the lexer (Language/C/Lexer.hs). This is a simple matter of adding the string "asm" to the list of reserved keywords:
    +    , T.reservedNames = ["asm", "auto", "break", "case", "char", "const", "continue", "default", "do", 
                             "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", 

Now comes the interesting part. We need to specify how the `asm()` statement is parsed. To do so, we open up Language/C/Statements.hs and add a new `asmStmt` parser action. 

    +  asmStmt :: Parser CStatement
    +  asmStmt  =  (L.reserved "asm" *> pure AsmStmt) 
    +          <*> optional volatile
    +          <*> (L.symbol "(" *> stringLiteral)
    +          <*> (L.symbol ":" *> optional stringLiteral)
    +          <*> (L.symbol ":" *> optional stringLiteral)
    +          <*> ((L.symbol ":" *> optional stringLiteral) <* L.symbol ")") where
    +            volatile = pure QVolatile <* L.reserved "volatile"

Note that we use applicative-style parsing, since monadic style is unnecessary in this case. The `*>` and `<*` perform sequencing, the `optional` combinator makes a statement optional, and the `<*>` operator separates the components in the ADT (actually, this is a simplification of what it does, but it'll do for now).

We then add this new `asmStmt` to the top-level `statement` parser:
        -- | A C statement. (C99 6.8.*)
        statement :: Parser CStatement
        statement = choice [ try labeledStmt 
     +                     , asmStmt
                           , compoundStmt 
                           , jumpStmt 
                           , expressionStmt

If we pop into the Pony REPL (to do this, load Repl.hs in `ghci`), we can test that this parses correctly:

    *Main> parseTest asmStmt "asm(\"code\" : \"reg\" ::)"
    AsmStmt Nothing (Constant code) (Just (Constant reg)) Nothing Nothing
    *Main> parseTest asmStmt "asm(\"morecode\":::)"
    AsmStmt Nothing (Constant morecode) Nothing Nothing Nothing

Okay, looks like it works. The next step is to define how this is represented in the semantic tree. There won't be much difference, other than converting the syntactic `CExpr` type to the semantic `Expression` type; however, instead of using the `Maybe TypeQualifier` type to represent whether the `volatile` qualifier is present, we'll just use a `Bool` type. (Using a type qualifier node implies that other type qualifiers (`const`, `static`, etc.) are valid, which they are not.) In Semantics/C/Nodes.hs:

       data Statement
    -    = Break
    +    = Asm Bool Expression (Maybe Expression) (Maybe Expression) (Maybe Expression)
    +    | Break
         | Case Expression Statement

Now we detail how to convert the syntactic structure to the semantic structure. In Semantics/C/Conversions.hs, we already have the required instance of `Reifiable` that dictates that `CStatement`s are convertible to `Statement`s. All we need to do is add another clause that converts `AsmStmt` objects to `Asm` objects:

       instance Reifiable CStatement Statement where
    +    convert (AsmStmt tq asm in' out clobber) 
    +      = Asm (isJust tq) (convert asm) (convert <$> in') (convert <$> out) (convert <$> clobber) 
         convert BreakStmt = Break

Notice that we use the `isJust` function to convert from a `Maybe TypeQualifier` to a `Bool`, and the infix fmap syntax `<$>` to lift the `convert` function into the Maybe type. 

Almost done! The last thing we have to do is specify how to pretty-print assembly statements. For concision's sake, let's define a new `<:>` combinator that separates two documents with a colon. In Text/Pretty.hs:

    +  (<:>) :: Doc -> Doc -> Doc
    +  a <:> b = a <> text ":" <> b

Now defining a pretty-printer for the `Asm` ADT is extremely concise:

       instance Pretty Statement where
    +    pretty (Asm True a b c d) = text "asm volatile" <> parens (pretty a <:> pretty b <:> pretty c <:> pretty d)
    +    pretty (Asm False a b c d) = text "asm" <> parens (pretty a <:> pretty b <:> pretty c <:> pretty d) 
         pretty Break = text "break;"

And we're done!
