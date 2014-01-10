int main (int argc, char const *argv[])
{
    char *first = "Hello";
    char *second = "from Pony!";
    char *third;
    third = first <+> second;
    printf(third);
    return 0;
}

Program [
  Function {ofType = Int, name = "main", arguments = [
    [ Variable {ofType = Int, name = "argc"}
    , Variable {ofType = Array { innerType = Pointer (Const Char), length = Void }, name = "argv"}]
  ]} [
      Declaration { variable = { typeOf = Pointer Char, name = "first"}, _initializer = StrLiteral "Hello!"}
      Declaration { variable = { typeOf = Pointer Char, name = "second"}, _initializer = StrLiteral "Hello!"}
      Declaration { variable = { typeOf = Pointer Char, name = "third"}, _initializer = Void }
      Binary { op = Assign, left = third, right = Binary { op = Concat, left = first, right = second }}
      Call { target = Ident { name = "printf"}, arguments = [Ident { name = "third"}]}
      Return { value = IntLiteral 0 }
  ]
  
]