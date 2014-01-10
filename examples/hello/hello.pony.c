#include <stdio.h>



int main (int argc, char const *argv[])
{
  hello;
  return 0;
}

// Program [
//   Function {ofType = Int, name = "main", arguments = [
//     [ Variable {ofType = Int, name = "argc"}
//     , Variable {ofType = Array { innerType = Pointer (Const Char), length = Void }, name = "argv"}]
//   ]} [
//     Ident { name = "hello" };
//     Return { value = IntLiteral 0 }
//   ]
//   
// ]