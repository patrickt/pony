int main (int argc, char const *argv[])
{
	int *x = malloc(sizeof(int) * 4096);
	return 0;
}

// Program [
//   Function {ofType = Int, name = "main", arguments = [
//     [ Variable {ofType = Int, name = "argc"}
//     , Variable {ofType = Array { innerType = Pointer (Const Char), length = Void }, name = "argv"}]
//   ]} [
//       Declaration { variable = { typeOf = Pointer Int, name = "x"}, _initializer = 
//           [Binary Mul (Unary SizeOf IntT) (IntLiteral 4096)]
//   ]
//   
// ]
