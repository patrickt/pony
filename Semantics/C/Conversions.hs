module Semantics.C.Conversions where
  
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Miscellany
  import Semantics.C.Nodes
  import Data.Maybe
  
  convertTypeQualifiers :: TypeQualifier -> Attribute
  convertTypeQualifiers QConst    = Const
  convertTypeQualifiers QRestrict = Restrict
  convertTypeQualifiers QVolatile = Volatile
  
  -- TODO: remember to put in the array size when I have a handle on expressions
  convertDerivedDeclarators :: DerivedDeclarator -> SType -> SType
  convertDerivedDeclarators (Pointer qs) t = SPointerTo t (map convertTypeQualifiers qs)
  convertDerivedDeclarators (Array qs size) t = SArray t Nothing (map convertTypeQualifiers qs)
  
  -- should probably be CDeclaration -> Either SType SVariable
  convertDeclarationToType :: CDeclaration -> Maybe SType
  convertDeclarationToType (TopLevel _ _) = Nothing
  convertDeclarationToType (Parameter specs decl) = Just (convertComponents specs decl)
  convertDeclarationToType (TypeName specs (Just decl)) = Just (convertComponents specs decl)
  convertDeclarationToType (TypeName specs Nothing) = Just (convertComponents specs (Abstract [] []))
  
  -- There is probably a better way to do this with Data.Generics or something
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers specs = extract ([], [], []) specs where
    extract r [] = r
    extract (as, bs, cs) ((TSpec t) : rest) = extract (t:as, bs, cs) rest
    extract (as, bs, cs) ((TQual q) : rest) = extract (as, q:bs, cs) rest
    extract (as, bs, cs) ((SSpec s) : rest) = extract (as, bs, s:cs) rest
  
  -- TODO: Finish composite types, enumerations, and typedefs
  -- This is where type aliases go, as defined in C99, 6.7.2.2
  convertTypeSpecifiers :: [TypeSpecifier] -> SType
  convertTypeSpecifiers [TVoid]                         = void
  convertTypeSpecifiers [TChar]                         = signedChar
  convertTypeSpecifiers [TSigned, TChar]                = signedChar
  convertTypeSpecifiers [TUnsigned, TChar]              = unsignedChar
  convertTypeSpecifiers [TShort]                        = shortSignedInt
  convertTypeSpecifiers [TSigned, TShort]               = shortSignedInt
  convertTypeSpecifiers [TShort, TInt]                  = shortSignedInt
  convertTypeSpecifiers [TSigned, TShort, TInt]         = shortSignedInt
  convertTypeSpecifiers [TBool]                         = shortSignedInt
  convertTypeSpecifiers [TUnsigned, TShort]             = shortUnsignedInt
  convertTypeSpecifiers [TUnsigned, TShort, TInt]       = shortUnsignedInt
  convertTypeSpecifiers [TInt]                          = signedInt
  convertTypeSpecifiers [TSigned]                       = signedInt
  convertTypeSpecifiers [TSigned, TInt]                 = signedInt
  convertTypeSpecifiers [TUnsigned]                     = unsignedInt
  convertTypeSpecifiers [TUnsigned, TInt]               = unsignedInt
  convertTypeSpecifiers [TLong]                         = longSignedInt
  convertTypeSpecifiers [TSigned, TLong]                = longSignedInt
  convertTypeSpecifiers [TLong, TInt]                   = longSignedInt
  convertTypeSpecifiers [TSigned, TLong, TInt]          = longSignedInt
  convertTypeSpecifiers [TUnsigned, TLong]              = longUnsignedInt
  convertTypeSpecifiers [TUnsigned, TLong, TInt]        = longUnsignedInt
  convertTypeSpecifiers [TLong, TLong]                  = longLongSignedInt
  convertTypeSpecifiers [TSigned, TLong, TLong]         = longLongSignedInt
  convertTypeSpecifiers [TLong, TLong, TInt]            = longLongSignedInt
  convertTypeSpecifiers [TSigned, TLong, TLong, TInt]   = longLongSignedInt
  convertTypeSpecifiers [TUnsigned, TLong, TLong]       = longLongSignedInt
  convertTypeSpecifiers [TUnsigned, TLong, TLong, TInt] = longLongSignedInt
  convertTypeSpecifiers [TFloat]                        = float
  convertTypeSpecifiers [TDouble]                       = double
  convertTypeSpecifiers [TLong, TDouble]                = longDouble
  convertTypeSpecifiers [(TStructOrUnion _ _ _)]        = SComposite undefined []
  convertTypeSpecifiers [(TEnumeration _ _)]            = SEnum undefined []
  convertTypeSpecifiers [(TTypedef _ _)]                = undefined
  convertTypeSpecifiers other                           = error ("unknown type " ++ (show other))
  
  -- TODO: We're leaving storage specifiers out here, those should be included too.
  convertComponents :: [Specifier] -> CDeclarator -> SType
  convertComponents specs decl = foldr convertDerivedDeclarators (convertTypeSpecifiers typeSpecs) (derivedPartsOfDeclarator decl) where
    (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
    
  -- This is an easy conversion; all that is necessary is to drop the last
  -- item in the list of derived declarators.
  returnTypeOfFunction :: CFunction -> SType
  returnTypeOfFunction (CFunction specs (Named n derived asm attrs) _) = convertComponents specs (Named n (init derived) asm attrs)
  
  argumentsOfFunctionDeclarator :: CDeclarator -> [SType]
  argumentsOfFunctionDeclarator (Named _ derived _ _) = 
    case (last derived) of
      (Function args _) -> catMaybes $ map convertDeclarationToType args
  
  -- TODO: fill in the function body here.
  convertFunction :: CFunction -> SFunction
  convertFunction f@(CFunction spec decl body) = 
    let ftype = returnTypeOfFunction f
        name = fromJust $ nameOfDeclarator decl
        args = argumentsOfFunctionDeclarator decl
        body = []
        in SFunction ftype name args body
  