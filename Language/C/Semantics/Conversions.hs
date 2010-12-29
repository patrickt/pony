module Language.C.Semantics.Conversions where
  
  import Data.Maybe
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (Named s _ _ _) = Just s
  nameOfDeclarator (Abstract _ _) = Nothing
  
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers = undefined -- BECAUSE I HAVE NO IDEA HOW TO WRITE IT, HAHA, JOKE'S ON YOU
  
  -- this is where type aliases go, as defined C99 6.7.2.2
  typeQualifiersToType :: [TypeQualifier] -> SType
  typeQualifiersToType [TVoid] = SVoid []
  
  typeQualifiersToType [TChar] = SChar Signed []
  
  typeQualifiersToType [TUnsigned, TChar] = SChar Unsigned []
  
  typeQualifiersToType [TSigned, TChar] = SChar Signed []
  
  typeQualifiersToType [TShort] = SInt undefined []
  typeQualifiersToType [TSigned, TShort] = SInt undefined []
  typeQualifiersToType [TShort, TInt] = SInt undefined []
  typeQualifiersToType [TSigned, TShort, TInt] = SInt undefined []
  typeQualifiersToType [TBool] = SInt undefined []
  
  typeQualifiersToType [TUnsigned, TShort] = SInt undefined []
  typeQualifiersToType [TUnsigned, TShort, TInt] = SInt undefined []
  
  typeQualifiersToType [TInt] = SInt undefined []
  typeQualifiersToType [TSigned] = SInt undefined []
  typeQualifiersToType [TSigned, TInt] = SInt undefined []
  
  typeQualifiersToType [TUnsigned] = SInt undefined []
  typeQualifiersToType [TUnsigned, TInt] = SInt undefined []
  
  typeQualifiersToType [TLong] = SInt undefined []
  typeQualifiersToType [TSigned, TLong] = SInt undefined []
  typeQualifiersToType [TLong, TInt] = SInt undefined []
  typeQualifiersToType [TSigned, TLong, TInt] = SInt undefined []
  
  typeQualifiersToType [TUnsigned, TLong] = SInt undefined []
  typeQualifiersToType [TUnsigned, TLong, TInt] = SInt undefined []
  
  typeQualifiersToType [TLong, TLong] = SInt undefined []
  typeQualifiersToType [TSigned, TLong, TLong] = SInt undefined []
  typeQualifiersToType [TLong, TLong, int] = SInt undefined []
  typeQualifiersToType [TSigned, TLong, TLong, TInt] = SInt undefined []
  
  typeQualifiersToType [TUnsigned, TLong, TLong] = SInt undefined []
  typeQualifiersToType [TUnsigned, TLong, TLong, TInt] = SInt undefined []
  
  typeQualifiersToType [TFloat] = SFloat undefined []
  
  typeQualifiersToType [TDouble] = SFloat undefined []
  
  typeQualifiersToType [TLong, TDouble] = SFloat undefined []
  
  -- haha, no idea what to do here!
  typeQualifiersToType [(TStructOrUnion _ _ _)] = SComposite undefined []
  
  typeQualfiersToType [(TEnumeration _ _)] = SEnum undefined []
  
  -- or here either!
  typeQualfiersToType [(TTypedef _ _)] = undefined
  
  typeQualifiersToType other = error ("unknown type " ++ (show other))
  
  
  
  semantifyFunction :: CFunction -> SFunction
  semantifyFunction (CFunction spec decl body) = 
    SFunction undefined (fromJust $ nameOfDeclarator decl) undefined undefined
  
  