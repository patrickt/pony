module Language.C.Semantics.Conversions where
  
  import Data.Maybe
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (Named s _ _ _) = Just s
  nameOfDeclarator (Abstract _ _) = Nothing
  
  convertToSemanticType :: CDeclarator -> [Specifier] -> CType
  convertToSemanticType decl specs = undefined where
    -- foldr over 
    (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
  
  -- there is probably a better way to do this with Data.Generics or something
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers specs = extract ([], [], []) specs where
    extract r [] = r
    extract (as, bs, cs) ((TSpec t) : rest) = extract (t:as, bs, cs) rest
    extract (as, bs, cs) ((TQual q) : rest) = extract (as, q:bs, cs) rest
    extract (as, bs, cs) ((SSpec s) : rest) = extract (as, bs, cs:s) rest
  
  shortSignedInt = SInt (IntegerFlags Signed Short) []
  shortUnsignedInt = SInt (IntegerFlags Unsigned Short) []
  signedInt = SInt (IntegerFlags Signed Regular) []
  unsignedInt = SInt (IntegerFlags Unsigned Regular) []
  longSignedInt = SInt (IntegerFlags Signed Long) []
  longUnsignedInt = SInt (IntegerFlags Unsigned Long) []
  longLongSignedInt = SInt (IntegerFlags Signed LongLong) []
  longlongUnsignedInt = SInt (IntegerFlags Unsigned LongLong) []
  
  -- this is where type aliases go, as defined C99 6.7.2.2
  typeQualifiersToType :: [TypeQualifier] -> SType
  typeQualifiersToType [TVoid] = SVoid []
  
  typeQualifiersToType [TChar] = SChar Signed []
  
  typeQualifiersToType [TUnsigned, TChar] = SChar Unsigned []
  
  typeQualifiersToType [TSigned, TChar] = SChar Signed []
  
  typeQualifiersToType [TShort]                = shortSignedInt
  typeQualifiersToType [TSigned, TShort]       = shortSignedInt
  typeQualifiersToType [TShort, TInt]          = shortSignedInt
  typeQualifiersToType [TSigned, TShort, TInt] = shortSignedInt
  typeQualifiersToType [TBool]                 = shortSignedInt
  
  typeQualifiersToType [TUnsigned, TShort]       = shortUnsignedInt
  typeQualifiersToType [TUnsigned, TShort, TInt] = shortUnsignedInt
  
  typeQualifiersToType [TInt]          = signedInt
  typeQualifiersToType [TSigned]       = signedInt
  typeQualifiersToType [TSigned, TInt] = signedInt
  
  typeQualifiersToType [TUnsigned]       = unsignedInt
  typeQualifiersToType [TUnsigned, TInt] = unsignedInt
  
  typeQualifiersToType [TLong]                = signedLong
  typeQualifiersToType [TSigned, TLong]       = signedLong
  typeQualifiersToType [TLong, TInt]          = signedLong
  typeQualifiersToType [TSigned, TLong, TInt] = signedLong
  
  typeQualifiersToType [TUnsigned, TLong]       = longUnsignedInt
  typeQualifiersToType [TUnsigned, TLong, TInt] = longUnsignedInt
  
  typeQualifiersToType [TLong, TLong]                = longLongSignedInt
  typeQualifiersToType [TSigned, TLong, TLong]       = longLongSignedInt
  typeQualifiersToType [TLong, TLong, TInt]          = longLongSignedInt
  typeQualifiersToType [TSigned, TLong, TLong, TInt] = longLongSignedInt
  
  typeQualifiersToType [TUnsigned, TLong, TLong]       = longLongSignedInt
  typeQualifiersToType [TUnsigned, TLong, TLong, TInt] = longLongSignedInt
  
  typeQualifiersToType [TFloat] = SFloat FFloat []
  
  typeQualifiersToType [TDouble] = SFloat FDouble []
  
  typeQualifiersToType [TLong, TDouble] = SFloat FLongDouble []
  
  -- haha, no idea what to do here!
  typeQualifiersToType [(TStructOrUnion _ _ _)] = SComposite undefined []
  
  typeQualfiersToType [(TEnumeration _ _)] = SEnum undefined []
  
  -- or here either!
  typeQualfiersToType [(TTypedef _ _)] = undefined
  
  typeQualifiersToType other = error ("unknown type " ++ (show other))
  
  
  
  semantifyFunction :: CFunction -> SFunction
  semantifyFunction (CFunction spec decl body) = 
    SFunction undefined (fromJust $ nameOfDeclarator decl) undefined undefined
  
  
