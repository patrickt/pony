module Language.C.Semantics.Conversions where
  
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Semantics.Nodes
  import Data.Maybe
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (Named s _ _ _) = Just s
  nameOfDeclarator (Abstract _ _) = Nothing
  
  derivedPartsOfDeclarator :: CDeclarator -> [DerivedDeclarator]
  derivedPartsOfDeclarator (Named _ ds _ _) = ds
  derivedPartsOfDeclarator (Abstract ds _) = ds
  
  rectifyDerivations :: DerivedDeclarator -> SType -> SType
  -- ignoring attributes for now
  rectifyDerivations (Pointer _) t = SPointerTo t []
  rectifyDerivations (Array _ _) t = SArray t Nothing []
  
  convertToSemanticType :: CDeclarator -> [Specifier] -> SType
  convertToSemanticType decl specs = foldr rectifyDerivations (typeSpecifiersToType typeSpecs) (derivedPartsOfDeclarator decl) where
    (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
  
  -- there is probably a better way to do this with Data.Generics or something
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers specs = extract ([], [], []) specs where
    extract r [] = r
    extract (as, bs, cs) ((TSpec t) : rest) = extract (t:as, bs, cs) rest
    extract (as, bs, cs) ((TQual q) : rest) = extract (as, q:bs, cs) rest
    extract (as, bs, cs) ((SSpec s) : rest) = extract (as, bs, s:cs) rest
  
  shortSignedInt = SInt (IntegerFlags Signed Short) []
  shortUnsignedInt = SInt (IntegerFlags Unsigned Short) []
  signedInt = SInt (IntegerFlags Signed Regular) []
  unsignedInt = SInt (IntegerFlags Unsigned Regular) []
  longSignedInt = SInt (IntegerFlags Signed Long) []
  longUnsignedInt = SInt (IntegerFlags Unsigned Long) []
  longLongSignedInt = SInt (IntegerFlags Signed LongLong) []
  longlongUnsignedInt = SInt (IntegerFlags Unsigned LongLong) []
  
  -- this is where type aliases go, as defined C99 6.7.2.2
  typeSpecifiersToType :: [TypeSpecifier] -> SType
  typeSpecifiersToType [TVoid] = SVoid []
  typeSpecifiersToType [TChar] = SChar Signed []
  typeSpecifiersToType [TUnsigned, TChar] = SChar Unsigned []
  typeSpecifiersToType [TSigned, TChar] = SChar Signed []
  typeSpecifiersToType [TShort]                = shortSignedInt
  typeSpecifiersToType [TSigned, TShort]       = shortSignedInt
  typeSpecifiersToType [TShort, TInt]          = shortSignedInt
  typeSpecifiersToType [TSigned, TShort, TInt] = shortSignedInt
  typeSpecifiersToType [TBool]                 = shortSignedInt
  typeSpecifiersToType [TUnsigned, TShort]       = shortUnsignedInt
  typeSpecifiersToType [TUnsigned, TShort, TInt] = shortUnsignedInt
  typeSpecifiersToType [TInt]          = signedInt
  typeSpecifiersToType [TSigned]       = signedInt
  typeSpecifiersToType [TSigned, TInt] = signedInt
  typeSpecifiersToType [TUnsigned]       = unsignedInt
  typeSpecifiersToType [TUnsigned, TInt] = unsignedInt
  typeSpecifiersToType [TLong]                = longSignedInt
  typeSpecifiersToType [TSigned, TLong]       = longSignedInt
  typeSpecifiersToType [TLong, TInt]          = longSignedInt
  typeSpecifiersToType [TSigned, TLong, TInt] = longSignedInt
  typeSpecifiersToType [TUnsigned, TLong]       = longUnsignedInt
  typeSpecifiersToType [TUnsigned, TLong, TInt] = longUnsignedInt
  typeSpecifiersToType [TLong, TLong]                = longLongSignedInt
  typeSpecifiersToType [TSigned, TLong, TLong]       = longLongSignedInt
  typeSpecifiersToType [TLong, TLong, TInt]          = longLongSignedInt
  typeSpecifiersToType [TSigned, TLong, TLong, TInt] = longLongSignedInt
  typeSpecifiersToType [TUnsigned, TLong, TLong]       = longLongSignedInt
  typeSpecifiersToType [TUnsigned, TLong, TLong, TInt] = longLongSignedInt
  typeSpecifiersToType [TFloat] = SFloat FFloat []
  typeSpecifiersToType [TDouble] = SFloat FDouble []
  typeSpecifiersToType [TLong, TDouble] = SFloat FLongDouble []
  typeSpecifiersToType [(TStructOrUnion _ _ _)] = SComposite undefined []
  typeSpecifiersToType [(TEnumeration _ _)] = SEnum undefined []
  typeSpecifiersToType [(TTypedef _ _)] = undefined
  typeSpecifiersToType other = error ("unknown type " ++ (show other))
  
  semantifyFunction :: CFunction -> SFunction
  semantifyFunction (CFunction spec decl body) = 
    SFunction undefined (fromJust $ nameOfDeclarator decl) undefined undefined
  
  
