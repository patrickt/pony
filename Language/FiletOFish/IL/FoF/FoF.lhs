%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Language.FiletOFish.IL.FoF.FoF where

> import Language.FiletOFish.Constructs
> import Language.FiletOFish.PureExpressions

%endif

\section{The FoF Intermediate Language}
\label{sec:il_fof_fof}

The FoF IL is nothing more than a direct translation of the
Filet-o-Fish operators. In retrospect, calling it |FoF| might be
confusing. Never forget that lives in the @IL/@ directory, so it is
simply not the abbreviation for Filet-o-Fish, and that's it.

Having said that, it is also obvious that, essentially, FoF is
Filet-o-Fish: it is a dumb translation of the Filet-o-Fish constructs
into a data-type. Hence, an |ILFoF| term is the reification of the
language constructs:

> data ILFoF
>     = FConstant PureExpr
>     | FStatement FStatement ILFoF
>     | FClosing FClosing
>     | FNewDef [FunAttr] String ILFoF TypeExpr [PureExpr] ILFoF
>     | FIf ILFoF ILFoF ILFoF ILFoF
>     | FFor ILFoF ILFoF ILFoF ILFoF ILFoF
>     | FWhile ILFoF ILFoF ILFoF
>     | FDoWhile ILFoF ILFoF ILFoF
>     | FSwitch PureExpr [(PureExpr, ILFoF)] ILFoF ILFoF

Where an |FStatement| is one of the sequential statement of the
Filet-o-Fish language, that is:

> data FStatement 
>     = FNewUnion VarName AllocUnion String [(String,TypeExpr)] (String, FoFData)
>     | FReadUnion VarName Loc String
>     | FWriteUnion Loc String FoFData 
>     | FTypedef TypeExpr String
>     | FTypedefE String TypeExpr 
>     | FNewStruct VarName AllocStruct String [(String,(TypeExpr,FoFData))] 
>     | FReadStruct VarName Loc String
>     | FWriteStruct Loc String FoFData
>     | FNewString VarName String
>     | FNewRef  VarName FoFData
>     | FReadRef VarName Loc
>     | FWriteRef Loc FoFData
>     | FNewEnum VarName String Enumeration String 
>     | FNewArray VarName AllocArray [FoFData] 
>     | FReadArray VarName Loc Index 
>     | FWriteArray Loc Index FoFData 
>     | FCallDef (Maybe VarName) PureExpr [PureExpr]
>     | FFFICall String [PureExpr]

And an |FClosing| is a standard C \emph{end of something} statement:

> data FClosing 
>     = FReturn PureExpr
>     | FBreak 
>     | FContinue 



