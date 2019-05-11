module Operations.SignExcOps where

import           AbstractTypes.SignExc
import           AbstractTypes.TTExc
import           Operations.Operations
import qualified Data.Matrix                   as Mat
import           Data.Matrix                    ( (!), Matrix )
import           Data.Bool                      ( bool )

signExcOps :: Operations SignExc TTExc
signExcOps = Operations
    { absI          = \i -> case i `compare` 0 of
                          LT -> NEG
                          EQ -> ZERO
                          GT -> POS
    , absB          = bool FF TT
    , negate'       = \i -> negateMap ! (1, signVal i)
    , add           = \i1 i2 -> addMap ! (signVal i1, signVal i2)
    , subtr         = \i1 i2 -> subMap ! (signVal i1, signVal i2)
    , multiply      = \i1 i2 -> multMap ! (signVal i1, signVal i2)
    , divide        = \i1 i2 -> divMap ! (signVal i1, signVal i2)
    , eq            = \i1 i2 -> eqMap ! (signVal i1, signVal i2)
    , leq           = \i1 i2 -> leqMap ! (signVal i1, signVal i2)
    , and'          = \t1 t2 -> andMap ! (ttVal t1, ttVal t2)
    , neg           = \t -> negMap ! (1, ttVal t)
    , possiblyAErr  = (`elem` [ERR_A, ANY_A])
    , possiblyBErr  = (`elem` [ERR_B, ANY_B])
    , possiblyTrue  = (`elem` [TT, T, ANY_B])
    , possiblyFalse = (`elem` [FF, T, ANY_B])
    , possiblyInt   = (/= ERR_A)
    , isInt         = (`notElem` [ERR_A, ANY_A])
    }

andMap :: Matrix TTExc
andMap = Mat.fromLists [
    {-             NONE_B  TT      FF      ERR_B   T       B_ANY -}
    {- NONE_B -} [ NONE_B, NONE_B, NONE_B, NONE_B, NONE_B, NONE_B ],
    {-     TT -} [ NONE_B, TT,     FF,     ERR_B,  T,      ANY_B  ],
    {-     FF -} [ NONE_B, FF,     FF,     ERR_B,  T,      ANY_B  ],
    {-  ERR_B -} [ NONE_B, ERR_B,  ERR_B,  ERR_B,  ERR_B,  ERR_B  ],
    {-      T -} [ NONE_B, T,      T,      ERR_B,  T,      ANY_B  ],
    {-  B_ANY -} [ NONE_B, ANY_B,  ANY_B,  ERR_B,  ANY_B,  ANY_B  ]
    ]

negMap :: Matrix TTExc
negMap = Mat.fromLists [
    {-    NONE_B  TT  FF  ERR_B  T  B_ANY -}
        [ NONE_B, FF, TT, ERR_B, T, ANY_B ]
    ]

negateMap :: Matrix SignExc
negateMap = Mat.fromLists [
    {-    NONE_A  NEG  ZERO  POS  ERR_A  NON_POS  NON_ZERO  NON_NEG  Z  ANY_A -}
        [ NONE_A, POS, ZERO, NEG, ERR_A, NON_NEG, NON_ZERO, NON_POS, Z, ANY_A ]
    ]

multMap :: Matrix SignExc
multMap = Mat.fromLists [
    {-                 NONE_A   NEG       ZERO     POS       ERR_A   NON_POS  NON_ZERO   NON_NEG   Z        ANY_A -}
    {-    NONE_A -}  [ NONE_A,  NONE_A,   NONE_A,  NONE_A,   NONE_A, NONE_A,  NONE_A,    NONE_A,   NONE_A,  NONE_A ],
    {-       NEG -}  [ NONE_A,  POS,      ZERO,    NEG,      ERR_A,  NON_NEG, NON_ZERO,  NON_POS,  Z,       ANY_A  ],
    {-      ZERO -}  [ NONE_A,  ZERO,     ZERO,    ZERO,     ERR_A,  ZERO,    ZERO,      ZERO,     ZERO,    ANY_A  ],
    {-       POS -}  [ NONE_A,  NEG,      ZERO,    POS,      ERR_A,  NON_POS, NON_ZERO,  NON_NEG,  Z,       ANY_A  ],
    {-     ERR_A -}  [ NONE_A,  ERR_A,    ERR_A,   ERR_A,    ERR_A,  ERR_A,   ERR_A,     ERR_A,    ERR_A,   ERR_A  ],
    {-   NON_POS -}  [ NONE_A,  NON_NEG,  ZERO,    NON_POS,  ERR_A,  NON_NEG, Z,         NON_POS,  Z,       ANY_A  ],
    {-  NON_ZERO -}  [ NONE_A,  NON_ZERO, ZERO,    NON_ZERO, ERR_A,  Z,       NON_ZERO,  Z,        Z,       ANY_A  ],
    {-   NON_NEG -}  [ NONE_A,  NON_POS,  ZERO,    NON_NEG,  ERR_A,  NON_POS, Z,         NON_NEG,  Z,       ANY_A  ],
    {-         Z -}  [ NONE_A,  Z,        ZERO,    Z,        ERR_A,  Z,       Z,         Z,        Z,       ANY_A  ],
    {-     ANY_A -}  [ NONE_A,  ANY_A,    ANY_A,   ANY_A,    ERR_A,  ANY_A,   ANY_A,     ANY_A,    ANY_A,   ANY_A  ]
    ]

divMap :: Matrix SignExc
divMap = Mat.fromLists [
    {-                 NONE_A   NEG       ZERO     POS       ERR_A   NON_POS  NON_ZERO   NON_NEG   Z        ANY_A -}
    {-    NONE_A -}  [ NONE_A,  NONE_A,   NONE_A,  NONE_A,   NONE_A, NONE_A,  NONE_A,    NONE_A,   NONE_A,  NONE_A ],
    {-       NEG -}  [ NONE_A,  NON_NEG,  ERR_A,   NON_POS,  ERR_A,  ANY_A,   Z,         ANY_A,    ANY_A,   ANY_A  ],
    {-      ZERO -}  [ NONE_A,  ZERO,     ERR_A,   ZERO,     ERR_A,  ANY_A,   ZERO,      ANY_A,    ANY_A,   ANY_A  ],
    {-       POS -}  [ NONE_A,  NON_POS,  ERR_A,   NON_NEG,  ERR_A,  ANY_A,   Z,         ANY_A,    ANY_A,   ANY_A  ],
    {-     ERR_A -}  [ NONE_A,  ERR_A,    ERR_A,   ERR_A,    ERR_A,  ERR_A,   ERR_A,     ERR_A,    ERR_A,   ERR_A  ],
    {-   NON_POS -}  [ NONE_A,  NON_NEG,  ERR_A,   NON_POS,  ERR_A,  ANY_A,   Z,         ANY_A,    ANY_A,   ANY_A  ],
    {-  NON_ZERO -}  [ NONE_A,  Z,        ERR_A,   Z,        ERR_A,  ANY_A,   Z,         ANY_A,    ANY_A,   ANY_A  ],
    {-   NON_NEG -}  [ NONE_A,  NON_POS,  ERR_A,   NON_NEG,  ERR_A,  ANY_A,   Z,         ANY_A,    ANY_A,   ANY_A  ],
    {-         Z -}  [ NONE_A,  Z,        ERR_A,   Z,        ERR_A,  ANY_A,   Z,         ANY_A,    ANY_A,   ANY_A  ],
    {-     ANY_A -}  [ NONE_A,  ANY_A,    ERR_A,   ANY_A,    ERR_A,  ANY_A,   ANY_A,     ANY_A,    ANY_A,   ANY_A  ]
    ]

addMap :: Matrix SignExc
addMap = Mat.fromLists [
    {-                 NONE_A   NEG       ZERO      POS       ERR_A   NON_POS  NON_ZERO   NON_NEG   Z         ANY_A -}
    {-    NONE_A -}  [ NONE_A,  NONE_A,   NONE_A,   NONE_A,   NONE_A, NONE_A,  NONE_A,    NONE_A,   NONE_A,    NONE_A ],
    {-       NEG -}  [ NONE_A,  NEG,      NEG,      Z,        ERR_A,  NEG,     Z,         Z,        Z,         ANY_A  ],
    {-      ZERO -}  [ NONE_A,  NEG,      ZERO,     POS,      ERR_A,  NON_POS, NON_ZERO,  NON_NEG,  Z,         ANY_A  ],
    {-       POS -}  [ NONE_A,  Z,        POS,      POS,      ERR_A,  Z,       Z,         POS,      Z,         ANY_A  ],
    {-     ERR_A -}  [ NONE_A,  ERR_A,    ERR_A,    ERR_A,    ERR_A,  ERR_A,   ERR_A,     ERR_A,    ERR_A,     ERR_A  ],
    {-   NON_POS -}  [ NONE_A,  NEG,      NON_POS,  Z,        ERR_A,  NON_POS, Z,         Z,        Z,         ANY_A  ],
    {-  NON_ZERO -}  [ NONE_A,  Z,        NON_ZERO, Z,        ERR_A,  Z,       Z,         Z,        Z,         ANY_A  ],
    {-   NON_NEG -}  [ NONE_A,  Z,        NON_NEG,  POS,      ERR_A,  Z,       Z,         NON_NEG,  Z,         ANY_A  ],
    {-         Z -}  [ NONE_A,  Z,        Z,        Z,        ERR_A,  Z,       Z,         Z,        Z,         ANY_A  ],
    {-     ANY_A -}  [ NONE_A,  ANY_A,    ANY_A,    ANY_A,    ERR_A,  ANY_A,   ANY_A,     ANY_A,    ANY_A,     ANY_A  ]
    ]

subMap :: Matrix SignExc
subMap = Mat.fromLists [
    {-                 NONE_A   NEG       ZERO      POS      ERR_A   NON_POS  NON_ZERO   NON_NEG   Z          ANY_A -}
    {-    NONE_A -}  [ NONE_A,  NONE_A,   NONE_A,   NONE_A,  NONE_A, NONE_A,  NONE_A,    NONE_A,   NONE_A,    NONE_A ],
    {-       NEG -}  [ NONE_A,  Z,        NEG,      NEG,     ERR_A,  Z,       Z,         Z,        Z,         ANY_A  ],
    {-      ZERO -}  [ NONE_A,  POS,      ZERO,     NEG,     ERR_A,  NON_NEG, NON_ZERO,  NON_POS,  Z,         ANY_A  ],
    {-       POS -}  [ NONE_A,  POS,      POS,      Z,       ERR_A,  POS,     Z,         Z,        Z,         ANY_A  ],
    {-     ERR_A -}  [ NONE_A,  ERR_A,    ERR_A,    ERR_A,   ERR_A,  ERR_A,   ERR_A,     ERR_A,    ERR_A,     ERR_A  ],
    {-   NON_POS -}  [ NONE_A,  POS,      NON_POS,  NEG,     ERR_A,  Z,       Z,         NON_POS,  Z,         ANY_A  ],
    {-  NON_ZERO -}  [ NONE_A,  Z,        NON_ZERO, Z,       ERR_A,  Z,       Z,         Z,        Z,         ANY_A  ],
    {-   NON_NEG -}  [ NONE_A,  POS,      NON_NEG,  Z,       ERR_A,  NON_NEG, Z,         Z,        Z,         ANY_A  ],
    {-         Z -}  [ NONE_A,  Z,        Z,        Z,       ERR_A,  Z,       Z,         Z,        Z,         ANY_A  ],
    {-     ANY_A -}  [ NONE_A,  ANY_A,    ANY_A,    ANY_A,   ERR_A,  ANY_A,   ANY_A,     ANY_A,    ANY_A,     ANY_A  ]
    ]

eqMap :: Matrix TTExc
eqMap = Mat.fromLists [
    {-                 NONE_A  NEG     ZERO    POS     ERR_A   NON_POS NON_ZERO NON_NEG Z       ANY_A -}
    {-    NONE_A -}  [ NONE_B, NONE_B, NONE_B, NONE_B, NONE_B, NONE_B, NONE_B,  NONE_B, NONE_B, NONE_B ],
    {-       NEG -}  [ NONE_B, T,      FF,     FF,     ERR_B,  T,      T,       FF,     T,      ANY_B  ],
    {-      ZERO -}  [ NONE_B, FF,     TT,     FF,     ERR_B,  T,      FF,      T,      T,      ANY_B  ],
    {-       POS -}  [ NONE_B, FF,     FF,     T,      ERR_B,  FF,     T,       T,      T,      ANY_B  ],
    {-     ERR_A -}  [ NONE_B, ERR_B,  ERR_B,  ERR_B,  ERR_B,  ERR_B,  ERR_B,   ERR_B,  ERR_B,  ERR_B  ],
    {-   NON_POS -}  [ NONE_B, T,      T,      FF,     ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-  NON_ZERO -}  [ NONE_B, T,      FF,     T,      ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-   NON_NEG -}  [ NONE_B, FF,     T,      T,      ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-         Z -}  [ NONE_B, T,      T,      T,      ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-     ANY_A -}  [ NONE_B, ANY_B,  ANY_B,  ANY_B,  ERR_B,  ANY_B,  ANY_B,   ANY_B,  ANY_B,  ANY_B  ]
    ]

leqMap :: Matrix TTExc
leqMap = Mat.fromLists [
    {-                 NONE_A  NEG     ZERO    POS     ERR_A   NON_POS NON_ZERO NON_NEG Z       ANY_A -}
    {-    NONE_A -}  [ NONE_B, NONE_B, NONE_B, NONE_B, NONE_B, NONE_B, NONE_B,  NONE_B, NONE_B, NONE_B ],
    {-       NEG -}  [ NONE_B, T,      TT,     TT,     ERR_B,  T,      T,       TT,     T,      ANY_B  ],
    {-      ZERO -}  [ NONE_B, FF,     TT,     TT,     ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-       POS -}  [ NONE_B, FF,     FF,     T,      ERR_B,  FF,     T,       T,      T,      ANY_B  ],
    {-     ERR_A -}  [ NONE_B, ERR_B,  ERR_B,  ERR_B,  ERR_B,  ERR_B,  ERR_B,   ERR_B,  ERR_B,  ERR_B  ],
    {-   NON_POS -}  [ NONE_B, T,      T,      TT,     ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-  NON_ZERO -}  [ NONE_B, T,      T,      T,      ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-   NON_NEG -}  [ NONE_B, FF,     T,      T,      ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-         Z -}  [ NONE_B, T,      T,      T,      ERR_B,  T,      T,       T,      T,      ANY_B  ],
    {-     ANY_A -}  [ NONE_B, ANY_B,  ANY_B,  ANY_B,  ERR_B,  ANY_B,  ANY_B,   ANY_B,  ANY_B,  ANY_B  ]
    ]