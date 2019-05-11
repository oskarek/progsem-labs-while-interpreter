module AbstractTypes.TTExc
    ( TTExc(..)
    , ttVal
    )
where

import           AM.Lattice
import qualified Data.Matrix                   as Mat
import           Data.Matrix                    ( (!)
                                                , Matrix
                                                )

data TTExc =
    NONE_B | TT | FF | ERR_B | T | ANY_B
    deriving (Eq, Ord, Enum, Show)

instance Lattice TTExc where
    join = ANY_B
    meet = NONE_B
    lub t1 t2 = lubMap ! (ttVal t1, ttVal t2)
    glb t1 t2 = glbMap ! (ttVal t1, ttVal t2)

ttVal :: TTExc -> Int
ttVal = (+1) . fromEnum

lubMap :: Matrix TTExc
lubMap = Mat.fromLists [
    {-             NONE_B  TT      FF      ERR_B   T    ANY_B -}
    {- NONE_B -} [ NONE_B, TT,     FF,     ERR_B,  T,      ANY_B  ],
    {- TT     -} [ TT,     TT,     T,      ANY_B,  T,      ANY_B  ],
    {- FF     -} [ FF,     T,      FF,     ANY_B,  T,      ANY_B  ],
    {- ERR_B  -} [ ERR_B,  ANY_B,  ANY_B,  ERR_B,  ANY_B,  ANY_B  ],
    {- T      -} [ T,      T,      T,      ANY_B,  T,      ANY_B  ],
    {- ANY_B  -} [ ANY_B,  ANY_B,  ANY_B,  ANY_B,  ANY_B,  ANY_B  ]
    ]

glbMap :: Matrix TTExc
glbMap = Mat.fromLists [
    {-             NONE_B  TT      FF      ERR_B   T       ANY_B -}
    {- NONE_B -} [ NONE_B, NONE_B, NONE_B, NONE_B, NONE_B, NONE_B ],
    {- TT     -} [ NONE_B, TT,     NONE_B, NONE_B, TT,     TT     ],
    {- FF     -} [ NONE_B, NONE_B, FF,     NONE_B, FF,     FF     ],
    {- ERR_B  -} [ NONE_B, NONE_B, NONE_B, ERR_B,  NONE_B, ERR_B  ],
    {- T      -} [ NONE_B, TT,     FF,     NONE_B, T,      T      ],
    {- ANY_B  -} [ NONE_B, TT,     FF,     ERR_B,  T,      ANY_B  ]
    ]