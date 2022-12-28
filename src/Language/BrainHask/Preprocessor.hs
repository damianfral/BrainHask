module Language.BrainHask.Preprocessor (preprocess) where

import Control.Applicative hiding (many)
import Data.Functor (($>))
import Data.Transformer
import Language.BrainHask.Types

type Preprocessor a = Transformer BFOp (ILOp Int a)

moveTransformer,
  addTransformer,
  loopTransformer,
  readTransformer,
  writeTransformer ::
    Preprocessor Int
moveTransformer =
  choice
    [ ILMove <$> howMany1 (matchConstructor BFMoveRight),
      ILMove . negate <$> howMany1 (matchConstructor BFMoveLeft)
    ]
addTransformer =
  choice
    [ ILAdd <$> howMany1 (matchConstructor BFIncrease),
      ILAdd . negate <$> howMany1 (matchConstructor BFDecrease)
    ]
loopTransformer = do
  (BFLoop ops) <- matchConstructor (BFLoop [])
  return $ ILLoop (preprocess ops)
readTransformer = matchConstructor BFRead $> ILRead
writeTransformer = ILWrite <$> howMany1 (matchConstructor BFWrite)

preprocessor :: Preprocessor Int
preprocessor =
  choice
    [ moveTransformer,
      addTransformer,
      loopTransformer,
      readTransformer,
      writeTransformer
    ]

preprocess :: BFProgram -> ILProgram Int
preprocess = transformOrOmit preprocessor
