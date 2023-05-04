module HelVM.HelTC.Calculators.LC.DefinitionExpander (
  expandDefinitions,
) where

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelIO.Control.Safe

import           Data.Map                          as Map

expandDefinitions :: MonadSafe m => InstructionList -> m LambdaList
expandDefinitions il = flip expandDefinitionsByMap il =<< buildDefinitionMap il

removeDefinitions :: InstructionList -> LambdaList
removeDefinitions l = (maybeToList . removeDefinition) =<< l

removeDefinition :: Instruction -> Maybe Lambda
removeDefinition (Def _ _) = Nothing
removeDefinition (Eval  l) = Just l

buildDefinitionMap :: MonadSafe m => InstructionList -> m Definitions
buildDefinitionMap il = nextDefinitionMap il Map.empty

nextDefinitionMap :: MonadSafe m => InstructionList -> Definitions -> m Definitions
nextDefinitionMap              [] m = pure m
nextDefinitionMap d@(Def n f : l) m = nextDefinitionMap l =<< appendError (show d) (ff <$> expandDefinitionByMap m f) where ff x = insert n x m
nextDefinitionMap   (_       : l) m = nextDefinitionMap l m

expandDefinitionsByMap :: MonadSafe m => Definitions -> InstructionList -> m LambdaList
expandDefinitionsByMap m il = appendError (show il) $ sequence (expandDefinitionByMap m <$> removeDefinitions il)

expandDefinitionByMap :: MonadSafe m => Definitions -> Lambda -> m Lambda
expandDefinitionByMap m (App f g) = liftA2 App (expandDefinitionByMap m f) (expandDefinitionByMap m g)
expandDefinitionByMap m (Abs n f) = Abs n <$> expandDefinitionByMap m f
expandDefinitionByMap m (Var n)   = liftMaybeOrErrorTupleList [("n", show n) , ("m" , show m)] $ lookup n m
expandDefinitionByMap _         l = pure l

-- | Types
type Definitions = Map Text Lambda
