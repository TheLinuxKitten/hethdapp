{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.EthABI.AST
  ( SolcAST(..)
  , Children(..)
  , AstNode(..)
  , ExportedSymbol(..)
  , parseSolcAst
  ) where

import Data.Aeson.JsonUtils
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Instances.TH.Lift
import qualified Language.Haskell.TH.Syntax as THS

-- Los campos Text pueden pasarse a un tipo de dato
-- Los campos Value estudiarlos para ver los valores
-- Los Maybe[] pueden ser [] simplemente

type AstId = Int

data ExportedSymbol = ExportedSymbol
  { expsymName :: Text
  , expsymIds :: [AstId]
  } deriving (THS.Lift, Show)

parseExportedSymbol :: Text -> Value -> Parser ExportedSymbol
parseExportedSymbol name v = ExportedSymbol name <$> parseJSON v

data AstNode =
    ArrayTypeName
      { atnLength :: Maybe Value
      , atnType :: Text
      }
  | Assignment
      { asArgumentTypes :: Value
      , asIsConstant :: Bool
      , asIsLValue :: Bool
      , asIsPure :: Bool
      , asLValueRequested :: Bool
      , asOperator :: Text
      , asType :: Text
      }
  | BinaryOperation
      { boArgumentTypes :: Value
      , boCommonType :: Value
      , boIsConstant :: Bool
      , boIsLValue :: Bool
      , boIsPure :: Bool
      , boLValueRequested :: Bool
      , boOperator :: Text
      , boType :: Text
      }
  | Block
  | ContractDefinition
      { cdBaseContracts :: Maybe [Value]
      , cdContractDependencies :: [Value]
      , cdContractKind :: Text
      , cdDocumentation :: Value
      , cdFullyImplemented :: Bool
      , cdLinearizedBaseContracts :: [AstId]
      , cdName :: Text
      , cdScope :: AstId
      }
  | ElementaryTypeName
      { etnName :: Text
      , etnType :: Text
      }
  | ElementaryTypeNameExpression
      { etneArgumentTypes :: Value
      , etneIsConstant :: Bool
      , etneIsLValue :: Bool
      , etneIsPure :: Bool
      , etneLValueRequested :: Bool
      , etneType :: Text
      , etneValue :: Text
      }
  | EventDefinition
      { edAnonymous :: Bool
      , edName :: Text
      }
  | ExpressionStatement
  | ForStatement
      { fsAssignments :: Maybe [AstId]
      , fsInitializationExpression :: Maybe Value
      }
  | FunctionCall
      { fcArgumentTypes :: Value
      , fcIsConstant :: Bool
      , fcIsLValue :: Bool
      , fcIsPure :: Bool
      , fcIsStructConstructorCall :: Bool
      , fcLValueRequested :: Bool
      , fcNames :: [Value]
      , fcType :: Text
      , fcType_Conversion :: Bool
      }
  | FunctionDefinition
      { fdConstant :: Bool
      , fdImplemented :: Bool
      , fdIsConstructor :: Bool
      , fdModifiers :: Maybe [Value]
      , fdName :: Text
      , fdPayable :: Bool
      , fdScope :: AstId
      , fdStateMutability :: Text
      , fdSuperFunction :: Value
      , fdVisibility :: Text
      }
  | Identifier
      { idArgumentTypes :: Value
      , idOverloadedDeclarations :: [Value]
      , idReferencedDeclaration :: Maybe AstId
      , idType :: Text
      , idValue :: Text
      }
  | IfStatement
      { isFalseBody :: Value
      }
  | ImportDirective
      { imSourceUnit :: AstId
      , imAbsolutePath :: FilePath
      , imFile :: FilePath
      , imScope :: AstId
      , imSymbolAliases :: [Value]
      , imUnitAlias :: Text
      }
  | IndexAccess
      { iaArgumentTypes :: Value
      , iaIsConstant :: Bool
      , iaIsLValue :: Bool
      , iaIsPure :: Bool
      , iaLValueRequested :: Bool
      , iaType :: Text
      }
  | InheritanceSpecifier
      { isArguments :: Maybe [Value]
      }
  | InlineAssembly
      { inaExternalReferences :: [Value]
      , inaOperations :: Text
      }
  | Literal
      { liArgumentTypes :: Value
      , liHexvalue :: Text
      , liIsConstant :: Bool
      , liIsLValue :: Bool
      , liIsPure :: Bool
      , liLValueRequested :: Bool
      , liSubdenomination :: Value
      , liToken :: Text
      , liType :: Maybe Text
      , liValue :: Text
      }
  | Mapping
      {  mapType :: Text
      }
  | MemberAccess
      { maArgumentTypes :: Value
      , maIsConstant :: Bool
      , maIsLValue :: Bool
      , maIsPure :: Bool
      , maLValueRequested :: Bool
      , maMember_Name :: Text
      , maReferencedDeclaration :: Maybe AstId
      , maType :: Text
      }
  | ModifierDefinition
      { mdName :: Text
      , mdVisibility :: Text
      }
  | ModifierInvocation
      { miArguments :: Maybe [Value]
      }
  | NewExpression
      { neArgumentTypes :: Value
      , neIsConstant :: Bool
      , neIsLValue :: Bool
      , neIsPure :: Bool
      , neLValueRequested :: Bool
      , neType :: Text
      }
  | ParameterList
      { plParameters :: Maybe [Value]
      }
  | PlaceholderStatement
  | PragmaDirective
      { pdLiterals :: [Text]
      }
  | Return
      { retExpression :: Maybe Value
      , retFunctionReturnParameters :: AstId
      }
  | SourceUnit
      { suAbsolutePath :: FilePath
      , suExportedSymbols :: [ExportedSymbol]
      }
  | StructDefinition
      { sdCanonicalName :: Text
      , sdName :: Text
      , sdScope :: AstId
      , sdVisibility :: Text
      }
  | TupleExpression
      { teArgumentTypes :: Value
      , teIsConstant :: Bool
      , teIsInlineArray :: Bool
      , teIsLValue :: Bool
      , teIsPure :: Bool
      , teLValueRequested :: Bool
      , teType :: Text
      }
  | UnaryOperation
      { uoArgumentTypes :: Value
      , uoIsConstant :: Bool
      , uoIsLValue :: Bool
      , uoIsPure :: Bool
      , uoLValueRequested :: Bool
      , uoOperator :: Text
      , uoPrefix :: Bool
      , uoType :: Text
      }
  | UserDefinedTypeName
      { utnContractScope :: Value
      , utnName :: Text
      , utnReferencedDeclaration :: Maybe AstId
      , utnType :: Text
      }
  | VariableDeclaration
      { vdConstant :: Bool
      , vdIndexed :: Maybe Bool
      , vdName :: Text
      , vdScope :: AstId
      , vdStateVariable :: Bool
      , vdStorageLocation :: Text
      , vdType :: Text
      , vdValue :: Maybe Value
      , vdVisibility :: Text
      }
  | VariableDeclarationStatement
      { vdsAssignments :: [Value]
      , vdsInitialValue :: Maybe Value
      }
  | WhileStatement
  | Undefined Text Value
  deriving (THS.Lift, Show)

parseKey :: (FromJSON a) => Text -> Object -> Parser a
parseKey k o = o .: k

parseMaybeKey :: (FromJSON a) => Text -> Object -> Parser (Maybe a)
parseMaybeKey k o = o .:? k

parseKeyMaybeNull :: (FromJSON a) => Text -> Object -> Parser (Maybe a)
parseKeyMaybeNull k o = do
  v <- o .: k
  case v of
    Null -> return Nothing
    _ -> Just <$> parseJSON v

parseMaybeKeyMaybeNull :: (FromJSON a) => Text -> Object -> Parser (Maybe a)
parseMaybeKeyMaybeNull k o = do
  mv <- o .:? k
  case mv of
    Nothing -> return Nothing
    Just v -> case v of
      Null -> return Nothing
      _ -> Just <$> parseJSON v

pAbsolutePath = parseKey "absolutePath"
pAnonymous = parseKey "anonymous"
pArguments = parseKey "arguments"
pArgumentTypes = parseKey "argumentTypes"
pAssignments = parseKey "assignments"
pmAssignments = parseMaybeKey "assignments"
pmInitializationExpression = parseMaybeKey "initializationExpression"
pmBaseContracts = parseMaybeKey "baseContracts"
pCanonicalName = parseKey "canonicalName"
pCommonType = parseKey "commonType"
pConstant = parseKey "constant"
pContractDependencies = parseKey "contractDependencies"
pContractKind = parseKey "contractKind"
pContractScope = parseKey "contractScope"
pDocumentation = parseKey "documentation"
pmExpression = parseMaybeKeyMaybeNull "expression"
pExternalReferences = parseKey "externalReferences"
pOperations = parseKey "operations"
pFalseBody = parseKey "falseBody"
pFile = parseKey "file"
pFullyImplemented = parseKey "fullyImplemented"
pFunctionReturnParameters = parseKey "functionReturnParameters"
pHexvalue = parseKey "hexvalue"
pImplemented = parseKey "implemented"
pmIndexed = parseMaybeKey "indexed"
pmInitialValue = parseMaybeKey "initialValue"
pIsConstant = parseKey "isConstant"
pIsConstructor = parseKey "isConstructor"
pIsInlineArray = parseKey "isInlineArray"
pIsLValue = parseKey "isLValue"
pIsPure = parseKey "isPure"
pIsStructConstructorCall = parseKey "isStructConstructorCall"
pmLength = parseMaybeKeyMaybeNull "length"
pLinearizedBaseContracts = parseKey "linearizedBaseContracts"
pLiterals = parseKey "literals"
pLValueRequested = parseKey "lValueRequested"
pMember_Name = parseKey "member_name"
pmModifiers = parseMaybeKey "modifiers"
pName = parseKey "name"
pNames = parseKey "names"
pOperator = parseKey "operator"
pOverloadedDeclarations = parseKey "overloadedDeclarations"
pParameters = parseKey "parameters"
pPayable = parseKey "payable"
pPrefix = parseKey "prefix"
pReferencedDeclaration  = parseKeyMaybeNull "referencedDeclaration"
pScope = parseKey "scope"
pSourceUnit = parseKey "SourceUnit"
pStateMutability = parseKey "stateMutability"
pStateVariable = parseKey "stateVariable"
pStorageLocation = parseKey "storageLocation"
pSubdenomination = parseKey "subdenomination"
pSuperFunction = parseKey "superFunction"
pSymbolAliases = parseKey "symbolAliases"
pToken = parseKey "token"
pType = parseKey "type"
pmType = parseKeyMaybeNull "type"
pType_Conversion = parseKey "type_conversion"
pUnitAlias = parseKey "unitAlias"
pValue = parseKey "value"
pmValue = parseMaybeKey "value"
-- pValueRequested = parseKey "valueRequested"
pVisibility = parseKey "visibility"

parseAstNode :: Text -> Maybe Value -> Parser AstNode
parseAstNode "ArrayTypeName" (Just (Object o)) = ArrayTypeName
                                             <$> pmLength o
                                             <*> pType o
parseAstNode "Assignment" (Just (Object o)) = Assignment
                                          <$> pArgumentTypes o
                                          <*> pIsConstant o
                                          <*> pIsLValue o
                                          <*> pIsPure o
                                          <*> pLValueRequested o
                                          <*> pOperator o
                                          <*> pType o
parseAstNode "BinaryOperation" (Just (Object o)) = BinaryOperation
                                               <$> pArgumentTypes o
                                               <*> pCommonType o
                                               <*> pIsConstant o
                                               <*> pIsLValue o
                                               <*> pIsPure o
                                               <*> pLValueRequested o
                                               <*> pOperator o
                                               <*> pType o
parseAstNode "Block" Nothing = return Block
parseAstNode "ContractDefinition" (Just (Object o)) = ContractDefinition
                                                  <$> pmBaseContracts o
                                                  <*> pContractDependencies o
                                                  <*> pContractKind o
                                                  <*> pDocumentation o
                                                  <*> pFullyImplemented o
                                                  <*> pLinearizedBaseContracts o
                                                  <*> pName o
                                                  <*> pScope o
parseAstNode "ElementaryTypeName" (Just (Object o)) = ElementaryTypeName
                                                  <$> pName o
                                                  <*> pType o
parseAstNode "ElementaryTypeNameExpression" (Just (Object o)) =
  ElementaryTypeNameExpression <$> pArgumentTypes o
                               <*> pIsConstant o
                               <*> pIsLValue o
                               <*> pIsPure o
                               <*> pLValueRequested o
                               <*> pType o
                               <*> pValue o
parseAstNode "EventDefinition" (Just (Object o)) = EventDefinition
                                               <$> pAnonymous o
                                               <*> pName o
parseAstNode "ExpressionStatement" Nothing = return ExpressionStatement
parseAstNode "ForStatement" Nothing = return $ ForStatement Nothing Nothing
parseAstNode "ForStatement" (Just (Object o)) = ForStatement
                                            <$> pmAssignments o
                                            <*> pmInitializationExpression o
parseAstNode "FunctionCall" (Just (Object o)) = FunctionCall
                                            <$> pArgumentTypes o
                                            <*> pIsConstant o
                                            <*> pIsLValue o
                                            <*> pIsPure o
                                            <*> pIsStructConstructorCall o
                                            <*> pLValueRequested o
                                            <*> pNames o
                                            <*> pType o
                                            <*> pType_Conversion o
parseAstNode "FunctionDefinition" (Just (Object o)) = FunctionDefinition
                                                  <$> pConstant o
                                                  <*> pImplemented o
                                                  <*> pIsConstructor o
                                                  <*> pmModifiers o
                                                  <*> pName o
                                                  <*> pPayable o
                                                  <*> pScope o
                                                  <*> pStateMutability o
                                                  <*> pSuperFunction o
                                                  <*> pVisibility o
parseAstNode "Identifier" (Just (Object o)) = Identifier
                                          <$> pArgumentTypes o
                                          <*> pOverloadedDeclarations o
                                          <*> pReferencedDeclaration o
                                          <*> pType o
                                          <*> pValue o
parseAstNode "IfStatement" Nothing = return $ IfStatement Null
parseAstNode "IfStatement" (Just (Object o)) = IfStatement <$> pFalseBody o
parseAstNode "ImportDirective" (Just (Object o)) = ImportDirective
                                               <$> pSourceUnit o
                                               <*> pAbsolutePath o
                                               <*> pFile o
                                               <*> pScope o
                                               <*> pSymbolAliases o
                                               <*> pUnitAlias o
parseAstNode "IndexAccess" (Just (Object o)) = IndexAccess
                                           <$> pArgumentTypes o
                                           <*> pIsConstant o
                                           <*> pIsLValue o
                                           <*> pIsPure o
                                           <*> pLValueRequested o
                                           <*> pType o
parseAstNode "InheritanceSpecifier" (Just (Object o)) = InheritanceSpecifier
                                                    <$> pArguments o
parseAstNode "InlineAssembly" (Just (Object o)) = InlineAssembly
                                              <$> pExternalReferences o
                                              <*> pOperations o
parseAstNode "Literal" (Just (Object o)) = Literal
                                       <$> pArgumentTypes o
                                       <*> pHexvalue o
                                       <*> pIsConstant o
                                       <*> pIsLValue o
                                       <*> pIsPure o
                                       <*> pLValueRequested o
                                       <*> pSubdenomination o
                                       <*> pToken o
                                       <*> pmType o
                                       <*> pValue o
parseAstNode "Mapping" (Just (Object o)) = Mapping <$> pType o
parseAstNode "MemberAccess" (Just (Object o)) = MemberAccess
                                            <$> pArgumentTypes o
                                            <*> pIsConstant o
                                            <*> pIsLValue o
                                            <*> pIsPure o
                                            <*> pLValueRequested o
                                            <*> pMember_Name o
                                            <*> pReferencedDeclaration o
                                            <*> pType o
parseAstNode "ModifierDefinition" (Just (Object o)) = ModifierDefinition
                                                  <$> pName o
                                                  <*> pVisibility o
parseAstNode "ModifierInvocation" Nothing = return $ ModifierInvocation Nothing
parseAstNode "ModifierInvocation" (Just (Object o)) = ModifierInvocation
                                                  <$> pArguments o
parseAstNode "NewExpression" (Just (Object o)) = NewExpression
                                             <$> pArgumentTypes o
                                             <*> pIsConstant o
                                             <*> pIsLValue o
                                             <*> pIsPure o
                                             <*> pLValueRequested o
                                             <*> pType o
parseAstNode "ParameterList" Nothing = return $ ParameterList Nothing
parseAstNode "ParameterList" (Just (Object o)) = ParameterList <$> pParameters o
parseAstNode "PlaceholderStatement" Nothing = return PlaceholderStatement
parseAstNode "PragmaDirective" (Just (Object o)) = PragmaDirective
                                               <$> pLiterals o
parseAstNode "Return" (Just (Object o)) = Return
                                      <$> pmExpression o
                                      <*> pFunctionReturnParameters o
parseAstNode "SourceUnit" (Just (Object o)) = SourceUnit
                                          <$> pAbsolutePath o
                                          <*> parseListOfKeyObject o "exportedSymbols" parseExportedSymbol
parseAstNode "StructDefinition" (Just (Object o)) = StructDefinition
                                                <$> pCanonicalName o
                                                <*> pName o
                                                <*> pScope o
                                                <*> pVisibility o
parseAstNode "TupleExpression" (Just (Object o)) = TupleExpression
                                               <$> pArgumentTypes o
                                               <*> pIsConstant o
                                               <*> pIsInlineArray o
                                               <*> pIsLValue o
                                               <*> pIsPure o
                                               <*> pLValueRequested o
                                               <*> pType o
parseAstNode "UnaryOperation" (Just (Object o)) = UnaryOperation
                                             <$> pArgumentTypes o
                                             <*> pIsConstant o
                                             <*> pIsLValue o
                                             <*> pIsPure o
                                             <*> pLValueRequested o
                                             <*> pOperator o
                                             <*> pPrefix o
                                             <*> pType o
parseAstNode "UserDefinedTypeName" (Just (Object o)) = UserDefinedTypeName
                                                   <$> pContractScope o
                                                   <*> pName o
                                                   <*> pReferencedDeclaration o
                                                   <*> pType o
parseAstNode "VariableDeclaration" (Just (Object o)) = VariableDeclaration
                                                   <$> pConstant o
                                                   <*> pmIndexed o
                                                   <*> pName o
                                                   <*> pScope o
                                                   <*> pStateVariable o
                                                   <*> pStorageLocation o
                                                   <*> pType o
                                                   <*> pmValue o
                                                   <*> pVisibility o
parseAstNode "VariableDeclarationStatement" (Just (Object o)) = 
  VariableDeclarationStatement <$> pAssignments o <*> pmInitialValue o
parseAstNode "WhileStatement" Nothing = return WhileStatement
parseAstNode name Nothing = return $ Undefined name Null
parseAstNode name (Just v) = return $ Undefined name v
--parseAstNode name mv = fail $ "parseAstNode: " ++ T.unpack name ++ " " ++ show mv

data Children = Children
  { chNode :: AstNode
  , chChildrens :: Maybe [Children]
  , chId :: AstId
  , chSrc :: Text
  } deriving (THS.Lift, Show)

instance FromJSON Children where
  parseJSON (Object o) = do
    i <- o .: "id"
    src <- o .: "src"
    name <- o .: "name"
    node <- (o .:? "attributes") >>= parseAstNode name
    childs <- o .:? "children"
    return $ Children node childs i src

data SolcAST = SolcAST
  { astSource :: FilePath
  , astRoot :: Children
  } deriving (THS.Lift, Show)

parseAST :: Text -> Value -> Parser SolcAST
parseAST n (Object o) = SolcAST (T.unpack n) <$> o .: "AST"

parseSolcAst :: Value -> Parser [SolcAST]
parseSolcAst (Object o) = parseListOfKeyObject o "sources" parseAST

