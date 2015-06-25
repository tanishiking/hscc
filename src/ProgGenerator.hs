module ProgGenerator where

import AST
import Parser
import Data.List

convProg :: Program -> String
convProg []     = ""
convProg (p:ps) = concat [convExDecl p, convProg ps]

convExDecl :: ExternalDeclaration -> String
convExDecl (Decl     _ declList) = convDeclList declList
convExDecl (FuncProt _ funcProt) = convFuncProt funcProt ++ ";\n"
convExDecl (FuncDef  _ funcDef)  = convFuncDef  funcDef


convDeclList :: DeclaratorList -> String
convDeclList = concat . foldr f []
  where f (ty, directDecl) acc = (concat [show ty, show directDecl, ";\n"]):acc

convFuncProt :: FunctionPrototype -> String
convFuncProt (FunctionPrototype _ ty fname params) =
  concat [show ty, show fname, "(", convParams params, ")"]

convFuncDef :: FunctionDefinition -> String
convFuncDef (FunctionDefinition _ ty fname params stmt) =
  concat [show ty, show fname, "(", convParams params, ")", "{\n", convStmt stmt, "}\n"]


{----------------------------------------------------------------}

convParams :: [(Type, Identifier)] -> String
convParams =
  concat . intersperse ", " . foldr (\(t, i) acc -> (show t ++ show i):acc) []

convCompound :: [DeclaratorList] -> [Stmt] -> String
convCompound decls stmts =
  concat [convDeclarationList decls, f stmts]
    where f stmts = foldr (\s acc -> convStmt s ++ "\n" ++ acc) "" stmts

convDeclarationList :: [DeclaratorList] -> String
convDeclarationList decls = concat $ map convDeclList decls

convStmts :: [Stmt] -> String
convStmts [s] = convStmt s
convStmts stmts = concat["{\n",
                  foldr (\s acc -> convStmt s ++ "\n" ++ acc) "" stmts,
                  "}\n"]

convStmt :: Stmt -> String
convStmt (EmptyStmt _)          = ";" 
convStmt (ExprStmt  _ e)        = convExpr e ++ ";"
convStmt (CompoundStmt _ ds ss) = convCompound ds ss
convStmt (IfStmt _ e s1 s2)     = concat ["if(", convExpr e, ") ",
                                          convStmt s1, "else ",
                                          convStmt s2]
convStmt (WhileStmt _ e s)      = concat ["while(", convExpr e,
                                          ") ", convStmt s]
--convStmt (ForStmt _ e1 e2 e3 s) = concat ["for(", 
--                                          convExpr e1, "; ",
--                                          convExpr e2, "; ",
--                                          convExpr e3, ")",
--                                          convStmt s] 
convStmt (ReturnStmt _ e)       = concat ["return ",
                                          convExpr e,
                                          ";"]


convExpr :: Expr -> String
convExpr (AssignExpr _ e1 e2)  = concat [convExpr e1, " = ", convExpr e2]
convExpr (Or _ e1 e2)          = concat [convExpr e1, " || ", convExpr e2]
convExpr (And _ e1 e2)         = concat [convExpr e1, " && ", convExpr e2]
convExpr (Equal _ e1 e2)       = concat [convExpr e1, " == ", convExpr e2]
convExpr (NotEqual _ e1 e2)    = concat [convExpr e1, " != ", convExpr e2]
convExpr (Lt _ e1 e2)          = concat [convExpr e1, " < ", convExpr e2]
convExpr (Gt _ e1 e2)          = concat [convExpr e1, " > ", convExpr e2]
convExpr (Lte _ e1 e2)         = concat [convExpr e1, " <= ", convExpr e2]
convExpr (Gte _ e1 e2)         = concat [convExpr e1, " >= ", convExpr e2]
convExpr (Minus _ e1 e2)       = concat [convExpr e1, " - ", convExpr e2]
convExpr (Plus _ e1 e2)        = concat [convExpr e1, " + ", convExpr e2]
convExpr (Multiple _ e1 e2)    = concat [convExpr e1, " * ", convExpr e2]
convExpr (Devide _ e1 e2)      = concat [convExpr e1, " / ", convExpr e2]
--convExpr (UnaryMinus _ e)      = "-" ++ convExpr e
convExpr (UnaryAddress _ e)    = "&" ++ convExpr e
convExpr (UnaryPointer _ e)    = "*" ++ "(" ++ convExpr e ++ ")"
convExpr (CallFunc _ name es)  = concat [show name, "(", convExprs es, ")"]
--convExpr (ArrayAccess _ e1 e2) = concat [convExpr e1, "[", convExpr e2, "]"] 
convExpr (ExprList _ es)       = convExprs es
convExpr (Constant _ int)      = show int
convExpr (IdentExpr _ ident)   = show ident


convExprs :: [Expr] -> String
convExprs = concat . intersperse "," . map convExpr
