module HM.Normal where

import HM.Types
import HM.Monad

import Data.IORef
import Data.List( (\\) )

import Debug.Trace

import Text.PrettyPrint.HughesPJ


typecheck :: Term -> Tc Sigma
typecheck e = do { ty <- inferSigma e
                 ; zonkType ty }


data Expected a = Infer (IORef a) | Check a


checkRho :: Term -> Rho -> Tc ()
checkRho expr ty = tcRho expr (Check ty)


inferRho :: Term -> Tc Rho
inferRho expr 
  = do { ref <- newTcRef (error "inferRho: empty result")
       ; tcRho expr (Infer ref)
       ; readTcRef ref }


tcRho :: Term -> Expected Rho -> Tc ()
tcRho (Lit _) exp_ty = instSigma intType exp_ty

tcRho (Var v) exp_ty
  = do { v_sigma <- lookupVar v
       ; instSigma v_sigma exp_ty 
       }

tcRho (App fun arg) exp_ty
  = do { fun_ty <- inferRho fun
       ; (arg_ty, res_ty) <- unifyFun fun_ty
       ; checkSigma arg arg_ty
       ; instSigma res_ty exp_ty 
       }

tcRho (Lam var body) (Check exp_ty)
  = do { (var_ty, body_ty) <- unifyFun exp_ty
       ; extendVarEnv var var_ty (checkRho body body_ty)
       }

tcRho (Lam var body) (Infer ref)
  = do { var_ty  <- newTyVarTy
       ; body_ty <- extendVarEnv var var_ty (inferRho body)
       ; writeTcRef ref (var_ty --> body_ty)
       }

tcRho (Let var rhs body) exp_ty
  = do { var_ty <- inferSigma rhs
       ; traceM $ ("tcRho Let var_ty = " ++ show var_ty)
       ; extendVarEnv var var_ty (tcRho body exp_ty)
       }

tcRho (Ann body ann_ty) exp_ty
   = do { checkSigma body ann_ty
        ; instSigma ann_ty exp_ty 
        }

tcRho (ALam var var_ty body) exp_ty = fail "annotated lambda is not supported in hindley milner"


inferSigma :: Term -> Tc Sigma
inferSigma e
   = do { exp_ty <- inferRho e
        ; traceM $ ("inferRho result = " ++ show exp_ty)
        ; env_tys <- getEnvTypes
        ; env_tvs <- getMetaTyVars env_tys
        ; res_tvs <- getMetaTyVars [exp_ty]
        ; let forall_tvs = res_tvs \\ env_tvs
        ; quantify forall_tvs exp_ty }


checkSigma :: Term -> Sigma -> Tc ()
checkSigma expr sigma
  = do { (skol_tvs, rho) <- shallowskol sigma
       ; checkRho expr rho
       ; env_tys <- getEnvTypes
       ; esc_tvs <- getFreeTyVars (sigma : env_tys)
       ; let bad_tvs = filter (`elem` esc_tvs) skol_tvs
       ; check (null bad_tvs) (text "Type not polymorphic enough") 
       }



instSigma :: Sigma -> Expected Rho -> Tc ()
instSigma t1 (Check t2) = unify t1 t2
instSigma t1 (Infer r)  = do { t1' <- instantiate t1
                             ; writeTcRef r t1'
                             }
