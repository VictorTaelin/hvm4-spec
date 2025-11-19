--[./../spec.md]

{-# LANGUAGE BangPatterns, CPP #-}

import Control.Monad (forM_, when)
import Data.Bits (shiftL)
import Data.Char (isDigit)
import Data.IORef
import Data.List (foldl', elemIndex)
import System.CPUTime
import Text.ParserCombinators.ReadP
import Text.Printf
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

debug :: Bool
debug = False

-- Types
-- =====

type Lab  = Int
type Name = Int

data Term
  = Var !Name
  | Dp0 !Name
  | Dp1 !Name
  | Ref !Name
  | Nam !String
  | Dry !Term !Term
  | Era
  | Sup !Lab !Term !Term
  | Dup !Name !Lab !Term !Term
  | Set
  | All !Term !Term
  | Lam !Name !Term
  | App !Term !Term
  | Sig !Term !Term
  | Tup !Term !Term
  | Get !Term
  | Emp
  | Efq
  | Uni
  | One
  | Use !Term
  | Bol
  | Fal
  | Tru
  | If !Term !Term
  | Nat
  | Zer
  | Suc !Term
  | Swi !Term !Term
  | Lst !Term
  | Nil
  | Con !Term !Term
  | Mat !Term !Term
  | And !Term !Term
  | Eql !Term !Term
  | Gua !Term !Term
  | Gen !Name !Term !Term !Term !Term
  deriving (Eq)

data Kind
  = VAR
  | DP0
  | DP1
  deriving (Enum)

data Book = Book (M.Map Name Term)

data Env = Env
  { env_book    :: !Book
  , env_inters  :: !(IORef Int)
  , env_new_id  :: !(IORef Int)
  , env_sub_map :: !(IORef (IM.IntMap Term))
  , env_dup_map :: !(IORef (IM.IntMap (Lab, Term)))
  }

-- Showing
-- =======

instance Show Term where
  show term = case term of
    Var k       -> int_to_name k
    Dp0 k       -> int_to_name k ++ "₀"
    Dp1 k       -> int_to_name k ++ "₁"
    Ref k       -> "@" ++ int_to_name k
    Nam k       -> k
    Dry f x     -> show_app f [x]
    Era         -> "&{}"
    Sup l a b   -> "&" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
    Dup k l v t -> "!" ++ int_to_name k ++ "&" ++ int_to_name l ++ "=" ++ show v ++ ";" ++ show t
    Set         -> "*"
    All a b     -> "∀" ++ show a ++ "." ++ show b
    Lam k f     -> "λ" ++ int_to_name k ++ "." ++ show f
    App f x     -> show_app f [x]
    Sig a b     -> "Σ" ++ show a ++ "." ++ show b
    Tup a b     -> "(" ++ show a ++ "," ++ show b ++ ")"
    Get c       -> "λ{,:" ++ show c ++ "}"
    Emp         -> "⊥"
    Efq         -> "λ{}"
    Uni         -> "⊤"
    One         -> "()"
    Use u       -> "λ{():" ++ show u ++ "}"
    Bol         -> "𝔹"
    Fal         -> "#F"
    Tru         -> "#T"
    If f t      -> "λ{#F:" ++ show f ++ ";#T:" ++ show t ++ "}"
    Nat         -> "ℕ"
    Zer         -> "0"
    Suc p       -> show_add 1 p
    Swi z s     -> "λ{0:" ++ show z ++ ";1+:" ++ show s ++ "}"
    Lst t       -> show t ++ "[]"
    Nil         -> "[]"
    Con h t     -> show h ++ "<>" ++ show t
    Mat n c     -> "λ{[]:" ++ show n ++ ";<>:" ++ show c ++ "}"
    And a b     -> show a ++ "&&" ++ show b
    Eql a b     -> show a ++ "==" ++ show b
    Gua f g     -> show f ++ "~>" ++ show g

show_add :: Int -> Term -> String
show_add n Zer     = show n
show_add n (Suc p) = show_add (n + 1) p
show_add n term    = show n ++ "+" ++ show term

show_app :: Term -> [Term] -> String
show_app (Dry f x) args = show_app f (x : args)
show_app (App f x) args = show_app f (x : args)
show_app f         args = "(" ++ unwords (map show (f : args)) ++ ")"

instance Show Book where
  show (Book m) = unlines [ "@" ++ int_to_name k ++ " = " ++ show ct | (k, ct) <- M.toList m ]

show_dup_map :: IM.IntMap (Lab, Term) -> String
show_dup_map m = unlines [ "! " ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v | (k, (l, v)) <- IM.toList m ]

show_sub_map :: IM.IntMap Term -> String
show_sub_map m = unlines [ int_to_name (k `div` 4) ++ suffix (k `mod` 4) ++ " ← " ++ show v | (k, v) <- IM.toList m ]
  where suffix x = case x of { 0 -> "" ; 1 -> "₀" ; 2 -> "₁" ; _ -> "?" }

-- Name Encoding/Decoding
-- ======================

alphabet :: String
alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

alphabet_first :: String
alphabet_first = filter (`notElem` "_0123456789") alphabet

name_to_int :: String -> Int
name_to_int = foldl' (\acc c -> (acc `shiftL` 6) + idx c) 0
  where idx c = maybe (error "bad name char") id (elemIndex c alphabet)

int_to_name :: Int -> String
int_to_name 0 = "_"
int_to_name n = reverse (go n)
  where go 0 = ""
        go m = let (q,r) = m `divMod` 64 in alphabet !! r : go q

-- Parsing
-- =======

lexeme :: ReadP a -> ReadP a
lexeme p = skipSpaces *> p

#include "./parse/name.hs"
#include "./parse/term.hs"
#include "./parse/op_and.hs"
#include "./parse/op_eql.hs"
#include "./parse/op_gua.hs"
#include "./parse/op_lst.hs"
#include "./parse/op_con.hs"
#include "./parse/par.hs"
#include "./parse/lam.hs"
#include "./parse/dup.hs"
#include "./parse/sup.hs"
#include "./parse/era.hs"
#include "./parse/set.hs"
#include "./parse/all.hs"
#include "./parse/sig.hs"
#include "./parse/nat.hs"
#include "./parse/ref.hs"
#include "./parse/add.hs"
#include "./parse/num.hs"
#include "./parse/number.hs"
#include "./parse/var.hs"
#include "./parse/emp.hs"
#include "./parse/uni.hs"
#include "./parse/bol.hs"
#include "./parse/ctr.hs"
#include "./parse/nil.hs"
#include "./parse/nam.hs"
#include "./parse/func.hs"
#include "./parse/book.hs"

read_term :: String -> Term
read_term s = case readP_to_S (parse_term <* skipSpaces <* eof) s of
  [(t, "")] -> t
  _         -> error "bad-parse"

read_book :: String -> Book
read_book s = case readP_to_S parse_book s of
  [(b, "")] -> b
  _         -> error "bad-parse"

-- Environment
-- ===========

new_env :: Book -> IO Env
new_env bk = do
  itr <- newIORef 0
  ids <- newIORef 1
  sub <- newIORef IM.empty
  dm  <- newIORef IM.empty
  return $ Env bk itr ids sub dm

inc_inters :: Env -> IO ()
inc_inters e = do
  !n <- readIORef (env_inters e)
  writeIORef (env_inters e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (env_new_id e)
  writeIORef (env_new_id e) (n + 1)
  return ((n `shiftL` 6) + 63)

taker :: IORef (IM.IntMap a) -> Int -> IO (Maybe a)
taker ref k = do
  !m <- readIORef ref
  case IM.lookup k m of
    Nothing -> do
      return Nothing
    Just v  -> do
      writeIORef ref (IM.delete k m)
      return (Just v)

take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
take_dup e k = taker (env_dup_map e) k

take_sub :: Kind -> Env -> Name -> IO (Maybe Term)
take_sub ki e k = taker (env_sub_map e) (k `shiftL` 2 + fromEnum ki)

make_dup :: Env -> Name -> Lab -> Term -> IO ()
make_dup e k l v = modifyIORef' (env_dup_map e) (IM.insert k (l, v))

make_auto_dup :: Env -> Name -> Lab -> Term -> IO ()
make_auto_dup e k l v = do { k <- fresh e ; make_dup e k l v }

subst :: Kind -> Env -> Name -> Term -> IO ()
subst s e k v = modifyIORef' (env_sub_map e) (IM.insert (k `shiftL` 2 + fromEnum s) v)

-- Cloning
-- =======

clone :: Env -> Lab -> Term -> IO (Term, Term)
clone e l v = do
  k <- fresh e
  make_dup e k l v
  return $ (Dp0 k , Dp1 k)

clone_list :: Env -> Lab -> [Term] -> IO ([Term],[Term])
clone_list e l []       = return $ ([],[])
clone_list e l (x : xs) = do
  (x0  , x1 ) <- clone e l x
  (xs0 , xs1) <- clone_list e l xs
  return $ (x0 : xs0 , x1 : xs1)

-- WNF: Weak Normal Form
-- =====================

-- now, complete with the rest of the file below:

data Frame
  = FDp0   Name Lab -- ! F &L = _; F₀
  | FDp1   Name Lab -- ! F &L = _; F₁
  | FApp   Term     -- (_ x)
  | FAppF  Term     -- (λ{..} _)
  | FAppG  Term     -- ((f~>_) x)
  | FAppGF Term     -- ((f~>λ{..}) _)
  | FAnd   Term     -- (_ && b)
  | FEql   Term     -- (_ == b)
  | FEqlA  Term     -- (a == _)
  deriving Show

type Stack = [Frame]

wnf :: Env -> Stack -> Term -> IO Term
wnf = wnf_enter

type WnfDup    = Env -> Stack -> Term -> Name -> Lab -> Term -> IO Term
type WnfEql    = Env -> Stack -> Term -> Term -> IO Term
type WnfGuaSwi = Env -> Stack -> Term -> Term -> Term -> Term -> IO Term

#include "./wnf/enter.hs"
#include "./wnf/unwind.hs"
#include "./wnf/sub.hs"
#include "./wnf/dup.hs"
#include "./wnf/app.hs"
#include "./wnf/and.hs"
#include "./wnf/eql.hs"
#include "./wnf/ref.hs"

-- Allocation
-- ==========

-- Allocates a closed term, replacing all bound names with fresh ones.
alloc :: Env -> Term -> IO Term
alloc e term = go IM.empty term where

  go :: IM.IntMap Name -> Term -> IO Term

  go m (Var k) = do
    return $ Var (IM.findWithDefault k k m)

  go m (Dp0 k) = do
    return $ Dp0 (IM.findWithDefault k k m)

  go m (Dp1 k) = do
    return $ Dp1 (IM.findWithDefault k k m)

  go _ Era = do
    return Era

  go m (Sup l a b) = do
    a' <- go m a
    b' <- go m b
    return $ Sup l a' b'

  go _ Set = do
    return Set

  go m (All a b) = do
    a' <- go m a
    b' <- go m b
    return $ All a' b'

  go m (App f x) = do
    f' <- go m f
    x' <- go m x
    return $ App f' x'

  go _ Nat = do
    return Nat

  go _ Zer = do
    return Zer

  go m (Suc n) = do
    n' <- go m n
    return $ Suc n'

  go m (Swi z s) = do
    z' <- go m z
    s' <- go m s
    return $ Swi z' s'

  go m (And a b) = do
    a' <- go m a
    b' <- go m b
    return $ And a' b'

  go m (Eql a b) = do
    a' <- go m a
    b' <- go m b
    return $ Eql a' b'

  go _ (Ref k) = do
    return $ Ref k

  go _ (Nam k) = do
    return $ Nam k

  go m (Dry f x) = do
    f' <- go m f
    x' <- go m x
    return $ Dry f' x'

  go m (Gua f g) = do
    f' <- go m f
    g' <- go m g
    return $ Gua f' g'

  go m (Sig a b) = do
    a' <- go m a
    b' <- go m b
    return $ Sig a' b'

  go m (Tup a b) = do
    a' <- go m a
    b' <- go m b
    return $ Tup a' b'

  go m (Get c) = do
    c' <- go m c
    return $ Get c'

  go _ Emp = do
    return Emp

  go _ Efq = do
    return Efq

  go _ Uni = do
    return Uni

  go _ One = do
    return One

  go m (Use u) = do
    u' <- go m u
    return $ Use u'

  go _ Bol = do
    return Bol

  go _ Fal = do
    return Fal

  go _ Tru = do
    return Tru

  go m (If f t) = do
    f' <- go m f
    t' <- go m t
    return $ If f' t'

  go m (Lst t) = do
    t' <- go m t
    return $ Lst t'

  go _ Nil = do
    return Nil

  go m (Con h t) = do
    h' <- go m h
    t' <- go m t
    return $ Con h' t'

  go m (Mat n c) = do
    n' <- go m n
    c' <- go m c
    return $ Mat n' c'

  go m (Dup k l v t) = do
    k' <- fresh e
    v' <- go m v
    t' <- go (IM.insert k k' m) t
    return $ Dup k' l v' t'

  go m (Lam k f) = do
    k' <- fresh e
    f' <- go (IM.insert k k' m) f
    return $ Lam k' f'

-- Normalization
-- =============

snf :: Env -> Int -> Term -> IO Term
snf e d x = do
  !x' <- wnf e [] x
  case x' of

    Var k -> do
      return $ Var k

    Dp0 k -> do
      return $ Dp0 k

    Dp1 k -> do
      return $ Dp1 k

    Era -> do
      return Era

    Sup l a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ Sup l a' b'

    Dup k l v t -> do
      subst DP0 e k (Nam (int_to_name d ++ "₀"))
      subst DP1 e k (Nam (int_to_name d ++ "₁"))
      v' <- snf e d v
      t' <- snf e d t
      return $ Dup d l v' t'

    Set -> do
      return Set
      
    All a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ All a' b'

    Lam k f -> do
      subst VAR e k (Nam (int_to_name d))
      f' <- snf e (d + 1) f
      return $ Lam d f'

    App f x -> do
      f' <- snf e d f
      x' <- snf e d x
      return $ App f' x'

    Nat -> do
      return Nat

    Zer -> do
      return Zer

    Suc p -> do
      p' <- snf e d p
      return $ Suc p'

    Swi z s -> do
      z' <- snf e d z
      s' <- snf e d s
      return $ Swi z' s'

    And a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ And a' b'

    Eql a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ Eql a' b'

    Ref k -> do
      return $ Ref k

    Nam k -> do
      return $ Nam k

    Dry f x -> do
      f' <- snf e d f
      x' <- snf e d x
      return $ Dry f' x'

    Gua f g -> do
      g' <- snf e d g
      return g'

    Sig a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ Sig a' b'

    Tup a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ Tup a' b'

    Get c -> do
      c' <- snf e d c
      return $ Get c'

    Emp -> do
      return Emp

    Efq -> do
      return Efq

    Uni -> do
      return Uni

    One -> do
      return One

    Use u -> do
      u' <- snf e d u
      return $ Use u'

    Bol -> do
      return Bol

    Fal -> do
      return Fal

    Tru -> do
      return Tru

    If f t -> do
      f' <- snf e d f
      t' <- snf e d t
      return $ If f' t'

    Lst t -> do
      t' <- snf e d t
      return $ Lst t'

    Nil -> do
      return Nil

    Con h t -> do
      h' <- snf e d h
      t' <- snf e d t
      return $ Con h' t'

    Mat n c -> do
      n' <- snf e d n
      c' <- snf e d c
      return $ Mat n' c'

-- Collapsing
-- ==========

collapse :: Env -> Term -> IO Term
collapse e x = do
  !x <- wnf e [] x
  case x of

    Era -> do
      return Era

    (Sup l a b) -> do
      a' <- collapse e a
      b' <- collapse e b
      return $ Sup l a' b'

    Set -> do
      return Set

    All a b -> do
      aV <- fresh e
      bV <- fresh e
      a' <- collapse e a
      b' <- collapse e b
      inj e (Lam aV (Lam bV (All (Var aV) (Var bV)))) [a',b']

    (Lam k f) -> do
      fV <- fresh e
      f' <- collapse e f
      inj e (Lam fV (Lam k (Var fV))) [f']

    (App f x) -> do
      fV <- fresh e
      xV <- fresh e
      f' <- collapse e f
      x' <- collapse e x
      inj e (Lam fV (Lam xV (App (Var fV) (Var xV)))) [f',x']

    Nat -> do
      return Nat

    Zer -> do
      return Zer

    (Suc p) -> do
      pV <- fresh e
      p' <- collapse e p
      inj e (Lam pV (Suc (Var pV))) [p']

    (Swi z s) -> do
      zV <- fresh e
      sV <- fresh e
      z' <- collapse e z
      s' <- collapse e s
      inj e (Lam zV (Lam sV (Swi (Var zV) (Var sV)))) [z',s']

    (And a b) -> do
      aV <- fresh e
      bV <- fresh e
      a' <- collapse e a
      b' <- collapse e b
      inj e (Lam aV (Lam bV (And (Var aV) (Var bV)))) [a',b']

    (Eql a b) -> do
      aV <- fresh e
      bV <- fresh e
      a' <- collapse e a
      b' <- collapse e b
      inj e (Lam aV (Lam bV (Eql (Var aV) (Var bV)))) [a',b']

    Nam n -> do
      return $ Nam n

    Dry f x -> do
      fV <- fresh e
      xV <- fresh e
      f' <- collapse e f
      x' <- collapse e x
      inj e (Lam fV (Lam xV (Dry (Var fV) (Var xV)))) [f',x']

    (Gua f g) -> do
      collapse e g

    Sig a b -> do
      aV <- fresh e
      bV <- fresh e
      a' <- collapse e a
      b' <- collapse e b
      inj e (Lam aV (Lam bV (Sig (Var aV) (Var bV)))) [a',b']

    Tup a b -> do
      aV <- fresh e
      bV <- fresh e
      a' <- collapse e a
      b' <- collapse e b
      inj e (Lam aV (Lam bV (Tup (Var aV) (Var bV)))) [a',b']

    Get c -> do
      cV <- fresh e
      c' <- collapse e c
      inj e (Lam cV (Get (Var cV))) [c']

    Emp -> do
      return Emp

    Efq -> do
      return Efq

    Uni -> do
      return Uni

    One -> do
      return One

    Use u -> do
      uV <- fresh e
      u' <- collapse e u
      inj e (Lam uV (Use (Var uV))) [u']

    Bol -> do
      return Bol

    Fal -> do
      return Fal

    Tru -> do
      return Tru

    If f t -> do
      fV <- fresh e
      tV <- fresh e
      f' <- collapse e f
      t' <- collapse e t
      inj e (Lam fV (Lam tV (If (Var fV) (Var tV)))) [f',t']

    Lst t -> do
      tV <- fresh e
      t' <- collapse e t
      inj e (Lam tV (Lst (Var tV))) [t']

    Nil -> do
      return Nil

    Con h t -> do
      hV <- fresh e
      tV <- fresh e
      h' <- collapse e h
      t' <- collapse e t
      inj e (Lam hV (Lam tV (Con (Var hV) (Var tV)))) [h',t']

    Mat n c -> do
      nV <- fresh e
      cV <- fresh e
      n' <- collapse e n
      c' <- collapse e c
      inj e (Lam nV (Lam cV (Mat (Var nV) (Var cV)))) [n',c']

    x -> do
      return $ x

inj :: Env -> Term -> [Term] -> IO Term
inj e f (x : xs) = do
  !x' <- wnf e [] x
  case x' of
    (Sup l a b) -> do
      (f0  , f1 ) <- clone e l f
      (xs0 , xs1) <- clone_list e l xs
      a' <- inj e f0 (a : xs0)
      b' <- inj e f1 (b : xs1)
      return $ Sup l a' b'
    x' -> do
      inj e (App f x') xs
inj e f [] = do
  return $ f

-- Flattening
-- ==========

flatten :: Term -> [Term]
flatten term = bfs [term] [] where
  bfs []     acc = reverse acc
  bfs (t:ts) acc = case t of
    Sup _ a b -> bfs (ts ++ [a, b]) acc
    _         -> bfs ts (t : acc)

-- Main
-- ====

run :: String -> String -> IO ()
run book_src term_src = do
  !env <- new_env $ read_book book_src
  !ini <- getCPUTime
  !val <- alloc env $ read_term term_src
  !val <- collapse env val
  !val <- snf env 1 val
  !end <- getCPUTime
  !itr <- readIORef (env_inters env)
  !dt  <- return $ fromIntegral (end - ini) / (10^12)
  !ips <- return $ fromIntegral itr / dt
  putStrLn $ show val
  putStrLn $ "- Itrs: " ++ show itr ++ " interactions"
  printf "- Time: %.3f seconds\n" (dt :: Double)
  printf "- Perf: %.2f M interactions/s\n" (ips / 1000000 :: Double)

#include "test.hs"

main :: IO ()
main = test

-- refactor alloc so that ALL clauses of the 'go' function use the do notation
-- no clause should use <$> or <*>
-- do not align the = sign
