{-# LANGUAGE FlexibleInstances #-}

-- | 

module Haskell.Functor.FunctorExercises where
import GHC.Arr
newtype Mu f = InF {outF :: f (Mu f)}


-- instance Functor f => Functor (Wrap f) where
  -- fmap f (Wrap fa) = Wrap (fmap f fa)

-- instance  Functor Mu where
  -- fmap f (InF fmu) = InF (fmap f fmu)



data D =
  D (Array Word Word) Int Int

-- instance Functor (D (Array Word Word) Int) where
  -- fmap f (D a i1 i) = D a  i1 (f i) 


data Sum b a =
  First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b


data Company a  c b =
  DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
  
data More b a =
  L a b a
  | R b a b
  deriving (Eq ,Show)


instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data Quant a b =
  Finance
  | Desk a
  | Bloor b
  deriving (Show ,Eq)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K' a b =
  K' a

instance Functor (K' a) where
  fmap _ (K' a) = K' a


newtype Flip f a b =
  Flip (f b a)
  deriving (Eq , Show)

newtype K a b = K a

instance  Functor (Flip K a) where
   fmap f (Flip (K a)) = Flip $  K (f a) 

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a=
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a=
  DaWrappa (f a) (g a)

instance (Functor g, Functor f) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap   h ga)

data IgnoreOne f g a b =
  IgnoreingSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoreingSomething fa gb) = IgnoreingSomething  fa $ fmap h gb

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt


data List a =
  Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) $ fmap f la

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) =  OneGoat (f a)
  fmap f (MoreGoats ga gb gc) = MoreGoats (fmap f ga)  (fmap f gb)  (fmap f gc)


data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s  (f a)
  fmap f (Read sa) = Read (fmap f sa)


