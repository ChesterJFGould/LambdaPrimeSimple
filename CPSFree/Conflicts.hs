module CPSFree.Conflicts
( conflicts
)
where

import Compiler.Types
import CPSFree.Types

import Control.Monad.Writer
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Set.Ordered as O

conflicts :: Body -> (S.Set Aloc, S.Set (Mloc, Mloc))
conflicts (Body _ expr) = conflictsExpr expr

conflictsExpr :: TExpr -> (S.Set Aloc, S.Set (Mloc, Mloc))
conflictsExpr (free, Set loc triv body) =
        let conflictingLocs = deleteLoc loc (deleteTriv triv free)
            conflicts = conflictsWith loc conflictingLocs
            (bodyAlocs, bodyConflicts) = conflictsExpr body
            alocs = S.unions [locAlocs loc, trivAlocs triv, bodyAlocs]
        in (alocs, S.union conflicts bodyConflicts)
conflictsExpr (free, NumOp _ l r body) =
        let conflictingLocs = deleteLoc l free
            conflicts = conflictsWith l conflictingLocs
            (bodyAlocs, bodyConflicts) = conflictsExpr body
            alocs = S.unions [locAlocs l, locAlocs r, bodyAlocs]
        in (alocs, S.union conflicts bodyConflicts)
conflictsExpr (free, MRef loc place _ body) =
        let conflictingLocs = deleteLoc loc free
            conflicts = conflictsWith loc conflictingLocs
            (bodyAlocs, bodyConflicts) = conflictsExpr body
            alocs = S.unions [locAlocs loc, placeAlocs place, bodyAlocs]
        in (alocs, S.union conflicts bodyConflicts)
conflictsExpr (_, MSet ptr _ val body) =
        let (bodyAlocs, bodyConflicts) = conflictsExpr body
            alocs = S.unions [placeAlocs ptr, placeAlocs val, bodyAlocs]
        in (alocs, bodyConflicts)
conflictsExpr (_, If (RelOp op l r) c a) =
        let (cAlocs, cConflicts) = conflictsExpr c
            (aAlocs, aConflicts) = conflictsExpr a
            alocs = S.unions [locAlocs l, locAlocs r, cAlocs, aAlocs]
        in (alocs, S.union cConflicts aConflicts)
conflictsExpr (_, Jump place _) = (placeAlocs place, S.empty)

conflictsWith :: Mloc -> O.OSet Mloc -> S.Set (Mloc, Mloc)
conflictsWith loc conflicts =
        let conflictsList = F.toList conflicts
        in S.fromList (zip (repeat loc) conflictsList)

deleteTriv :: MTriv -> O.OSet Mloc -> O.OSet Mloc
deleteTriv (MPlace place) set = deletePlace place set
deleteTriv (MWord _) set = set

deletePlace :: MPlace -> O.OSet Mloc -> O.OSet Mloc
deletePlace (MMloc loc) set = deleteLoc loc set
deletePlace (MLabel _) set = set

deleteLoc :: Mloc -> O.OSet Mloc -> O.OSet Mloc
deleteLoc loc set = O.delete loc set

locAlocs :: Mloc -> S.Set Aloc
locAlocs (MAloc aloc) = S.singleton aloc
locAlocs (MReg _) = S.empty

trivAlocs :: MTriv -> S.Set Aloc
trivAlocs (MPlace place) = placeAlocs place
trivAlocs (MWord _) = S.empty

placeAlocs :: MPlace -> S.Set Aloc
placeAlocs (MMloc loc) = locAlocs loc
placeAlocs (MLabel _) = S.empty
