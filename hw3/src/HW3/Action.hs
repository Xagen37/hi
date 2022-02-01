{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf  #-}
module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.ByteString as DBS
import Data.Functor (($>))
import qualified Data.Sequence as DS
import Data.Set (Set, difference, empty, findMin, fromList, isSubsetOf)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random.Stateful (getStdRandom, randomR)

import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))

-- | Permissions, required by some of HiActions.
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

-- | Exception thrown if HiAction has not received required permission.
data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving Functor via ReaderT (Set HiPermission) IO
  deriving Applicative via ReaderT (Set HiPermission) IO
  deriving Monad via ReaderT (Set HiPermission) IO

-- | Accepts HiAction, permissions for it,
-- checks, whether required permissions are given and,
-- if ok, run the action.
instance HiMonad HIO where
  runAction act = HIO $ \perms ->
    if checkPerms perms (mapPerms act)
      then applyAction act
      else throwIO $ PermissionRequired (findMin $ difference (mapPerms act) perms)

-- | Helper function for creating HiMonad instance for HIO.
-- Pattern matches by HiAction and does corresponding IO action.
applyAction :: HiAction -> IO HiValue
applyAction (HiActionWrite pth str) = DBS.writeFile pth str $> HiValueNull
applyAction (HiActionMkDir pth)     = createDirectory pth $> HiValueNull
applyAction (HiActionChDir pth)     = setCurrentDirectory pth $> HiValueNull
applyAction HiActionCwd             = HiValueString . fromString <$> getCurrentDirectory
applyAction HiActionNow             = HiValueTime <$> getCurrentTime
applyAction (HiActionRand l r)      = HiValueNumber . toRational <$> getStdRandom (randomR (l, r))
applyAction (HiActionEcho str)      = print str $> HiValueNull
applyAction (HiActionRead pth)      = readHelper pth

-- | Tries to perform read action on given path.
-- If the path is directory, returns HiValueList of the directory content.
-- If the path is file, reads it.
-- Returns HiValueString if the content was valid UTF-8 and HiValueBytes otherwise.
-- Returns HiValueNull if was given neither path to directory nor to file.
readHelper :: FilePath -> IO HiValue
readHelper pth = do
  directory <- doesDirectoryExist pth
  file      <- doesFileExist pth
  if
    | directory ->
        HiValueList . DS.fromList .
        map (HiValueString . fromString) <$>
        listDirectory pth
    | file -> do
        str <- DBS.readFile pth
        case decodeUtf8' str of
          Left _    -> return $ HiValueBytes str
          Right txt -> return $ HiValueString txt
    | otherwise -> return HiValueNull

-- | Checks, whether action is given required permissions.
checkPerms :: Set HiPermission -> Set HiPermission -> Bool
checkPerms required given = given `isSubsetOf` required

-- | Maps action to required set of permissions.
mapPerms :: HiAction -> Set HiPermission
mapPerms (HiActionRead _)    = readPerm
mapPerms (HiActionWrite _ _) = writePerm
mapPerms (HiActionMkDir _)   = writePerm
mapPerms (HiActionChDir _)   = readPerm
mapPerms HiActionCwd         = readPerm
mapPerms HiActionNow         = timePerm
mapPerms (HiActionRand _ _)  = empty
mapPerms (HiActionEcho _)    = writePerm

-- | Set, consisting of AllowRead permission.
readPerm :: Set HiPermission
readPerm = fromList [AllowRead]

-- | Set, consisting of AllowWrite permission.
writePerm :: Set HiPermission
writePerm = fromList [AllowWrite]

-- | Set, consisting of AllowTime permission.
timePerm :: Set HiPermission
timePerm = fromList [AllowTime]
