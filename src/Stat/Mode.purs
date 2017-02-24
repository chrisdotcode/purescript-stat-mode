module Stat.Mode
	( FileType
		( Directory
		, File
		, BlockDevice
		, CharacterDevice
		, SymbolicLink
		, NamedPipe
		, Socket
		, Other
		)
	, Scope(Scope)
	, Permission(Read, Write, Execute)
	, Mode(Mode)
	, getFileType
	, getSetUID
	, getSetGID
	, getStickyBit
	, hasUserPermission
	, hasGroupPermission
	, hasOthersPermission
	, getPermissions
	, scope
	, mode
	, toOctalString
	, toOctalString'
	) where

import Prelude
	( class Eq
	, class Ord
	, class Show
	, Ordering(LT, EQ, GT)
	, otherwise
	, show
	, ($)
	, (<>)
	, (==)
	, (/=)
	)

import Data.Array    (filter)
import Data.Generic  (class Generic, gEq, gShow)
import Data.Int.Bits ((.&.))
import Data.Set      (Set(), fromFoldable)

-- Constants (defined in `stat.h`):

-- 0170000 type of file
s_ifmt :: Int
s_ifmt = 61440

-- 0010000 named pipe (fifo)
s_ififo :: Int
s_ififo = 4096

-- 0020000 character special
s_ifchr :: Int
s_ifchr = 8192

-- 0040000 directory
s_ifdir :: Int
s_ifdir = 16384

-- 0060000 block special
s_ifblk :: Int
s_ifblk = 24576

-- 0100000 regular
s_ifreg :: Int
s_ifreg = 32768

-- 0120000 symbolic link
s_iflnk :: Int
s_iflnk = 40960

-- 0140000 socket
s_ifsock :: Int
s_ifsock = 49152

-- 0160000 whiteout
s_ifwht :: Int
s_ifwht = 57344

-- 0004000 set user id on execution
s_isuid :: Int
s_isuid = 2048

-- 0002000 set group id on execution
s_isgid :: Int
s_isgid = 1024

-- 0001000 save swapped text even after use
s_isvtx :: Int
s_isvtx = 512

-- 0000400 read permission, owner
s_irusr :: Int
s_irusr = 256

-- 0000200 write permission, owner
s_iwusr :: Int
s_iwusr = 128

-- 0000100 execute/search permission, owner
s_ixusr :: Int
s_ixusr = 64

-- 0000040 read permission, group
s_irgrp :: Int
s_irgrp = 32

-- 0000020 write permission, group
s_iwgrp :: Int
s_iwgrp = 16

-- 0000010 execute/search permission, group
s_ixgrp :: Int
s_ixgrp = 8

-- 0000004 read permission, others
s_iroth :: Int
s_iroth = 4

-- 0000002 write permission, others
s_iwoth :: Int
s_iwoth = 2

-- 0000001 execute/search permission, others
s_ixoth :: Int
s_ixoth = 1

-- | Represents how a specific scope can access the associated file.
data Permission = Read | Write | Execute

instance showPermission :: Show Permission where
	show    Read = "Read"
	show   Write = "Write"
	show Execute = "Execute"

instance eqPermission :: Eq Permission where
	eq    Read    Read = true
	eq   Write   Write = true
	eq Execute Execute = true
	eq _             _ = true

instance ordPermission :: Ord Permission where
	compare    Read    Read = EQ
	compare   Write   Write = EQ
	compare Execute Execute = EQ
	compare    Read       _ = LT
	compare   Write    Read = GT
	compare   Write Execute = LT
	compare Execute       _ = GT

-- | Represents the various entities that can access files and their associated
-- | permissions.
newtype Scope = Scope
	{ user   :: Set Permission
	, group  :: Set Permission
	, others :: Set Permission
	}

instance showScope :: Show Scope where
	show (Scope s) = "(Scope "
		<> "{ user: "   <> show s.user
		<> ", group: "  <> show s.group
		<> ", others: " <> show s.others
		<> " })"

instance eqScope :: Eq Scope where
	eq s1 s2 = show s1 == show s2

-- XXX Write Ord instance for Scope.

-- | Represents what type of file the associate file is.
data FileType = Directory
	      | File
	      | BlockDevice
	      | CharacterDevice
	      | SymbolicLink
	      | NamedPipe
	      | Socket
	      | Other

derive instance genericFileType :: Generic FileType

instance showFileType :: Show FileType where
	show = gShow

instance eqFileType :: Eq FileType where
	eq = gEq

-- XXX Does FileType even have an Ord instance?

-- | Represents all of the information present in a numeric mode literal.
newtype Mode = Mode
	{ fileType     :: FileType
	, setUID       :: Boolean
	, setGID       :: Boolean
	, stickyBit    :: Boolean
	, scope        :: Scope
	, mode         :: Int
	}

-- XXX Show, Eq and maybe Ord instances for `Mode`. Then go ahead and remove
-- XXX Generics and hand-write them all.

--  | Retrives the `FileType` information from a numeric mode literal.
getFileType :: Int -> FileType
getFileType mode'
	| mode' .&. s_ifmt == s_ifdir  = Directory
	| mode' .&. s_ifmt == s_ifreg  = File
	| mode' .&. s_ifmt == s_ifblk  = BlockDevice
	| mode' .&. s_ifmt == s_ifchr  = CharacterDevice
	| mode' .&. s_ifmt == s_iflnk  = SymbolicLink
	| mode' .&. s_ifmt == s_ififo  = NamedPipe
	| mode' .&. s_ifmt == s_ifsock = Socket
	| otherwise                = Other

-- | Verifies whether or not the setUID bit is set for a given numeric mode
-- | literal.
getSetUID :: Int -> Boolean
getSetUID mode' = mode' .&. s_isuid /= 0

-- | Verifies whether or not the setGID bit is set for a given numeric mode
-- | literal.
getSetGID :: Int -> Boolean
getSetGID mode' = mode' .&. s_isgid /= 0

-- | Verifies whether or not the sticky bit is set for a given numeric mode
-- | literal.
getStickyBit :: Int -> Boolean
getStickyBit mode' = mode' .&. s_isvtx /= 0

hasScopePermission :: Int -> Int -> Boolean
hasScopePermission scope' mode' = mode' .&. scope' /= 0

-- | Verifies whether or not the given user-level permission is set on the
-- | given numeric mode literal.
hasUserPermission :: Permission -> Int -> Boolean
hasUserPermission Read    = hasScopePermission s_irusr
hasUserPermission Write   = hasScopePermission s_iwusr
hasUserPermission Execute = hasScopePermission s_ixusr

-- | Verifies whether or not the given group-level permission is set on the
-- | given numeric mode literal.
hasGroupPermission :: Permission -> Int -> Boolean
hasGroupPermission Read    = hasScopePermission s_irgrp
hasGroupPermission Write   = hasScopePermission s_iwgrp
hasGroupPermission Execute = hasScopePermission s_ixgrp

-- | Verifies whether or not the given others-level permission is set on the
-- | given numeric mode literal.
hasOthersPermission :: Permission -> Int -> Boolean
hasOthersPermission Read    = hasScopePermission s_iroth
hasOthersPermission Write   = hasScopePermission s_iwoth
hasOthersPermission Execute = hasScopePermission s_ixoth

-- | Returns scope-level permissions for a given numeric mode literal.
-- |
-- | The first argument is a function that checks for permissions about a given
-- | scope on a numeric mode literal
-- | (i.e.: `hasUserPermission`, `hasGroupPermission`, or `hasOtherPermission`).
getPermissions :: (Permission -> Int -> Boolean) -> Int -> Set Permission
getPermissions hasPermissions mode' =
	fromFoldable $ filter (\p -> hasPermissions p mode') [Read, Write, Execute]

-- | Returns scope information for the given numeric mode literal.
scope :: Int -> Scope
scope mode' = Scope
	{ user  : getPermissions hasUserPermission   mode'
	, group : getPermissions hasGroupPermission  mode'
	, others: getPermissions hasOthersPermission mode'
	}

-- | Convert a numeric mode literal into a `Mode` data type.
mode :: Int -> Mode
mode m = Mode
	{ fileType : getFileType m
	, setUID   : getSetUID m
	, setGID   : getSetGID m
	, stickyBit: getStickyBit m
	, scope    : scope m
	, mode     : m
	}

-- XXX Implement toLSString

foreign import toOctalStringImpl :: Int -> String

-- | Returns an octal representation of the given numeric mode literal
-- | (e.g.: "0754").
toOctalString' :: Int -> String
toOctalString' = toOctalStringImpl

-- | Returns an octal representation of the given `Mode` (e.g.: "0754").
toOctalString :: Mode -> String
toOctalString (Mode m) = toOctalString' m.mode
