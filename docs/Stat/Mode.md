## Module Stat.Mode

#### `Permission`

``` purescript
data Permission
  = Read
  | Write
  | Execute
```

Represents how a specific scope can access the associated file.

##### Instances
``` purescript
Show Permission
Eq Permission
Ord Permission
```

#### `Scope`

``` purescript
newtype Scope
  = Scope { "user" :: Set Permission, "group" :: Set Permission, "others" :: Set Permission }
```

Represents the various entities that can access files and their associated
permissions.

##### Instances
``` purescript
Show Scope
Eq Scope
```

#### `FileType`

``` purescript
data FileType
  = Directory
  | File
  | BlockDevice
  | CharacterDevice
  | SymbolicLink
  | NamedPipe
  | Socket
  | Other
```

Represents what type of file the associate file is.

##### Instances
``` purescript
Generic FileType
Show FileType
Eq FileType
```

#### `Mode`

``` purescript
newtype Mode
  = Mode { "fileType" :: FileType, "setUID" :: Boolean, "setGID" :: Boolean, "stickyBit" :: Boolean, "scope" :: Scope, "mode" :: Int }
```

Represents all of the information present in a numeric mode literal.

#### `getFileType`

``` purescript
getFileType :: Int -> FileType
```

Retrives the `FileType` information from a numeric mode literal.

#### `getSetUID`

``` purescript
getSetUID :: Int -> Boolean
```

Verifies whether or not the setUID bit is set for a given numeric mode
literal.

#### `getSetGID`

``` purescript
getSetGID :: Int -> Boolean
```

Verifies whether or not the setGID bit is set for a given numeric mode
literal.

#### `getStickyBit`

``` purescript
getStickyBit :: Int -> Boolean
```

Verifies whether or not the sticky bit is set for a given numeric mode
literal.

#### `hasUserPermission`

``` purescript
hasUserPermission :: Permission -> Int -> Boolean
```

Verifies whether or not the given user-level permission is set on the
given numeric mode literal.

#### `hasGroupPermission`

``` purescript
hasGroupPermission :: Permission -> Int -> Boolean
```

Verifies whether or not the given group-level permission is set on the
given numeric mode literal.

#### `hasOthersPermission`

``` purescript
hasOthersPermission :: Permission -> Int -> Boolean
```

Verifies whether or not the given others-level permission is set on the
given numeric mode literal.

#### `getPermissions`

``` purescript
getPermissions :: (Permission -> Int -> Boolean) -> Int -> Set Permission
```

Returns scope-level permissions for a given numeric mode literal.

The first argument is a function that checks for permissions about a given
scope on a numeric mode literal
(i.e.: `hasUserPermission`, `hasGroupPermission`, or `hasOtherPermission`).

#### `scope`

``` purescript
scope :: Int -> Scope
```

Returns scope information for the given numeric mode literal.

#### `mode`

``` purescript
mode :: Int -> Mode
```

Convert a numeric mode literal into a `Mode` data type.

#### `toOctalString'`

``` purescript
toOctalString' :: Int -> String
```

Returns an octal representation of the given numeric mode literal
(e.g.: "0754").

#### `toOctalString`

``` purescript
toOctalString :: Mode -> String
```

Returns an octal representation of the given `Mode` (e.g.: "0754").


