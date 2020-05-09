{-# LANGUAGE TypeFamilies #-}
module Database.Beam.Keyed.Migrate where

import           Data.Text
import           Database.Beam
import           Database.Beam.Migrate

import           Database.Beam.Keyed

-- | Construct a 'EntityModification' for a 'Keyed' table.
checkedKeyed
  :: Text -- ^ The table name in the schema.
  -> KeyT value (CheckedFieldModification (KeyedT value))
  -> value (CheckedFieldModification (KeyedT value))
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity (KeyedT value))
checkedKeyed name k v = modifyCheckedTable (const name) $ Keyed
  { _keyed_key = k
  , _keyed_value = v
  }

-- | Construct a 'EntityModification' for a 'Keyed' table with a
-- single-column 'Id'-based primary key which will be named "id" in the schema.
checkedKeyedWithId
  :: (KeyT value ~ IdKeyT value')
  => Text
  -> value (CheckedFieldModification (KeyedT value))
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity (KeyedT value))
checkedKeyedWithId name = checkedKeyed name $ IdKey $ checkedFieldNamed $ pack "id"

-- | Construct a 'CheckedFieldModification' for a foreign key to a 'Keyed'
-- table.
checkedForeignKeyIdFieldNamed
  :: (HasIdKey value a)
  => Text
  -> IdKeyT value (CheckedFieldModification (KeyedT value'))
checkedForeignKeyIdFieldNamed = IdKey . checkedFieldNamed
