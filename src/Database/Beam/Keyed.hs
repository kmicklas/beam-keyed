{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Keyed where

import           Control.Lens
import           Data.Aeson
import           Data.Text
import           Database.Beam
import           Database.Beam.Migrate

class HasKey (value :: (* -> *) -> *) where
  -- | Associates a database table's value (non-key) component with its key type.
  type KeyT value :: ((* -> *) -> *)
  type KeyT value = IdKeyT value

type Key value = KeyT value Identity

-- | Combines a value type and its key type to form a complete database row type.
data KeyedT (value :: (* -> *) -> *) (f :: * -> *) = Keyed
  { _keyed_key :: KeyT value f
  , _keyed_value :: value f
  } deriving Generic

type Keyed value = KeyedT value Identity

keyed_key :: Lens' (KeyedT value f) (KeyT value f)
keyed_key = lens _keyed_key $ \(Keyed _ v) k -> Keyed k v

keyed_value :: Lens' (KeyedT value f) (value f)
keyed_value = lens _keyed_value $ \(Keyed k _) v -> Keyed k v

deriving instance (HasKey value, Beamable (KeyT value), Beamable value) => Beamable (KeyedT value)
deriving instance (HasKey value, Eq (KeyT value f), Eq (value f)) => Eq (KeyedT value f)
deriving instance (HasKey value, Ord (KeyT value f), Ord (value f)) => Ord (KeyedT value f)
deriving instance (HasKey value, Show (KeyT value f), Show (value f)) => Show (KeyedT value f)
deriving instance (HasKey value, Read (KeyT value f), Read (value f)) => Read (KeyedT value f)
deriving instance (HasKey value, ToJSON (KeyT value f), ToJSON (value f)) => ToJSON (KeyedT value f)
deriving instance (HasKey value, FromJSON (KeyT value f), FromJSON (value f)) => FromJSON (KeyedT value f)

instance (HasKey value, Beamable (KeyT value), Typeable value, Beamable value) => Table (KeyedT value) where
  newtype PrimaryKey (KeyedT value) f = RowKey
    { _rowKey_data :: (KeyT value f)
    } deriving Generic
  primaryKey = RowKey . _keyed_key

instance (HasKey value, Beamable (KeyT value)) => Beamable (PrimaryKey (KeyedT value))
deriving instance (Eq (KeyT value f)) => Eq (RowKeyT f value)
deriving instance (Ord (KeyT value f)) => Ord (RowKeyT f value)
deriving instance (Show (KeyT value f)) => Show (RowKeyT f value)
deriving instance (Read (KeyT value f)) => Read (RowKeyT f value)
deriving newtype instance (ToJSON (KeyT value f)) => ToJSON (RowKeyT f value)
deriving newtype instance (FromJSON (KeyT value f)) => FromJSON (RowKeyT f value)
deriving anyclass instance (ToJSON (KeyT value f)) => ToJSONKey (RowKeyT f value)
deriving anyclass instance (FromJSON (KeyT value f)) => FromJSONKey (RowKeyT f value)

-- | Gets the 'PrimaryKey' of a 'Keyed' and switches the argument order for
-- consistency with 'C'.
type RowKeyT f value = PrimaryKey (KeyedT value) f

type RowKey value = RowKeyT Identity value

rowKey_data :: Iso' (RowKeyT f value) (KeyT value f)
rowKey_data = iso _rowKey_data RowKey

-- | Associates a table type with a single column ID key.
type family Id (value :: (* -> *) -> *) :: *

-- | A single-column primary key.
newtype IdKeyT value f = IdKey
  { _idKey_data :: C f (Id value) }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type IdKey value = IdKeyT value Identity

type HasIdKey value a = (HasKey value, KeyT value ~ IdKeyT value, Id value ~ a)

idKey_data :: Iso' (IdKeyT value f) (C f (Id value))
idKey_data = iso _idKey_data IdKey

deriving instance Eq (C f (Id value)) => Eq (IdKeyT value f)
deriving instance Ord (C f (Id value)) => Ord (IdKeyT value f)
deriving instance Show (C f (Id value)) => Show (IdKeyT value f)
deriving instance Read (C f (Id value)) => Read (IdKeyT value f)
deriving newtype instance ToJSON (C f (Id value)) => ToJSON (IdKeyT value f)
deriving newtype instance FromJSON (C f (Id value)) => FromJSON (IdKeyT value f)
deriving newtype instance ToJSONKey (C f (Id value)) => ToJSONKey (IdKeyT value f)
deriving newtype instance FromJSONKey (C f (Id value)) => FromJSONKey (IdKeyT value f)

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

keyed_rowKey :: Lens' (KeyedT value f) (RowKeyT f value)
keyed_rowKey = keyed_key . from rowKey_data

-- | Shorthand for 'keyed_key'.
key :: Lens' (KeyedT value f) (KeyT value f)
key = keyed_key

-- | Shorthand for 'keyed_value'.
value :: Lens' (KeyedT value f) (value f)
value = keyed_value

keyed_idKeyData :: HasIdKey value a => Lens' (KeyedT value f) (C f a)
keyed_idKeyData = keyed_key . idKey_data

rowKey_idKeyData :: HasIdKey value a => Iso' (RowKeyT f value) (C f a)
rowKey_idKeyData = rowKey_data . idKey_data
