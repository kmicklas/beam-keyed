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
import           Data.Hashable
import           Database.Beam

-- | A class for Beam-style row types which have an associated key type.
class HasKey (value :: (* -> *) -> *) where
  -- | Associates a database table's value (non-key) component with its key type.
  -- Defaults to 'IdKeyT', which creates a single-column key type defined by 'Id'.
  type KeyT value :: (* -> *) -> *
  type KeyT value = IdKeyT value

-- | Convenience synonym for 'KeyT' specialized to 'Identity' columns (i.e. pure values).
type Key value = KeyT value Identity

-- | Combines a value type and its key type to form a complete database row type.
data KeyedT (value :: (* -> *) -> *) (f :: * -> *) = Keyed
  { _keyed_key :: KeyT value f
  , _keyed_value :: value f
  } deriving Generic

-- | Convenience synonym for 'KeyedT' specialized to 'Identity' columns (i.e. pure values).
type Keyed value = KeyedT value Identity

-- | A lens to '_keyed_key'.
keyed_key :: Lens' (KeyedT value f) (KeyT value f)
keyed_key = lens _keyed_key $ \(Keyed _ v) k -> Keyed k v

-- | A lens to '_keyed_value'.
keyed_value :: Lens' (KeyedT value f) (value f)
keyed_value = lens _keyed_value $ \(Keyed k _) v -> Keyed k v

deriving instance (HasKey value, Beamable (KeyT value), Beamable value) => Beamable (KeyedT value)
deriving instance (HasKey value, Eq (KeyT value f), Eq (value f)) => Eq (KeyedT value f)
deriving instance (HasKey value, Ord (KeyT value f), Ord (value f)) => Ord (KeyedT value f)
deriving instance (HasKey value, Show (KeyT value f), Show (value f)) => Show (KeyedT value f)
deriving instance (HasKey value, Read (KeyT value f), Read (value f)) => Read (KeyedT value f)
deriving instance (HasKey value, Hashable (KeyT value f), Hashable (value f)) => Hashable (KeyedT value f)
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
deriving newtype instance (Hashable (KeyT value f)) => Hashable (RowKeyT f value)
deriving newtype instance (ToJSON (KeyT value f)) => ToJSON (RowKeyT f value)
deriving newtype instance (FromJSON (KeyT value f)) => FromJSON (RowKeyT f value)
deriving anyclass instance (ToJSON (KeyT value f)) => ToJSONKey (RowKeyT f value)
deriving anyclass instance (FromJSON (KeyT value f)) => FromJSONKey (RowKeyT f value)

-- | Gets the 'PrimaryKey' of a 'Keyed' and switches the argument order for
-- consistency with 'Columnar'.
type RowKeyT f value = PrimaryKey (KeyedT value) f

-- | Convenience synonym for 'RowKeyT' specialized to 'Identity' columns (i.e. pure values).
type RowKey value = RowKeyT Identity value

-- | An isomorphism between 'RowKeyT' and its underlying 'KeyT'.
rowKey_data :: Iso' (RowKeyT f value) (KeyT value f)
rowKey_data = iso _rowKey_data RowKey

-- | Associates a table type with a single column key.
type family Id (value :: (* -> *) -> *) :: *

-- | A single-column primary key whose column type is defined by 'Id'.
newtype IdKeyT value f = IdKey
  { _idKey_data :: C f (Id value) }
  deriving stock (Generic)
  deriving anyclass (Beamable)

-- | Convenience synonym for 'IdKeyT' specialized to 'Identity' columns (i.e. pure values).
type IdKey value = IdKeyT value Identity

-- | Constrains that 'value' has a key type which is the single-column 'Id' type 'a'.
type HasIdKey value a = (HasKey value, KeyT value ~ IdKeyT value, Id value ~ a)

-- | An isomorphism between 'IdKeyT' and its underlying 'Columnar' value.
idKey_data :: Iso' (IdKeyT value f) (C f (Id value))
idKey_data = iso _idKey_data IdKey

deriving instance Eq (C f (Id value)) => Eq (IdKeyT value f)
deriving instance Ord (C f (Id value)) => Ord (IdKeyT value f)
deriving instance Show (C f (Id value)) => Show (IdKeyT value f)
deriving instance Read (C f (Id value)) => Read (IdKeyT value f)
deriving newtype instance Hashable (C f (Id value)) => Hashable (IdKeyT value f)
deriving newtype instance ToJSON (C f (Id value)) => ToJSON (IdKeyT value f)
deriving newtype instance FromJSON (C f (Id value)) => FromJSON (IdKeyT value f)
deriving newtype instance ToJSONKey (C f (Id value)) => ToJSONKey (IdKeyT value f)
deriving newtype instance FromJSONKey (C f (Id value)) => FromJSONKey (IdKeyT value f)

-- | A lens from 'KeyedT' to its 'PrimaryKey' type ('RowKeyT').
keyed_rowKey :: Lens' (KeyedT value f) (RowKeyT f value)
keyed_rowKey = keyed_key . from rowKey_data

-- | Shorthand for 'keyed_key'.
key :: Lens' (KeyedT value f) (KeyT value f)
key = keyed_key

-- | Shorthand for 'keyed_value'.
value :: Lens' (KeyedT value f) (value f)
value = keyed_value

-- | A lens from 'KeyedT' to it underlying key column when the key is defined by 'Id'.
keyed_idKeyData :: HasIdKey value a => Lens' (KeyedT value f) (C f a)
keyed_idKeyData = keyed_key . idKey_data

-- | A lens from 'RowKeyT' to it underlying column when the key is defined by 'Id'.
rowKey_idKeyData :: HasIdKey value a => Iso' (RowKeyT f value) (C f a)
rowKey_idKeyData = rowKey_data . idKey_data
