//! JSON types

use std::str::FromStr;

use crate::object::Obj;
use crate::schema::{Array, Num, Schema, Str};

/// The six types in JSON.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum InstanceType {
    Null = 0,
    Boolean,
    Object,
    Array,
    Number,
    String,
}

impl InstanceType {
    /// Returns an array of all JSON types.
    pub fn all() -> [InstanceType; 6] {
        use InstanceType::*;
        [Null, Boolean, Object, Array, Number, String]
    }

    /// Returns a schema that checks a type and nothing else.
    pub fn to_schema(self) -> Schema {
        match self {
            InstanceType::Null => Schema::Null,
            InstanceType::Boolean => Schema::Boolean,
            InstanceType::Object => Schema::Object(Obj::Any),
            InstanceType::Array => Schema::Array(Array::Any),
            InstanceType::Number => Schema::Number(Num::Any),
            InstanceType::String => Schema::String(Str::Any),
        }
    }
}

impl FromStr for InstanceType {
    type Err = miette::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "null" => InstanceType::Null,
            "boolean" => InstanceType::Boolean,
            "object" => InstanceType::Object,
            "array" => InstanceType::Array,
            "integer" => InstanceType::Number,
            "number" => InstanceType::Number,
            "string" => InstanceType::String,
            s => miette::bail!("unknown instance type `{s}`"),
        };
        Ok(ty)
    }
}

/// A set of JSON types.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstanceTypeSet {
    /// There are only six types, so we represent a set of them using a
    /// bitfield.
    inner: u8,
}

impl InstanceTypeSet {
    /// The set of all types.
    pub const FULL: InstanceTypeSet = InstanceTypeSet { inner: 0b0011_1111 };
    /// The set of no types.
    pub const EMPTY: InstanceTypeSet = InstanceTypeSet { inner: 0b0000_0000 };

    /// Constructs a set with one type in it.
    pub fn singleton(ty: InstanceType) -> Self {
        Self {
            inner: 1 << ty as u8,
        }
    }

    /// Inserts a type into this set.
    pub fn insert(&mut self, ty: InstanceType) {
        self.inner |= 1 << ty as u8;
    }

    /// Does this set contain `ty`?
    pub fn contains(self, ty: InstanceType) -> bool {
        self.inner & (1 << ty as u8) != 0
    }

    /// Intersects this set with another one, returning the intersection.
    pub fn intersect(self, other: InstanceTypeSet) -> InstanceTypeSet {
        InstanceTypeSet {
            inner: self.inner & other.inner,
        }
    }

    /// Creates a schema that validates whether a value belongs to any of the
    /// types in this type set.
    pub fn to_schema(self) -> Schema {
        let types: Vec<_> = InstanceType::all()
            .into_iter()
            .filter(|ty| self.contains(*ty))
            .collect();

        match types.as_slice() {
            [] => Schema::Never,
            [ty] => ty.to_schema(),
            _ => Schema::AnyOf(types.into_iter().map(InstanceType::to_schema).collect()),
        }
    }

    /// If this set is a singleton, returns the single type that it contains.
    pub fn to_singleton(self) -> Option<InstanceType> {
        if self.inner.count_ones() == 1 {
            InstanceType::all()
                .iter()
                .find(|ty| self.contains(**ty))
                .copied()
        } else {
            None
        }
    }
}

impl FromIterator<InstanceType> for InstanceTypeSet {
    fn from_iter<T: IntoIterator<Item = InstanceType>>(iter: T) -> Self {
        let mut ret = InstanceTypeSet::EMPTY;
        for ty in iter.into_iter() {
            ret.insert(ty);
        }
        ret
    }
}
