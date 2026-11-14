//! A stack of layers read together: the standard library, a library, the program.
//!
//! Each file is validated against the layers below it, so every tagged id in the
//! stack names something that exists. [`Stack`] then resolves an id used in one
//! layer to the layer and entry it names.

use zerocopy::little_endian::U32;

use crate::format::*;
use crate::reader::{Counts, Tast, TastError, lineage};

/// Layers in stacking order, bottom first.
#[derive(Debug, Clone)]
pub struct Stack<'a> {
    layers: Vec<Tast<'a>>,
}

impl<'a> Stack<'a> {
    /// Check each file against the ones below it. A file's `Parents` must be the
    /// identities of exactly the files below it, in order.
    pub fn parse(files: &[&'a [u8]]) -> Result<Stack<'a>, TastError> {
        let mut layers: Vec<Tast<'a>> = Vec::with_capacity(files.len());
        let mut counts: Vec<Counts> = Vec::with_capacity(files.len());
        for (at, bytes) in files.iter().enumerate() {
            // The parents first: ids are only meaningful against the layers named.
            let (_, named) = lineage(bytes)?;
            let below: Vec<u64> = layers.iter().map(Tast::layer_id).collect();
            if named != below {
                return Err(TastError::ParentMismatch { layer: at });
            }
            let layer = Tast::parse_in(bytes, &counts)?;
            counts.push(layer.counts());
            layers.push(layer);
        }
        Ok(Stack { layers })
    }

    pub fn layers(&self) -> &[Tast<'a>] {
        &self.layers
    }

    /// The program: the last layer.
    pub fn top(&self) -> &Tast<'a> {
        self.layers.last().expect("a stack has at least one layer")
    }

    /// The layer and index `id`, used in layer `from`, names.
    pub fn resolve(&self, from: usize, id: U32) -> (usize, usize) {
        match tag::split(id.get()) {
            Some((layer, index)) => (layer as usize, index as usize),
            None => (from, id.get() as usize),
        }
    }

    /// A type named in layer `from`, and the layer it lives in.
    pub fn type_rec(&self, from: usize, id: U32) -> (usize, &'a TypeRec) {
        let (layer, i) = self.resolve(from, id);
        (layer, &self.layers[layer].types()[i])
    }

    pub fn class_rec(&self, from: usize, id: U32) -> (usize, &'a ClassRec) {
        let (layer, i) = self.resolve(from, id);
        (layer, &self.layers[layer].classes()[i])
    }

    pub fn field_rec(&self, from: usize, id: U32) -> (usize, &'a FieldRec) {
        let (layer, i) = self.resolve(from, id);
        (layer, &self.layers[layer].fields()[i])
    }

    pub fn abstract_rec(&self, from: usize, id: U32) -> (usize, &'a AbstractRec) {
        let (layer, i) = self.resolve(from, id);
        (layer, &self.layers[layer].abstracts()[i])
    }

    pub fn enum_rec(&self, from: usize, id: U32) -> (usize, &'a EnumRec) {
        let (layer, i) = self.resolve(from, id);
        (layer, &self.layers[layer].enums()[i])
    }

    pub fn enum_ctor_rec(&self, from: usize, id: U32) -> (usize, &'a EnumCtorRec) {
        let (layer, i) = self.resolve(from, id);
        (layer, &self.layers[layer].enum_ctors()[i])
    }
}
