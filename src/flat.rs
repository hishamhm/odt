//! Facilities for working with the Flattened Device Tree (FDT) binary format,
//! also known as a Devicetree Blob (DTB).

use crate::BinaryNode;
use core::fmt::{Debug, Display, Formatter};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

const FDT_HEADER_SIZE: usize = 40;
const FDT_MAGIC: u32 = 0xd00dfeed;

#[derive(Copy, Clone, Eq, PartialEq, FromPrimitive)]
enum FdtToken {
    BeginNode = 1,
    EndNode = 2,
    Prop = 3,
    #[allow(dead_code)]
    Nop = 4,
    End = 9,
}

#[derive(Debug, Eq, PartialEq)]
pub enum DeserializeError {
    Truncated,
    Other(&'static str),
}

impl From<&'static str> for DeserializeError {
    fn from(message: &'static str) -> DeserializeError {
        DeserializeError::Other(message)
    }
}

impl Display for DeserializeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&self, f)
    }
}

impl core::error::Error for DeserializeError {}

/// Parse a DTB.
pub fn deserialize(blob: &[u8]) -> Result<BinaryNode, DeserializeError> {
    let mut header = blob;
    let magic = header.read_u32()?;
    let totalsize = header.read_u32()? as usize;
    let off_dt_struct = header.read_u32()? as usize;
    let off_dt_strings = header.read_u32()? as usize;
    let _off_dt_rsvmap = header.read_u32()? as usize;
    let version = header.read_u32()?;
    let last_comp_version = header.read_u32()?;
    let _boot_cpuid_phys = header.read_u32()?;
    let size_dt_strings = header.read_u32()? as usize;
    let size_dt_struct = header.read_u32()? as usize;
    if magic != FDT_MAGIC {
        return Err("bad magic".into());
    }
    if totalsize > blob.len() {
        return Err("truncated".into());
    }
    if version < 17 || last_comp_version > 17 {
        return Err("unsupported version".into());
    }
    if off_dt_struct > totalsize || off_dt_strings > totalsize {
        return Err("invalid header".into());
    }
    if size_dt_struct > totalsize - off_dt_struct || size_dt_strings > totalsize - off_dt_strings {
        return Err("invalid header".into());
    }
    let mut dt_struct = &blob[off_dt_struct..off_dt_struct + size_dt_struct];
    let dt_strings = &blob[off_dt_strings..off_dt_strings + size_dt_strings];

    let node = match dt_struct.read_token()? {
        FdtToken::BeginNode => {
            // discard the name of the root node
            dt_struct.read_cstr()?;
            dt_struct.realign_to(blob)?;
            deserialize_node(&mut dt_struct, dt_strings)?
        }
        FdtToken::End => {
            // Given "/delete-node/ &{/};", `dtc` will produce a DTB with no root node.
            // Treat that as an empty root node.
            return Ok(BinaryNode::default());
        }
        _ => return Err("unexpected start token".into()),
    };
    if dt_struct.read_token()? != FdtToken::End {
        return Err("missing end token".into());
    }
    Ok(node)
}

fn deserialize_node(stream: &mut &[u8], strtab: &[u8]) -> Result<BinaryNode, DeserializeError> {
    let mut node = BinaryNode::default();
    let frame: &[u8] = stream;
    loop {
        match stream.read_token()? {
            FdtToken::BeginNode => {
                let name = stream.read_cstr()?;
                stream.realign_to(frame)?;
                *node.add_child(&name) = deserialize_node(stream, strtab)?;
            }
            FdtToken::EndNode => return Ok(node),
            FdtToken::Prop => {
                let value_len = stream.read_u32()?;
                let name_offset = stream.read_u32()?;
                let value = stream.read_bytes(value_len as usize)?;
                let name = strtab.pread_cstr(name_offset as usize)?;
                stream.realign_to(frame)?;
                node.set_property(&name, value);
            }
            FdtToken::Nop => (),
            FdtToken::End => return Err("unexpected end token".into()),
        }
    }
}

/// Construct a DTB.
/// Memory reservations are not supported.
pub fn serialize(root: &BinaryNode) -> Vec<u8> {
    let mut out = Vec::<u8>::new();
    let mut strings = StringTable::default();

    /*
    struct fdt_header {
        uint32_t magic;
        uint32_t totalsize;
        uint32_t off_dt_struct;
        uint32_t off_dt_strings;
        uint32_t off_mem_rsvmap;
        uint32_t version;
        uint32_t last_comp_version;
        uint32_t boot_cpuid_phys;
        uint32_t size_dt_strings;
        uint32_t size_dt_struct;
    };
    */
    out.write_u32(FDT_MAGIC);
    out.write_u32(0); // totalsize not yet known
    out.write_u32(0); // off_dt_struct not yet known
    out.write_u32(0); // off_dt_strings not yet known
    out.write_u32(FDT_HEADER_SIZE as u32);
    out.write_u32(17);
    out.write_u32(16);
    out.write_u32(0);
    out.write_u32(0); // size_dt_strings not yet known
    out.write_u32(0); // size_dt_struct not yet known

    // memory reservations block
    out.write_u64(0);
    out.write_u64(0);

    // structure block
    let off_dt_struct = out.tell();
    serialize_inner(&mut out, &mut strings, "", root);
    out.write_token(FdtToken::End);
    let size_dt_struct = out.tell() - off_dt_struct;

    // strings block
    let off_dt_strings = out.tell();
    strings.serialize(&mut out);
    let size_dt_strings = out.tell() - off_dt_strings;
    let total_size = out.tell();

    out.pwrite_u32(4, total_size);
    out.pwrite_u32(8, off_dt_struct);
    out.pwrite_u32(12, off_dt_strings);
    out.pwrite_u32(32, size_dt_strings);
    out.pwrite_u32(36, size_dt_struct);

    out
}

fn serialize_inner(out: &mut Vec<u8>, strings: &mut StringTable, name: &str, node: &BinaryNode) {
    out.write_token(FdtToken::BeginNode);
    out.write_string(name);
    out.align();
    for (name, value) in node.properties() {
        out.write_token(FdtToken::Prop);
        out.write_u32(value.len().try_into().unwrap());
        out.write_u32(strings.intern(name));
        out.extend_from_slice(value);
        out.align();
    }
    for (name, child) in node.children() {
        serialize_inner(out, strings, name, child);
    }
    out.write_token(FdtToken::EndNode);
}

trait FdtWriter {
    fn write_u32(&mut self, value: u32);
    fn write_u64(&mut self, value: u64);
    fn align(&mut self);
    fn tell(&self) -> u32;
    fn pwrite_u32(&mut self, offset: usize, value: u32);
    fn write_token(&mut self, token: FdtToken);
    fn write_string(&mut self, s: &str) -> u32;
}

impl FdtWriter for Vec<u8> {
    fn write_u32(&mut self, value: u32) {
        self.extend_from_slice(&value.to_be_bytes());
    }
    fn write_u64(&mut self, value: u64) {
        self.extend_from_slice(&value.to_be_bytes());
    }
    fn align(&mut self) {
        self.resize(self.len().next_multiple_of(4), 0);
    }
    fn tell(&self) -> u32 {
        self.len().try_into().unwrap()
    }
    fn pwrite_u32(&mut self, offset: usize, value: u32) {
        if self.len() < offset + 4 {
            self.resize(offset + 4, 0);
        }
        self[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
    }
    fn write_token(&mut self, token: FdtToken) {
        self.write_u32(token as u32);
    }
    fn write_string(&mut self, s: &str) -> u32 {
        let r = self.tell();
        self.extend_from_slice(s.as_bytes());
        self.push(0);
        r
    }
}

#[derive(Default)]
struct StringTable {
    map: HashMap<String, u32>,
    block: Vec<u8>,
}

impl StringTable {
    fn intern(&mut self, s: &str) -> u32 {
        let r = match self.map.entry(s.into()) {
            Entry::Occupied(entry) => return *entry.get(),
            Entry::Vacant(entry) => *entry.insert(self.block.write_string(s)),
        };
        // Also insert each suffix which wasn't already present.  `dtc` implements this
        // optimization, so it eases comparing DTB output if we do it too.
        for (i, _) in s.char_indices().skip(1) {
            match self.map.entry(s[i..].into()) {
                Entry::Occupied(_) => break,
                Entry::Vacant(entry) => *entry.insert(r + i as u32),
            };
        }
        r
    }
    fn serialize(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.block);
    }
}

trait FdtReader {
    fn read_u32(&mut self) -> Result<u32, DeserializeError>;
    fn read_token(&mut self) -> Result<FdtToken, DeserializeError>;
    fn read_bytes(&mut self, len: usize) -> Result<Vec<u8>, DeserializeError>;
    fn read_cstr(&mut self) -> Result<String, DeserializeError>;
    fn pread_cstr(&self, pos: usize) -> Result<String, DeserializeError>;
    fn realign_to(&mut self, outer: &[u8]) -> Result<(), DeserializeError>;
}

impl FdtReader for &[u8] {
    fn read_u32(&mut self) -> Result<u32, DeserializeError> {
        if let Some((first, rest)) = self.split_first_chunk::<4>() {
            *self = rest;
            Ok(u32::from_be_bytes(*first))
        } else {
            Err(DeserializeError::Truncated)
        }
    }

    fn read_token(&mut self) -> Result<FdtToken, DeserializeError> {
        FdtToken::from_u32(self.read_u32()?).ok_or("invalid FDT token".into())
    }

    fn read_bytes(&mut self, len: usize) -> Result<Vec<u8>, DeserializeError> {
        if let Some((first, rest)) = self.split_at_checked(len) {
            *self = rest;
            Ok(first.into())
        } else {
            Err(DeserializeError::Truncated)
        }
    }

    fn read_cstr(&mut self) -> Result<String, DeserializeError> {
        let cstr = core::ffi::CStr::from_bytes_until_nul(self)
            .map_err(|_| DeserializeError::from("string not terminated"))?;
        let s = cstr
            .to_str()
            .map_err(|_| DeserializeError::from("invalid UTF-8"))?;
        *self = &self[s.len() + 1..];
        Ok(s.into())
    }

    // TODO:  This could return a &str, but appeasing the borrow checker is a chore.
    fn pread_cstr(&self, pos: usize) -> Result<String, DeserializeError> {
        let Some(mut suffix) = self.get(pos..) else {
            return Err(DeserializeError::Truncated);
        };
        suffix.read_cstr()
    }

    fn realign_to(&mut self, outer: &[u8]) -> Result<(), DeserializeError> {
        let outer = outer.as_ptr_range();
        let outer = outer.start as usize..outer.end as usize;
        let inner = self.as_ptr_range();
        let inner = inner.start as usize..inner.end as usize;
        assert!(outer.contains(&inner.start));
        assert!(outer.contains(&inner.end) || outer.end == inner.end);
        let offset = inner.start - outer.start;
        let aligned = offset.next_multiple_of(4);
        self.read_bytes(aligned - offset)?;
        Ok(())
    }
}
