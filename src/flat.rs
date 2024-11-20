//! Facilities for emitting the Flattened Device Tree (FDT) binary format,
//! also known as a Devicetree Blob (DTB).

use crate::Node;
use std::collections::HashMap;

// TODO: /memreserve/ not supported

pub fn serialize(root: &Node) -> Vec<u8> {
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
    const FDT_HEADER_SIZE: u32 = 40;
    out.write_u32(0xd00dfeed);
    out.write_u32(0); // totalsize not yet known
    out.write_u32(0); // off_dt_struct not yet known
    out.write_u32(0); // off_dt_strings not yet known
    out.write_u32(FDT_HEADER_SIZE);
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
    serialize_inner(&mut out, &mut strings, root);
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

#[derive(Copy, Clone)]
enum FdtToken {
    BeginNode = 1,
    EndNode = 2,
    Prop = 3,
    Nop = 4,
    End = 9,
}

fn serialize_inner(out: &mut Vec<u8>, strings: &mut StringTable, root: &Node) {
    out.write_token(FdtToken::BeginNode);
    out.write_string(&root.name);
    out.align();
    for prop in &root.properties {
        out.write_token(FdtToken::Prop);
        out.write_u32(prop.value.len().try_into().unwrap());
        out.write_u32(strings.intern(&prop.name));
        out.extend_from_slice(&prop.value);
        out.align();
    }
    for node in &root.children {
        serialize_inner(out, strings, &node);
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
        let entry = self.map.entry(s.to_owned());
        *entry.or_insert_with(|| self.block.write_string(s))
    }
    fn serialize(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.block);
    }
}
