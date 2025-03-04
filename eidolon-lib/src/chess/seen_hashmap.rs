const SIZE_HASHMAP: usize = 1 << 11;
const BITS_POS: usize = 4;
const POS_MASK: u32 = (1 << BITS_POS) - 1;

/// Custom hashmap which stores the times a position was seen in the lower 4 bits,
/// the position's hash's lower 28 bits in the upper 28 bits of the value,
/// and uses the upper 32 bits of the hash for indexing
#[derive(Debug, Clone)]
pub struct SeenHashMap([u32; SIZE_HASHMAP]);

impl SeenHashMap {
    pub fn new() -> Self {
        SeenHashMap([0; SIZE_HASHMAP])
    }

    pub fn get(&self, hash: u64) -> u8 {
        let lower_hash = (hash as u32) << BITS_POS;
        let upper_hash = (hash >> 32) as usize;

        let mut index = upper_hash & (SIZE_HASHMAP - 1);

        loop {
            let val = self.0[index];

            let masked_val = val & !POS_MASK;

            if masked_val == lower_hash {
                return (val & POS_MASK) as u8;
            }

            index += 1;
            index &= SIZE_HASHMAP - 1;
        }
    }

    pub fn increase(&mut self, hash: u64) {
        let lower_hash = (hash as u32) << BITS_POS;
        let upper_hash = (hash >> 32) as usize;

        let mut index = upper_hash & (SIZE_HASHMAP - 1);

        loop {
            let val = &mut self.0[index];

            let seen = *val & POS_MASK;

            // This hashmap might fail occasionally because the value here
            // might have been set previously, and then deleted
            // which could mean that the same position is store twice in the hashmap
            // but I am pretty sure the chance is insignificant
            if seen == 0 {
                *val = lower_hash | 1;
                return;
            }

            let masked_val = *val & !POS_MASK;

            if masked_val == lower_hash {
                *val += 1;
                return;
            }

            index += 1;
            index &= SIZE_HASHMAP - 1;
        }
    }

    pub fn decrease(&mut self, hash: u64) {
        let lower_hash = (hash as u32) << BITS_POS;
        let upper_hash = (hash >> 32) as usize;

        let mut index = upper_hash & (SIZE_HASHMAP - 1);

        loop {
            let val = &mut self.0[index];

            let masked_val = *val & !POS_MASK;

            if masked_val == lower_hash {
                *val -= 1;

                return;
            }

            index += 1;
            index &= SIZE_HASHMAP - 1;
        }
    }
}
