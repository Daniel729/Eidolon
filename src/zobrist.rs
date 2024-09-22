use const_random::const_random;
use seq_macro::seq;

// Randomly generated constant numbers used for zobrist hashing

pub const BLACK_TO_MOVE: u64 = const_random!(u64);

pub const EMPTY_PLACE: u64 = const_random!(u64);

pub const STATE: [u64; 256] = seq!(i in 0..256 {[#(const_random!(u64),)*]});

pub const PIECE: [[u64; 12]; 64] = seq!(i in 0..64 {[
    #(seq!(k in 0..12 {[
        #(const_random!(u64),)*
    ]}),)*
]});
