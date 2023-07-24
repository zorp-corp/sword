## Ares: Noun representation

Nouns are represented as machine words with the MSBs as tag bits. By treating a
0 MSB as the tag for a direct atom, we can compute directly with direct atoms
without masking the tag.


| MSBs | Noun |
|------|-----------------------|
| 0    | Direct Atom           |
| 10   | Indirect Atom Pointer |
| 110  | Cell Pointer          |


A direct atom is an atom which fits in a machine word, less one bit for the tag.
It is stored directly.

An indirect atom is an atom which is too big to be a direct atom. It is thus
represented as a tagged pointer. The memory referenced is 64-bit aligned. The
first 64 bits are metadata, followed by the size in 64-bit words, then the
actual value of the atom, whose number of words is equal to the size in
little-endian order. The metadata field is primarily used for the mug. The first
three bits of the size are reserved for a possible forwarding pointer tag.

A cell is represented as a tagged pointer. The memory referenced is adjacent
machine words. The machine word at the pointer is metadata. The machine word
directly following (higher in memory) is the noun representation of the head,
followed by the noun representation of the tail. The metadata field is primarily
used for the mug.

During collection the memory for indirect atoms and cells may be rewritten to
indicate a _forwarding pointer_. When an indirect atom is copied from one stack
frame to another, its size is replaced with a forwarding pointer. When a cell is
copied from one stack frame to another, its head is replaced with a forwarding
pointer.

The tag for a forwarding pointer is 111.
