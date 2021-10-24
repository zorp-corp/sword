# Noun Representation

A noun is represented as a word, which may be a pointer or reference to other memory.

Semantically, nouns are either atoms or cells, where a cell is an ordered pair of nouns.

A noun-word is a word sized to the machine architecture (e.g. 64 bits on x86_64, 32 bits on ARMv)
which, possibly together with other memory to which it points, represents a noun.

A noun-word with a 1 as the least significant bit is a direct atom. The atom value can be obtained as a machine word
by right-shifting the representational word by 1 bit.

A noun-word with 10 as the least significant bits is an indirect atom. By masking the 2 least significant bits,
this noun can be converted to a (4 byte aligned) pointer, which points to an length-tagged array.
That is, the machine word pointed to by the pointer specifies the length of the array in bytes,
and is immediately followed in memory by a byte array containing the full indirect atom.

A noun-word with 100 as the least significant bits is a pointer to a cell. By masking the 2 least significant bits,
this noun can be converted to a (8 byte aligned) pointer, which points to a cell in memory.

A noun-word with 1000 as the least significant bits is a hash reference. This is a (machine bits - 4) unsigned
integer hash of a noun, used to reference the noun in the hash memory arena.

|`MSB                                                          LSB`|                 |
|`|--------------------------------------------------------------|`|                 |
|------------------------------------------------------------------|-----------------|
|`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX1`| Direct atom     |
|`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX10`| Indirect atom   |
|`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX100`| Pointer to cell |
|`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX1000`| Hash reference  |

## Indirect atoms

An indirect atom is a pointer to a machine word, which word is directly adjacent to an array of bytes.
The machine pointer is obtained by masking the least 2 significant bits of the noun-word.
The machine word specifies the size of the atom in bytes, and the byte array stores the atom in little-endian order.

## Cells

A cell is a pointer to a noun-word, which is adjacent to another noun-word.
The machine pointer is obtained by masking the last 3 bits of the cell noun-word.
The pointed-to noun-word is the head of the cell, and the immediately adjacent noun-word is the tail.

## Hash reference
New Mars maintains a HAMT which can store nouns which are large(1) and/or have metadata such as bytecode or a jet reference. An entry in the hashtable consists of metadata and a noun-word, which, if it is a pointer, references memory mapped in a memory arena separate from the stack allocator. Each noun in the hashtable is stored in contiguous memory,
unless it references another whole noun in the hash table, which it may do by a hash reference.

Hash references may be replaced in the stack arena by other noun-words on lookup, and then operated on.
This can result in noun-words which point into a memory arena for a noun at another location than the top of the
noun tree. This is permissible on the stack, but not permissible in the HAMT. Therefore,
when ejecting a noun from the stack arena to the hashtable, such references must result in the referenced
subnouns being copied to their own HAMT entries and replaced with a hash reference.

***TODO*** describe implementation and details of HAMT and stack arena

