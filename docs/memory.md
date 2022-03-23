# New Mars: Memory layout and representation

This document describes the current (2022/01/13) state of the memory layout and noun representation for New Mars.

## Noun representation

Nouns are represented as machine words with the MSBs as tag bits.
By treating a 0 MSB as the tag for a direct atom, we can compute directly with direct atoms without masking the tag.

|------|-----------------------|
| MSBs | Noun                  |
| 0    | Direct Atom           |
| 10   | Cell Pointer          |
| 110  | Indirect Atom Pointer |
|------|-----------------------|

A direct atom is an atom which fits in a machine word, less one bit for the tag. It is stored directly.

An indirect atom is an atom which is too big to be a direct atom.
It is thus represented as a tagged pointer.
The memory referenced is at least 64-bit aligned.
The first 64 bits are the size of the atom in bytes.
This is followed by that number of bytes containing the atom in little-endian order.

A cell is represented as a tagged pointer.
The memory referenced is adjacent machine words.
The machine word at the pointer is the noun representation of the head (left side/first) of the cell.
The machine word directly following (higher in memory) is the noun representation of the tail (right side/second) of the cell.

During collection the memory for indirect atoms and cells may be rewritten to indicate a _forwarding pointer_.
This is discussed further in the "Memory Layout" section.

## Memory Layout

Computation proceeds in a memory arena, using two stacks growing towards each other from opposite ends.
The stack growing from lower memory to higher is termed the "west" stack and lower memory is directionally "west."
The stack growing from higher memory to lower is termed the "east" stack and, similarly, higher memory is directionally "east."

Computational frames are always pushed onto the opposing stack from the current frame.
Thus the two stacks logically form one single computational stack.
The stack on which the current frame is located is termed the "polarity".
Thus, the current stack frame may be said to have "west" or "east" polarity.

Two pointers are kept in registers.
The "frame pointer" is changed only when pushing a new stack frame.
When the current polarity is west, it points to the westernmost byte in the current frame.
Slots relative to the frame (for register saves or return addresses) are addressed by offsetting eastward.
When the current polarity is east, it points to the easternmost byte _west of_ the current frame.
Slots relative to the frame are in this case addressed by offsetting westward.
Note that we do not reverse which "end" of a machine word we address by the polarity.

The "stack pointer" is changed when pushing a new stack frame, and also to allocate memory.
When the current polarity is west, it points to the westernmost byte east of the current frame.
When the current polarity is east, it points to the westernmost byte *of* the current frame.

When pushing a new frame, the frame pointer is always set to the stack pointer of the *previous* frame to the current.
This achieves the polarity reversal.
The stack pointer is offset east of the frame pointer by the machine words necessary (for a west frame) or west of the frame pointer (for an east frame).
Note that the bytecode instruction for a push implicitly adds two machine words to the requested stack frame size.
This is because each frame saves the previous frame's stack and frame pointer as its first and second word slots,
respectively.
This provides both a return mechanism (restore stack and frame pointer from current frame, flip polarity), and
a polarity-swapping push (frame pointer is previous stack pointer, stack pointer offset from frame pointer, flip polarity.

Allocation is achieved by saving the stack pointer, moving it east, and returning the saved pointer (for a west frame) or by moving the stack pointer west and returning the result (for an east frame).

When popping a frame, the result register is examined to provide a collection root.
Pointers which fall within the current frame are recursively chased and copied to the parent frames allocation area, bumping the saved stack pointer as this progresses. This ensures that data allocated in this frame (or a child frame and then copied here) which is live from the returned result is not lost when the frame is popped.

The collector makes use of memory just beyond the returning frame as an ephemeral stack, containing noun words to
be copied and a destination to copy them to.
Copying a cell results in a new cell allocation on the parent memory, and pushing both constituent nouns onto the
stack with the head and tail of the cell as destinations.
Copying a direct atom is just a matter of writing it to the destination.
Copying an indirect atom involves an allocation, copying of the atom's memory, and writing the new allocation to the saved destination.

Frames to which a call may return should reserve the first frame slot (after the frame and stack pointers) for the return address.
The `lnk` instruction (see Bytecode Representation, below) will save the current program counter in this slot.
The `don` instruction will read the program counter from this register and jump to it.
_Note that this makes it the ***caller's*** responsibility to push a new frame for a non-tail call._

## Bytecode Representation

The virtual machine which runs our bytecode provides a layer of abstraction from some details of the memory layout.
Allocation is performed by bytecode operations and the stack discipline above is relied on to free unused memory.  
This allows us to simply consider registers and machine-word-sized frame slots as containing nouns.

The virtual machine for the bytecode has three registers: `sub`, for current subject noun, `res`, for the
result of running a formula, and `pc`, for the current position in the bytecode.
The contents of the PC register are opaque to bytecode programs and can only be stored to memory as part of an
`lnk` instruction or loaded from memory as part of a `don` instruction.

Note that Nock allows for an arbitrary noun to be read computed and then treated as a Nock formula.
Of course, if a noun is not a valid nock formula (e.g. `2` or `[14 0]`) this causes a crash.
But if it is a valid Nock formula, we may not have bytecode already generated for it.

New Mars looks forward to caching bytecode, but for now we will simply assume that it is generated each time a nock noun is evaluated as a formula.
In either case, there is need of some mechanism to transfer control between linear sequences of bytecode, invoking
a sequence from some point within another, and returning to that point when the invoked sequence completes.
Further, we need to support *tail calls* to permit recursion without linear memory usage for stale frames.

The `puh` instruction permits us to push a frame with a certain number of "slots".
In the underlying representation, this number will be increased by 2, in order to store the previous stack and frame
pointers.
However this detail is hidden from programs in the bytecode, for which slot 0 is the first slot after the
stack and frame pointers, slot one the second, and so on.

The `pop` instruction pops a frame, and performs the limited copying collection described above, using the contents of the `res` register as a root.

The `lnk` instruction saves the current `pc` register into the `0` slot of the frame, and then jumps to the bytecode for the noun in the `res` register.
(This may involve generating the bytecode, or it may be cached.)
By doing so, it sets up the bytecode for the formula in `res` to return to the following instruction, with its result in `res`.

The `don` instruction accomplishes this return.
It simply loads the `0` slot of the frame into the PC register.

The `lnt` instruction jumps to bytecode for a formula, as in `lnk`, but *does not save the pc*.
It is intended for tail calls where the result of the called formula is the result of the current formula.

Thus any the bytecode for any formula compiled either as the initial computation or as the result of being invoked from the `res` register by `lnk` or `lnt` ends with either `lnt` or `don`.

The bytecode instructions are as follows:

| Name     | Opcode       | Description
|----------|--------------|------------
| `nop`    | 0x0          | Do nothing
| `axe[x]` | 0xC1         | Look up an axis in the subject, place in the result
| `cel[x]` | 0x82         | Allocate a cell, use the given frame slot as the head and the `res` register as the tail, writing the allocated cell to `res`.
| `con[x]` | 0xC3         | Place a constant in the `res` register
| `cl?`    | 0x4          | Write `0` to `res` if `res` is a cell, `1` otherwise
| `inc`    | 0x5          | Increment `res` if `res` is an atom. Crash otherwise
| `eq?[x]` | 0x86         | Write `0` to `res` if the nouns in the given frame slot and `res` are structurally equal
| `puh[x]` | 0x87         | Push a frame with the given number of slots
| `pop`    | 0x8          | Pop a frame
| `put[x]` | 0x89         | Write the noun in `res` to the given frame slot
| `get[x]` | 0x8A         | Read the noun in the given frame slot to `res`
| `sav[x]` | 0x8B         | Write the noun in `sub` to the given frame slot
| `reo[x]` | 0x8C         | Read the noun in the given frame slot to `sub
| `sub[x]` | 0xD          | Set the `sub` register equal to the `res` register (1)
| `ext[x]` | 0xE          | Allocate a cell, use the `res` register as the head and the `sub` register as the tail, writing the allocated cell to `sub`.
| `edt[x]` | 0xCF         | Overwrite the given axis in the noun in `sub` with the noun in `res`, placing the result in `res`.
| `br1[x]` | 0x90 or 0xD0 | Skip over the given number of following instructions if `res` is 1, skip none if res is 0. Crash otherwise. (2)
| `bru[x]` | 0x91 or 0xD1 | Skip over the given number of following instructions. (2)
| `lnt`    | 0x12         | Compile or read cache of bytecode for formula noun in `res` and jump
| `lnk`    | 0x13         | Compile or read cache of bytecode for formula noun in `res`, save pc to frame slot `0`, and jump
| `don`    | 0x14         | Restore pc from frame slot 0
| `spy`    | 0x15         | Ask runtime to query its namespace with cell from `res` and place result in `res`.

### Compiling formulae to bytecode

The bytecode compiler uses a "caller-save" convention for registers, including the program counter.
This means that we must track whether it is necessary to save the program counter (tail position or not) and the subject register.
Thus, the abstract procedure for compiling Nock formulae to bytecode is specified with two additional bits of input.

One bit, when 0, specifies that the subject need not be saved prior to modification.
This is true either in tail position (since when the code is finished its subject will no longer be needed) or when the code will be immediately followed by a `sub` or `reo` instruction which will overwrite the subject.
When this bit is 1, it specifies that the subject should be saved prior to modification, and thus any overwriting of the subject should be preceded by a `sav` instruction and followed eventually by a `reo` instruction.

The other bit, when 0, specifies that this code is being generated for "tail position", meaning that its result will be the final result either of an external invocation or a dynamic call.
Code is tail position when it is the code generated for the outermost formula of a compilation, or it is a recursive invocation to compile a subformula with no additional instructions appended after the recursive invocation.

For instance, when compiling Nock 7, if the Nock 7 is in tail position, then firstly, no frame is pushed as no registers need to be saved.
The first subformula is compiled with bits 0,1 specifying that the subject may be mangled.
A `sub` instruction is appended, and then the second subformula is compiled with bits 0,0, specifying both that the subject may be mangled and that the second subformula is in tail position.
The result of this subcomputation will be the result of the outer computation, with no additional steps.

When compiling Nock 2 or Nock 9 in tail position, we take care that any frames pushed are popped (necessary for 2 as we must save the first subformula's result while we compute the second), properly set the `sub` and `res` registers, and then generate an `lnt` instruction which ends the code for our current outer formula.
Thus the result of our outer formula will be the result of whichever formula was invoked by the Nock 2 or Nock 9.

When compiling Nock 2 or Nock 9 in a non-tail position, we must push a frame to ensure that frame slot 0 is available for the `lnk` instruction to save the return in, and pop this frame only after control returns from the invoked formula.
If it is necessary that the subject should not be mangled, it is the caller's responsibility (i.e. the responsibility of the code generated for Nock 2 or Nock 9) to save the subject in this frame and restore it after control returns.

The following is the complete definition of the abstract function for compiling Nock formulae to this bytecode.
A `;` represents concatenation of bytecode sequences.
Note that when this function is applied to produce bytecode for an external computation of a Nock formula, or a formula invoked by Nock 2 or Nock 9, it is invoked with both extra bits true/unset/0.

The `len[]` function gives the offset which when provided to br1 or bru would skip over the input instruction sequence.

| Nock           | S | T | bc[s t n]
|----------------|---|---|---------
| `[[b c] d]`    | * | 0 | `puh[1]; bc[1 1 [b c]]; put[0]; bc[s 1 d]; cel[0]; pop; don`
| `[[b c] d]`    | * | 1 | `puh[1]; bc[1 1 [b c]]; put[0]; bc[s 1 d]; cel[0]; pop`
| `[0 b]`        | * | 0 | `axe[b]; don`
| `[0 b]`        | * | 1 | `axe[b]`
| `[1 c]`        | * | 0 | `con[c]; don`
| `[1 c]`        | * | 1 | `con[c]`
| `[2 b c]`      | 0 | 0 | `puh[1]; bc[1 1 [c]]; put[0]; bc[0 1 b]; sub; get[0]; pop; lnt` COMMENT: does this require we treat sub as a GC root? probably?
| `[2 b c]`      | 0 | 1 | `puh[2]; bc[1 1 [c]]; put[1]; bc[0 1 b]; sub; get[0]; lnk; pop`
| `[2 b c]`      | 1 | 1 | `puh[3]; sav[2]; bc[0 1 c]; put[1]; reo[2]; bc[0 1 b]; sub; get[1]; lnk; reo[2]; pop`
| `[3 b]`        | 0 | 0 | `bc[0 1 b]; cl?; don`
| `[3 b]`        | * | 1 | `bc[s 1 b]; cl?`
| `[4 b]`        | 0 | 0 | `bc[0 1 b]; inc; don`
| `[4 b]`        | * | 1 | `bc[s 1 b]; inc; don`
| `[5 b c]`      | 0 | 0 | `puh[1]; bc[1 1 b]; put[0]; bc[0 1 c]; eq?[0]; pop; don`
| `[5 b c]`      | * | 1 | `puh[1]; bc[1 1 b]; put[0]; bc[s 1 c]; eq?[0]; pop`
| `[6 b c d]     | 0 | 0 | `bc[1 1 b]; br1[len[bc[0 0 c]]]; bc[0 0 c]; bc[0 0 d]`
| `[6 b c d]     | * | 1 | `bc[1 1 b]; br1[len[bc[s 1 c]; bru[len[bc[s 1 d]]]]]; bc[s 1 c]; bru[len[bc[s 1 d]]]; bc[s 1 d]`
| `[7 b c]`      | 0 | * | `bc[0 1 b]; sub; bc[0 t c]`
| `[7 b c]`      | 1 | 1 | `puh[1]; sav[0]; bc[0 1 b]; sub; bc[0 1 c]; reo[0]; pop`
| `[8 b c]`      | 0 | * | `bc[1 1 b]; ext; bc[0 t c]`
| `[8 b c]`      | 1 | 1 | `puh[1]; sav[0]; bc[0 1 b]; reo[0]; ext; bc[0 1 c]; reo[0]; pop`
| `[9 b c]`      | 0 | 0 | `bc[0 1 c]; sub; axe[b]; lnt`
| `[9 b c]`      | 0 | 1 | `puh[1]; bc[0 1 c]; sub; axe[b]; lnk; pop`
| `[9 b c]`      | 1 | 1 | `puh[2]; sav[1]; bc[0 1 c]; sub; axe[b]; lnk; reo[1]; pop`
| `[10 [b c] d]` | 0 | 0 | `puh[1]; bc[1 1 d]; put[0]; bc[0 1 c]; reo[0]; edt[b]; pop; don`
| `[10 [b c] d]` | 0 | 1 | `puh[1]; bc[1 1 d]; put[0]; bc[0 1 c]; reo[0]; edt[b]; pop`
| `[10 [b c] d]` | 1 | 1 | `puh[2]; sav[1]; bc[0 1 d]; put[0]; reo[1]; bc[0 1 c]; reo[0]; edt[b]; reo[1]; pop`
| `[11 [b c] d]` | * | * | `bc[1 1 c]; hns[b]; hnd; bc[s t d]`
| `[11 b c]`     | * | * | `hns[b]; bc[s t c]`
| `[12 b c]`     | 0 | 0 | `puh[1]; bc[1 1 b]; put[0]; bc[s 1 c]; cel[0]; pop; spy; don`
| `[12 b c]`     | * | 1 | `puh[1]; bc[1 1 b]; put[0]; bc[s 1 c]; cel[0]; pop; spy`


