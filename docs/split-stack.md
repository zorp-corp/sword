# Ares: Split stack memory layout

This document describes the split stack memory layout for Ares, first suggested
by ~master-morzod and later implemented in July 2023.

## Memory Layout

Computation proceeds in a memory arena, using two stacks growing towards each
other from opposite ends. Stack frames are "split" between the two stacks into
what one might typically call a stack and heap, with each successive frame
alternating which end of the memory it uses as a stack and which end it uses as
a heap. This looks a lot like Vere's road system, though while Vere utilizes
roads for virtualization, Ares utilizes this system for tail call optimization.
That being said, we do not presume any knowledge of Vere's road system.

Lower memory is directionally "west", and higher memory is directionally "east".
A frame whose "stack side" (to be defined shortly) is lower in memory than their
"heap side" is called a "west frame". On the other hand, a frame whose stack
side is higher in memory than their heap side is called an "east frame".

Computational frames are always pushed in an alternating west/east fashion (with
the first frame always being a west frame).
Thus the two stacks logically form one single computational stack.
The stack on which the current frame is located is termed the "polarity".
Thus, the current stack frame may be said to have "west" or "east" polarity.

Every stack frame undergoes two sequential phases: the execution phase, and the
cleanup phase. Ordinary operation occurs in the execution phase, while the
cleanup phase occurs if and only if the stack frame is the current frame and we
are preparing to pop it. The memory layout changes depending on which phase the
stack frame in. To keep things simple, in what follows we only describe the
execution phase, and have a separate section for the cleanup phase.

### Execution phase

Three pointers are kept in registers - the frame pointer, stack pointer, and
allocation pointer. They are arranged for west and east frames as follows:

West frame (execution phase):
|----aXXXFLLSoooAYYYs----|
East frame (execution phase):
|----sYYYAoooSLLFXXXa----|

key:
- senior memory
A current allocation pointer
a previous allocation pointer
F current frame pointer
f previous frame pointer
S current stack pointer
s previous stack pointer
X Current memory slots
L Current lightweight stack
Y Current allocations
o free memory

The "stack side" for a west frame is the memory eastward of the previous
allocation pointer (inclusive) and west of the current stack pointer
(non-inclusive). The "heap side" for a west frame is the memory eastward of the
allocation pointer (inclusive) and westward of the previous stack pointer
(non-inclusive).

Similarly, the stack side for an east frame is the memory westward of the
previous allocation pointer (non-inclusive) and eastward of the current stack
pointer (inclusive). The heap side for an east frame is the memory westward of
the current allocation pointer (non-inclusive) and eastward of the previous
stack pointer (inclusive).

The following sections describe how each of these pointers are utilized.

#### Frame pointer

The frame pointer is the only pointer of the three which never moves.

Each stack frame has a number of slots (each one machine word in length) in the
memory arena. There are always at least three slots. These three slots are
reserved to store the pointers for the previous frame. Further slots may be used
to store things like the return of a function or the Nock instruction being run in
that stack frame.

These slots are sometimes called "frame pointer relative slots", or FP-relative
slots. For a west stack frame, these slots are immediately west of the frame
pointer, with the first three slots west of the frame pointer being reserved to
store the pointers of the previous frame. These slots extend to the previous
allocation pointer.

We emphasize that the frame pointer does _not_ mark a boundary for the stack
frame. For a west frame, the pointers are always ordered as previous allocation
pointer is less than the current frame pointer is less than or equal to the
current stack pointer. For an east frame, substitute "less" with "greater". The
reason for this is because we can test if the lightweight stack (to be defined
in the following section) is empty when the frame pointer equals the stack
pointer. Otherwise, we would have to store the original location of the stack
pointer in one of the FP-relative slots to determine when the lightweight stack
is empty.

#### Stack pointer

The stack pointer is always initialized to be equal to the frame pointer. The
stack pointer is for marking the top of a "lightweight stack", which is a stack
entirely contained within the current stack frame that is primarily used for
things such as noun traversal. It is utilized for several things, including jam,
cue, mug, unifying equality, noun copying, and executing subformulas of a Nock
formula in the current stack frame.

For a west frame, pushing onto the lightweight stack moves the stack pointer
eastward, while popping moves it westward. For an east frame, pushing onto the
lightweight stack moves the stack pointer westward, while popping moves it
eastward.

#### Allocation pointer

The allocation pointer is always initialized to be equal to the previous frame's
stack pointer. This pointer is used to represent the extent of the "heap" for the current
stack frame. The remaining free memory at a moment in time for the virtual
machine is always precisely the region between the current stack frame's stack
pointer and allocation pointer (at least during the execution phase). Thus the
term "split stack".

Every time an allocation is made or reclaimed, the allocation
pointer moves. For a west frame, an allocation moves the allocation pointer
westward. For an east frame, an allocation moves the allocation pointer eastward.

### Pushing a stack frame

When a new stack frame is pushed, the following occurs.

First, a number of slots is specified (0 or more). These are frame-pointer
relative slots beyond the 3 necessary to store the previous frame's pointers.

The new frame pointer is then set to the previous frame's allocation pointer, offset
east (for a west frame) or west (for an east frame) by 3 plus the specified
number of slots in machine words.

The new stack pointer is set to be equal to the new frame pointer.

The new allocation pointer is set to be equal to the previous frame's stack pointer.

The relative position of the stack and allocation pointers determines whether a
stack frame is called west or east. If the stack pointer is west of the 
allocation pointer, the polarity is west. If the stack pointer is east of the
allocation pointer, the polarity is east. Thus, by the process outlined about,
we see that when the current stack frame has a west polarity, the following one
will always have an east polarity, and vice versa.

### Popping a stack frame

Popping the current stack frame in the split stack memory model is the trickiest
part. Naively, this would be as simple as restoring the three pointers of the
previous frame stored in FP-relative slots of the current frame. Unfortunately
it is not so easy.

When popping a stack frame, it is usually the case that we want to preserve some
of the allocated memory. Any allocations in the current stack frame will be gone
once we've finished popping, and so we need to copy the ones we wish to keep
into the allocation arena (heap side) of the parent frame. Of course, doing so
will immediately start overwriting the FP-relative slots of the current frame.
Thus we enter the cleanup phase.

#### Cleanup phase

The cleanup phase is the second phase a stack frame undergoes, and occurs only
when it is the current (youngest) stack frame and is preparing to be popped. In
order to sidestep the problem that copying allocations in the current frame's
allocation arena into the previous frame's will overwrite the FP-relative slots,
we copy the slots reserved for the previous frame's pointers into the free
memory adjacent to the current allocation arena. Furthermore, we also move the
lightweight stack (used to e.g. traverse nouns being copied) adjacent to that.
After the previous frame's pointers have been copied to their new location and
the stack pointer has moved, but before any copying of allocations has begun,
the memory layout looks as follows:

West frame (cleanup phase):
|----aXXXFoooSXXXAYYYs---|
East frame (cleanup phase):
|----sYYYAXXXSoooFXXXa---|

key:
- senior memory
A current allocation pointer
a previous allocation pointer
F current frame pointer
f previous frame pointer
S current stack pointer
s previous stack pointer
X Current memory slots
L Current lightweight stack
Y Current allocations
o free memory

Thus, for a west frame, the "west-oriented" lighweight stack moves and becomes
an "east-oriented" lightweight stack.

While this may be useful context for the programmer to have, Ares provides a unified interface
for working with the previous frame's pointers and lightweight stack such that
the programmer does not need to know whether the frame is in the execution or
cleanup phase. E.g. you still use `stack_push()` to push items onto the
lightweight stack regardless of which phase the current stack frame is in.

#### Copying

After the frame has moved into the cleanup phase, copying from the current
allocation arena to the previous one can begin.

When popping a frame, the result register is examined to provide a collection
root. Pointers which fall within the current frame's allocation arena are
recursively chased and copied to the parent frame's allocation arena. This is
facilitated using the lightweight stack - for example, when a cell is found, its
head and tail are pushed onto the lightweight stack. Then when we pop, say, the
head, we check to see if its located within the current stack frame or a senior
one. If it is the current stack frame we copy it to the previous stack frame,
otherwise it is left alone.

This ensures that data allocated in this frame which is live from the returned
result is not lost when the frame is popped.

Copying a cell results in a new cell allocation to the parent frame's allocation
arena, and pushing both constituent nouns onto the lightweight stack with the
head and tail of cell as destinations.

Copying a direct atom consists of simply writing it to the destination.

Copying an indirect atom means allocating memory for it, copying the atom's
memory, and writing the new allocation to the saved destination.
