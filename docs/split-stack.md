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
That being said, we do not presume any knowledge of Vere's road system in this
document.

Lower memory is directionally "west", and higher memory is directionally "east".
A frame whose "stack side" (to be defined shortly) is lower in memory than their
"heap side" is called a "west frame". On the other hand, a frame whose stack
side is higher in memory than their heap side is called an "east frame".

Computational frames are always pushed in an alternating west/east fashion (with
the first frame always being a west frame).
Thus the two stacks logically form one single computational stack.
The stack on which the stack side of the current frame is located is termed the "polarity".
Thus, the current stack frame may be said to have "west" or "east" polarity.

Every stack frame undergoes two sequential phases: the execution phase and the
cleanup phase. Ordinary operation occurs in the execution phase, while the
cleanup phase occurs if and only if the stack frame is the current frame and we
are preparing to pop it. Put another way, at any given time, either:

 - All stack frames are in the execution phase, or
 - The top stack frame is in the cleanup phase, and all others are in the
   execution phase.

The memory layout changes depending on which phase the
stack frame in. To keep things simple, in what follows we only describe the
execution phase, and have a separate section for the cleanup phase.

### Execution phase

Three pointers are kept in registers - the frame pointer, stack pointer, and
allocation pointer. I strongly recommend looking at the diagram split-stack.svg
in this folder - a simple text diagram does not convey this information well.

They are arranged for west and east frames as follows.

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

#### West and East frames

For a west frame with `n` slots, the pointers are located in memory as follows:

 - `frame_ptr <= stack_ptr < alloc_ptr`
 - `prev_alloc_ptr = frame_ptr - 3 - n`
 - `*(frame_ptr - 1) = prev_frame_ptr`
 - `*(frame_ptr - 2) = prev_stack_ptr`
 - `*(frame_ptr - 3) = prev_alloc_ptr`
 - The initial position of `stack_ptr` is `frame_ptr`
 - `frame_ptr` is frozen, `stack_ptr` and `alloc_ptr` can move.
 - Pushing the lightweight stack moves `stack_ptr` eastward.
 - Allocating moves `alloc_ptr` westward.
 
 For an east frame with `n` slots, the pointers are located in memory as follows:
 - `alloc_ptr < stack_ptr <= frame_ptr`
 - `prev_alloc_ptr = frame_ptr + 3 + n`
 - `*frame_ptr = prev_frame_ptr`
 - `*(frame_ptr + 1) = prev_stack_ptr`
 - `*(frame_ptr + 2) = prev_alloc_ptr`
 - The initial position of the `stack_ptr` is `frame_ptr`
 - `frame_ptr` is frozen, `stack_ptr` and `alloc_ptr` can move.
 - Pushing the lightweight stack moves `stack_ptr` westward.
 - Allocating moves `alloc_ptr` eastward.
 
The offset difference of 1 between west and east frames for `frame_ptr`-relative slots
is due to how memory is laid out - for a newly pushed west frame, the memory
beginning at `frame_ptr` is unallocated, while for a newly pushed east frame, the
memory beginning at `frame_ptr` is allocated (and is the value of
`prev_frame_ptr`). Another way to think about this is that the beginning of a
machine word is always its westmost bit, so there's a fundamental asymmetry in
how memory is laid out for west and east frames that manifests as the offset difference.
 
That being said, the interface is set up so the programmer does not need to
think about this. They never need to be aware of or need to check whether the current
stack frame is east or west - they will call the same functions either way. See
the [interface](#interface) section for more details.
 
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

We emphasize that, unlike many common stack schemas, the frame pointer does
_not_ mark a boundary for the stack frame. For a west frame, the memory west of
`frame_ptr` are slots in which other pointers are stored. If the lightweight
stack is non-empty, then the memory east of `frame_ptr` contains elements of the
lightweight stack.

The ultimate reason for this is that it improves the ergonomics of the pointer
interface. A deeper explanation is given [at the end](#frame-boundary), since
you need to understand what all the pointers are for and what the cleanup phase
for this choice to make sense.

#### Stack pointer

The stack pointer is always initialized to be equal to the frame pointer when a
stack frame is pushed. The stack pointer is for marking the top of a
"lightweight stack", which is a stack entirely contained within the current
stack frame. It is utilized for several things, including jam, cue, mug,
unifying equality, noun copying, and executing subformulas of a Nock formula in
the current stack frame. The primary reason for the existence of the lightweight
stack is to enable completely reliable tail calls. Indirect calls, which may
need arbitrarily many stack slots, utilize the lightweight stack for this
purpose.

For a west frame, pushing onto the lightweight stack moves the stack pointer
eastward, while popping moves it westward. For an east frame, pushing onto the
lightweight stack moves the stack pointer westward, while popping moves it
eastward.

#### Allocation pointer

The allocation pointer is always initialized to be equal to the previous frame's
stack pointer when a stack frame is pushed. This pointer is used to represent
the extent of the "heap" for the current stack frame. The remaining free memory
at a moment in time for the virtual machine is always precisely the region
between the current stack frame's stack pointer and allocation pointer (at least
during the execution phase). Thus the term "split stack".

Every time an allocation is made or reclaimed, the allocation
pointer moves. For a west frame, an allocation moves the allocation pointer
westward. For an east frame, an allocation moves the allocation pointer eastward.

### Pushing a stack frame

When a new stack frame is pushed, the following occurs.

First, a number of slots is specified (0 or more). These are frame-pointer
relative slots beyond the 3 necessary to store the parent frame's reserved pointers.

The new frame pointer is then set to the parent frame's allocation pointer, offset
east (for a west frame) or west (for an east frame) by 3 plus the specified
number of slots in machine words.

The new stack pointer is set to be equal to the new frame pointer.

The new allocation pointer is set to be equal to the parent frame's stack pointer.

The relative position of the frame and allocation pointers determines whether a
stack frame is called west or east. If the frame pointer is west of the 
allocation pointer, the polarity is west. If the frame pointer is east of the
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

### Cleanup phase

The cleanup phase is the second phase a stack frame undergoes, and occurs only
when it is the current (youngest) stack frame and is preparing to be popped. In
order to sidestep the problem that copying allocations in the current frame's
allocation arena into the previous frame's will overwrite the FP-relative slots,
we copy the slots reserved for the previous frame's pointers into the free
memory adjacent to the current allocation arena. Furthermore, we also move the
lightweight stack (used to traverse nouns being copied) adjacent to that. After
the previous frame's pointers have been copied to their new location and the
stack pointer has moved, but before any copying of allocations has begun, the
memory layout looks as follows. As before, I strongly recommend looking at the
diagram split-stack.svg in this folder.

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
cleanup phase - see [interface](#interface).

#### West and East frames

For a west frame, the pointers are located in memory as follows:

 - `frame_ptr < stack_ptr < alloc_ptr`
 - `*(alloc_ptr - 1) = prev_frame_ptr`
 - `*(alloc_ptr - 2) = prev_stack_ptr`
 - `*(alloc_ptr - 3) = prev_alloc_ptr`
 - The initial position of the `stack_ptr` is `alloc_ptr - 4`.
 - `frame_ptr` and `alloc_ptr` are frozen, `stack_ptr` can move.
 - Pushing the lightweight stack moves `stack_ptr` westward.

 For an east frame, the pointers are located in memory as follows:
 - `alloc_ptr < stack_ptr > frame_ptr`
 - `*alloc_ptr = prev_frame_ptr`
 - `*(alloc_ptr + 1) = prev_stack_ptr`
 - `*(alloc_ptr + 2) = prev_alloc_ptr`
 - The initial position of the `stack_ptr` is `alloc_ptr + 3`.
 - `frame_ptr` and `alloc_ptr` are frozen, `stack_ptr` can move.
 - Pushing the lightweight stack moves `stack_ptr` eastward.

#### Copying

After the frame has moved into the cleanup phase, copying from the current
allocation arena to the previous one can begin.

When popping a frame, the result register is examined to provide a collection
root. Pointers which fall within the current frame's allocation arena are
recursively chased and copied to the parent frame's allocation arena. This is
facilitated using the lightweight stack - for example, when a cell is found, its
head and tail pointers are pushed onto the lightweight stack. Then when we pop, say, the
head, we check to see if its located within the current stack frame or a senior
one. If it is the current stack frame we copy it to the previous stack frame,
otherwise it is left alone.

This ensures that data allocated in this frame which is live from the returned
result is not lost when the frame is popped.

The reason that this works is due to the following invariant: junior memory may
only ever point to other memory within in the same stack frame, or more senior
memory. Senior memory may never point into junior memory.

Copying a cell results in a new cell allocation to the parent frame's allocation
arena, and pushing both constituent noun pointers onto the lightweight stack with the
head and tail of cell as destinations.

Copying a direct atom consists of simply writing it to the destination.

Copying an indirect atom means allocating memory for it, copying the atom's
memory, and writing the new allocation to the saved destination.

### Interface {#interface}

We provide here a brief description of the most commonly used methods of
`NockStack`. See the actual code for more comments and details.

#### Stack frames

We provide here a brief description of the key `NockStack` methods surrounding
stack frames: 

 - `frame_push(num_locals: usize)` - pushes a stack frame with `num_locals`
   extra slots.
 - `frame_pop()` - ends the cleanup phase and pops a frame.
 - `pre_copy()` - idempotent function that changes the current frame from
   execution to cleanup phase.
 - `prev_frame_pointer_pointer() -> *mut *mut u64` - pointer to where the previous frame pointer
   is saved in a frame.
 - `prev_stack_pointer_pointer() -> *mut *mut u64` - pointer to where the previous stack pointer
   is saved in a frame.
 - `prev_alloc_pointer_pointer() -> *mut *mut u64` - pointer to where the previous stack pointer
   is saved in a frame.
 - `is_west() -> bool` - `true` when the current stack frame is west.

#### Lightweight stack

We provide here a brief description of the key `NockStack` methods surrounding
the lightweight stack: 

 - `push<T>() -> *mut T` - push a `T` onto the lightweight stack.
 - `pop<T>()` - pop a `T` off the lightweight stack.
 - `top<T>() -> *mut T` - peek at a `T` on the top of the lightweight stack.
 - `stack_is_empty() -> bool` - true when lightweight stack is empty.

### Why `frame_ptr` does not mark a frame boundary {#frame-boundary}

Let's instead consider what would happen if `frame_ptr` for a west frame were
the westmost word of the frame - i.e. it marks a boundary for the stack frame,
and in fact is then equal to `prev_alloc_ptr`. This should mean that you now only need to reserve 2 slots
in each stack frame for the parent frame's pointers.

However, before we could test whether the lightweight stack was empty by
computing `stack_ptr == frame_ptr`. Now instead, we will need to save the
initial location of `stack_ptr` in a reserved slot. This is because it is not a
fixed offset from `frame_ptr` since the total number of slots in a given stack frame
can vary. So we end up with the same number of reserved slots in the execution
phase by putting the frame pointer at the boundary. If this is where it ended, this would arguably
have been a better choice.

However, recall that in the cleanup phase, the reserved pointers for the parent
frame are moved from being `frame_ptr`-relative slots to `alloc_ptr`-relative
slots. So `prev_alloc_ptr` can no longer be calculated relative to
`frame_ptr`, and instead needs to be saved in an `alloc_ptr`-relative slot.

In the cleanup phase, since the number of slots is fixed at 3 (as opposed to
varying as in the execution phase), the emptiness of the lightweight stack can
be determined by checking if its equal to one of the reserved slots (which one
depends on how you order them). So we no longer need to save the initial value
of `stack_ptr` in the cleanup phase.

Thus we would have two different sets of reserved pointers in the execution and
cleanup phases:
 - `prev_frame_ptr`, `prev_stack_ptr`, `init_stack_ptr` in the execution phase,
   and
 - `prev_frame_ptr`, `prev_stack_ptr`, `prev_alloc_ptr` in the cleanup phase.
 
 Having which set of pointers are being saved change between phases is more
 confusing for the programmer. Thus, we put the `frame_ptr` in the "middle", and
 make the same `frame_ptr == stack_ptr` check to see whether the lightweight
 stack is empty in both the execution and cleanup phases.
