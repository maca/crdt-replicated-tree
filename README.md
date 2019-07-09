# CRDT Replicated Tree

Example app: https://github.com/maca/crdt-text-editor

Conflict-free Replicate Data Types are data structures that can be
replicated across many network nodes and edited independently, making
them useful for applications where state has to be syncronized
without manual conflict resolution, a cannonical example is a
collaborative text editor.

For this Replicated Tree, children of each branch are represented as an
RGA.

As with an RGA only two operations are permited: **add** a node after
a position, and **delete** a node at a position, although deleting does
not actually removes the node from the list but marks it a `Tombstone`,
as other nodes can always be inserted after it.

Because each Replicated Tree must have a unique id across the network,
it is recommended for this id to be assigned by a coordinating
server that keeps track of all replicas.
