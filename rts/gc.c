#include <llvm-statepoint-tablegen.h>

#include "gc.h"
#include "object.h"
#include "rts.h"

#undef RAPID_GC_DEBUG_ENABLED
/*#define RAPID_GC_DEBUG_ENABLED*/

#ifdef __linux__
  #define STACKMAP __LLVM_StackMaps
#endif

#ifdef __APPLE__
  #define STACKMAP _LLVM_StackMaps
#endif
extern uint8_t STACKMAP[];

static statepoint_table_t *rapid_global_stackmap_table;

void *rapid_C_allocate(Idris_TSO *base, int32_t size) {
  return malloc(size);
}

static inline uint32_t aligned(uint32_t size) {
  return 8 * ((size + 7) / 8);
}

ObjPtr alloc_during_gc(Idris_TSO *base, uint32_t size) {
  uint8_t *p = base->nurseryNext;
  assert(((uint64_t)base->nurseryNext & 0x07) == 0);
  base->nurseryNext += aligned(size);
  assert((uint64_t)base->nurseryNext <= (uint64_t)base->nurseryEnd);
  return (ObjPtr)p;
}

ObjPtr copy(Idris_TSO *base, ObjPtr p) {
  ObjPtr new;
  uint32_t size;

  if (OBJ_IS_INLINE(p)) {
    return p;
  }

  if (OBJ_IS_FWD_INPLACE(p)) {
    uint64_t fwd_target = (p->hdr << 1);
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "-- in place fwd: %llx -> %llx\n", (uint64_t)p, fwd_target);
#endif
    return (ObjPtr)fwd_target;
  }

  // TODO: replace this with more efficient pointer tagging
  if ( !(((uint64_t)p >= (uint64_t)base->heap_aux) && ((uint64_t)p < (uint64_t)base->heap_aux_end)) ) {
    // object is not inside the old nursery, keep it where it is
    return p;
  }
  assert(((uint64_t)p >= (uint64_t)base->heap_aux) && ((uint64_t)p < (uint64_t)base->heap_aux_end) && "object is not inside old nursery");

  switch (OBJ_TYPE(p)) {
    case OBJ_TYPE_FWD_REF:
      return (ObjPtr)OBJ_GET_SLOT(p, 0);
    default:
      size = OBJ_TOTAL_SIZE(p);
      new = alloc_during_gc(base, size);
      memcpy(new, p, size);
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "-- object copied: %p -> %p (%u bytes)\n", (void *)p, (void *)new, size);
#endif
      if (size >= 16) {
        p->hdr = MAKE_HEADER(OBJ_TYPE_FWD_REF, 8);
        p->data = new;
      } else {
        p->hdr = 0x8000000000000000ull | (((uint64_t)new) >> 1);
      }
      return new;
  }
}

static void cheney(Idris_TSO *base) {
  uint8_t *scan = base->nurseryStart;

  while(scan < base->nurseryNext) {
    ObjPtr obj = (ObjPtr)scan;
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "================================== cheney obj start\n");
    dump_obj(obj);
    fprintf(stderr, "================================== cheney obj start\n");
#endif
    assert(!OBJ_IS_INLINE(obj));
    switch(OBJ_TYPE(obj)) {
      case OBJ_TYPE_CLOSURE:
        {
          int argCount = 0xffff & obj->hdr;
          for (int i = 0; i < argCount; ++i) {
            ObjPtr value = OBJ_GET_SLOT(obj, i + 1);
            ObjPtr argCopy = copy(base, value);
            OBJ_PUT_SLOT(obj, i + 1, argCopy);
          }
        }
        break;
      case OBJ_TYPE_CON_NO_ARGS:
        {
          int argCount = obj->hdr >> 40;
          for (int i = 0; i < argCount; ++i) {
            ObjPtr value = OBJ_GET_SLOT(obj, i);
            ObjPtr argCopy = copy(base, value);
            OBJ_PUT_SLOT(obj, i, argCopy);
          }
        }
        break;
      case OBJ_TYPE_IOREF:
        {
          ObjPtr argCopy = copy(base, OBJ_GET_SLOT(obj, 0));
          OBJ_PUT_SLOT(obj, 0, argCopy);
        }
        break;
      case OBJ_TYPE_IOARRAY:
        {
          int arraySize = OBJ_SIZE(obj);
          for (int i = 0; i < arraySize; ++i) {
            ObjPtr value = OBJ_GET_SLOT(obj, i);
            ObjPtr valCopy = copy(base, value);
            OBJ_PUT_SLOT(obj, i, valCopy);
          }
        }
        break;
      case OBJ_TYPE_FWD_REF:
        rapid_C_crash("illegal forward ref found");
        break;
      default:
        break;
    }
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "================================== cheney obj end\n");
    dump_obj(obj);
    fprintf(stderr, "================================== cheney obj end\n");
#endif
    scan += aligned(OBJ_TOTAL_SIZE(obj));
  }
}

void idris_rts_gc(Idris_TSO *base, uint8_t *sp) {
  uint8_t * orig_sp = sp;
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "GC called for %llu bytes, stack pointer: %p\n", base->heap_alloc, (void *)sp);
#endif

  uint64_t returnAddress = *((uint64_t *) sp);
  sp += sizeof(void *);
  frame_info_t *frame = lookup_return_address(rapid_global_stackmap_table, returnAddress);
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "GC begin return addr: 0x%016llx\n", returnAddress);
  fprintf(stderr, "GC begin frame info: 0x%016llx\n", (uint64_t)frame);
#endif

  uint64_t oldNurserySize = (uint64_t)base->nurseryEnd - (uint64_t)base->nurseryStart;
  uint64_t nextNurserySize = base->next_nursery_size;

  uint8_t *oldNursery = (uint8_t *)base->nurseryStart;
  uint8_t *newNursery = realloc(base->heap_aux, nextNurserySize);
  memset(newNursery, 0, nextNurserySize);
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "nursery size: %llu -> %llu\n", oldNurserySize, nextNurserySize);
  fprintf(stderr, "old nursery at: %p - %p\n", (void *)base->nurseryStart, (void *)base->nurseryEnd);
  fprintf(stderr, "new nursery at: %p\n", (void *)newNursery);
#endif

  base->nurseryStart = newNursery;
  base->nurseryNext = newNursery;
  base->nurseryEnd = (uint8_t *) ((uint64_t)newNursery + nextNurserySize);

  base->heap_aux = oldNursery;
  base->heap_aux_end = oldNursery + oldNurserySize;

  while (frame != NULL) {
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "=====\nstack walk for return addr: 0x%016llx\n", returnAddress);
    fprintf(stderr, "    frame info: 0x%016llx\n", (uint64_t)frame);
    fprintf(stderr, "    frame size: 0x%016llx\n", (uint64_t)frame->frameSize);
#endif

    // Replace each derived pointer with its offset relative to base pointer.
    // The actual relocation will happen after all base pointers have been
    // relocated.
    for (int i = frame->numBaseSlots; i < frame->numSlots; ++i) {
      pointer_slot_t ptrSlot = frame->slots[i];
      assert(ptrSlot.kind >= 0 && "must be a derived pointer");

      pointer_slot_t baseSlot = frame->slots[ptrSlot.kind];
      ObjPtr *baseStackSlot = (ObjPtr *)(sp + baseSlot.offset);
      ObjPtr *derivedStackSlot = (ObjPtr *)(sp + ptrSlot.offset);
      *derivedStackSlot = (void *)((uint64_t)(*derivedStackSlot) - (uint64_t)(*baseStackSlot));
    }

    for (int i = 0; i < frame->numBaseSlots; ++i) {
      pointer_slot_t ptrSlot = frame->slots[i];
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "    pointer slot %04d: kind=%02d offset + 0x%04x\n", i, ptrSlot.kind, ptrSlot.offset);
#endif
      if (ptrSlot.kind != -1) {
        assert((ptrSlot.kind < frame->numSlots) && "basse idx out of bounds");
        pointer_slot_t baseSlot = frame->slots[ptrSlot.kind];
        fprintf(stderr, "    pointer slot %04d: kind=%02d offset + 0x%04x = %p\n", i, ptrSlot.kind, ptrSlot.offset, *(void **)(sp + ptrSlot.offset));
        fprintf(stderr, "    -->base slot %04d: kind=%02d offset + 0x%04x = %p\n", ptrSlot.kind, baseSlot.kind, baseSlot.offset, *(void **)(sp + baseSlot.offset));
        ObjPtr *stackSlot = (ObjPtr *)(sp + baseSlot.offset);
        dump_obj(*stackSlot);
      }
      assert(ptrSlot.kind == -1);

      ObjPtr *stackSlot = (ObjPtr *)(sp + ptrSlot.offset);
#ifdef RAPID_GC_DEBUG_ENABLED
      dump_obj(*stackSlot);
#endif

      ObjPtr copied = copy(base, *stackSlot);
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "::copy: %p -> %p\n", (void *)*stackSlot, (void *)copied);
      dump_obj(copied);
#endif
      *stackSlot = copied;
    }

    // relocate all derived pointers
    for (int i = frame->numBaseSlots; i < frame->numSlots; ++i) {
      pointer_slot_t ptrSlot = frame->slots[i];
      assert(ptrSlot.kind >= 0 && "must be a derived pointer");

      pointer_slot_t baseSlot = frame->slots[ptrSlot.kind];
      ObjPtr *baseStackSlot = (ObjPtr *)(sp + baseSlot.offset);
      ObjPtr *derivedStackSlot = (ObjPtr *)(sp + ptrSlot.offset);
      *derivedStackSlot = (void *)((uint64_t)(*derivedStackSlot) + (uint64_t)(*baseStackSlot));
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "derived ptr relocated\n");
#endif
    }

    sp += frame->frameSize;
    returnAddress = *((uint64_t *) sp);
    sp += sizeof(void *);

    frame = lookup_return_address(rapid_global_stackmap_table, returnAddress);
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "\n  next ret: %p\n", (void *)returnAddress);
#endif
  }

  cheney(base);

  uint64_t nurseryUsed = (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart;
  if (nurseryUsed > (base->next_nursery_size >> 1)) {
    base->next_nursery_size = base->next_nursery_size * 2;
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "\nnursery will grow next GC: %llu -> %llu\n", nextNurserySize, base->next_nursery_size);
#endif
  }

  if (rapid_global_config->debug_heap_write_poison) {
    memset(base->heap_aux, 0x5f, oldNurserySize);
  }

#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "\n===============================================\n");
  fprintf(stderr, " GC FINISHED: %llu / %llu\n", (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart, nextNurserySize);
  fprintf(stderr, "===============================================\n");
  fprintf(stderr, " next object: %p\n", (void *)base->nurseryNext);
#endif

  // There's probably a more efficient way to do this, but this should be rare
  // enough, that it shouldn't matter too much.
  if ((uint64_t)base->nurseryNext + base->heap_alloc > (uint64_t)base->nurseryEnd) {
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "WARNING: still not enough room for requested allocation of %llu bytes, recurse GC\n", base->heap_alloc);
#endif
    idris_rts_gc(base, orig_sp);
  }
  assert((uint64_t)base->nurseryNext + base->heap_alloc <= (uint64_t)base->nurseryEnd);
  base->heap_alloc = 0;
}

void rapid_gc_init() {
  rapid_global_stackmap_table = generate_table((void *)STACKMAP, 0.5);
}
