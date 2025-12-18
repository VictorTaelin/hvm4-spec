// collapse/queue.c - sliding 3-bucket priority queue (top/mid/bot)
//
// Overview:
// - Maintains 3 FIFO buffers:
//   - top: highest priority value observed (numerically largest 'pri')
//   - mid: exactly one below top (top - 1)
//   - bot: all priorities <= (top - 2)
// - On push:
//   - if p <= top-2 -> enqueue on bot
//   - if p == top-1 -> enqueue on mid
//   - if p == top   -> enqueue on top
//   - if p > top    -> slide window up until top == p, then enqueue
// - On pop: try bot, then mid (LIFO), then top (lowest numeric priority first)
// - Stores actual 'pri' per item for correct propagation on push

#define COLLAPSE_QUEUE_BUFSIZE_LOG2 23u
#define COLLAPSE_QUEUE_BUFSIZE      (1u << COLLAPSE_QUEUE_BUFSIZE_LOG2)
#define COLLAPSE_QUEUE_INDEX_MASK   (COLLAPSE_QUEUE_BUFSIZE - 1u)

typedef struct {
  u8  pri;  // 0..63 - lower is better (explored first)
  u32 loc;  // heap location
} CollapseQueueItem;

typedef struct {
  u32               head;  // dequeue cursor (mod COLLAPSE_QUEUE_BUFSIZE)
  u32               size;  // current length
  CollapseQueueItem *data;  // ring storage (FIFO)
} CollapseQueueBuf;

typedef struct {
  CollapseQueueBuf top;      // highest priority value group
  CollapseQueueBuf mid;      // (top - 1)
  CollapseQueueBuf bot;      // (<= top - 2)
  u8               max_pri;  // highest priority value observed
  u8               has_pri;  // whether max_pri has been initialized
} CollapseQueue;

fn void collapse_queue_buf_init(CollapseQueueBuf *b) {
  b->head = 0;
  b->size = 0;
  b->data = (CollapseQueueItem*)malloc((size_t)COLLAPSE_QUEUE_BUFSIZE * sizeof(CollapseQueueItem));
  if (!b->data) {
    fprintf(stderr, "collapse_queue: out of memory\n");
    exit(1);
  }
}

fn void collapse_queue_buf_free(CollapseQueueBuf *b) {
  if (b->data) {
    free(b->data);
    b->data = NULL;
  }
  b->head = 0;
  b->size = 0;
}

fn void collapse_queue_buf_clear(CollapseQueueBuf *b) {
  b->head = 0;
  b->size = 0;
}

fn void collapse_queue_buf_push(CollapseQueueBuf *b, CollapseQueueItem it) {
  if (b->size == COLLAPSE_QUEUE_BUFSIZE) {
    fprintf(stderr, "collapse_queue: buffer overflow\n");
    exit(1);
  }
  u32 tail = (b->head + b->size) & COLLAPSE_QUEUE_INDEX_MASK;
  b->data[tail] = it;
  b->size++;
}

fn u8 collapse_queue_buf_pop(CollapseQueueBuf *b, CollapseQueueItem *out) {
  if (b->size == 0) return 0;
  *out = b->data[b->head & COLLAPSE_QUEUE_INDEX_MASK];
  b->head = (b->head + 1) & COLLAPSE_QUEUE_INDEX_MASK;
  b->size--;
  return 1;
}

// Pop from tail (LIFO) - used for mid bucket
fn u8 collapse_queue_buf_pop_back(CollapseQueueBuf *b, CollapseQueueItem *out) {
  if (b->size == 0) return 0;
  u32 idx = (b->head + b->size - 1) & COLLAPSE_QUEUE_INDEX_MASK;
  *out = b->data[idx];
  b->size--;
  return 1;
}

// Copy ring buffer elements
fn void collapse_queue_buf_ring_copy(CollapseQueueItem *dst_data, u32 dpos, const CollapseQueueItem *src_data, u32 spos, u32 len) {
  while (len > 0) {
    u32 drem = COLLAPSE_QUEUE_BUFSIZE - (dpos & COLLAPSE_QUEUE_INDEX_MASK);
    u32 srem = COLLAPSE_QUEUE_BUFSIZE - (spos & COLLAPSE_QUEUE_INDEX_MASK);
    u32 chunk = len;
    if (chunk > drem) chunk = drem;
    if (chunk > srem) chunk = srem;
    memcpy(&dst_data[dpos & COLLAPSE_QUEUE_INDEX_MASK],
           &src_data[spos & COLLAPSE_QUEUE_INDEX_MASK],
           chunk * sizeof(CollapseQueueItem));
    dpos += chunk;
    spos += chunk;
    len -= chunk;
  }
}

// Append all elements from src to dst (FIFO order), empties src
fn void collapse_queue_buf_append_all(CollapseQueueBuf *dst, CollapseQueueBuf *src) {
  if (src->size == 0) return;
  if (dst->size + src->size > COLLAPSE_QUEUE_BUFSIZE) {
    fprintf(stderr, "collapse_queue: overflow in append\n");
    exit(1);
  }
  u32 dst_tail = dst->head + dst->size;
  collapse_queue_buf_ring_copy(dst->data, dst_tail, src->data, src->head, src->size);
  dst->size += src->size;
  collapse_queue_buf_clear(src);
}

// Prepend all elements from src to front of dst, empties src
fn void collapse_queue_buf_prepend_all(CollapseQueueBuf *dst, CollapseQueueBuf *src) {
  if (src->size == 0) return;
  if (dst->size + src->size > COLLAPSE_QUEUE_BUFSIZE) {
    fprintf(stderr, "collapse_queue: overflow in prepend\n");
    exit(1);
  }
  u32 new_head = (dst->head - src->size) & COLLAPSE_QUEUE_INDEX_MASK;
  collapse_queue_buf_ring_copy(dst->data, new_head, src->data, src->head, src->size);
  dst->head = new_head;
  dst->size += src->size;
  collapse_queue_buf_clear(src);
}

fn void collapse_queue_init(CollapseQueue *q) {
  collapse_queue_buf_init(&q->top);
  collapse_queue_buf_init(&q->mid);
  collapse_queue_buf_init(&q->bot);
  q->max_pri = 0;
  q->has_pri = 0;
}

fn void collapse_queue_free(CollapseQueue *q) {
  collapse_queue_buf_free(&q->top);
  collapse_queue_buf_free(&q->mid);
  collapse_queue_buf_free(&q->bot);
  q->has_pri = 0;
}

// Slide the top/mid/bot window up by 1 (max_pri := max_pri + 1)
fn void collapse_queue_slide_up(CollapseQueue *q) {
  CollapseQueueBuf old_top = q->top;
  CollapseQueueBuf old_mid = q->mid;
  CollapseQueueBuf old_bot = q->bot;

  if (old_bot.size <= old_mid.size) {
    // Prepend bot into mid: [bot][mid]
    collapse_queue_buf_prepend_all(&old_mid, &old_bot);
    q->top = old_bot;  // empty, becomes new top
    q->mid = old_top;
    q->bot = old_mid;
  } else {
    // Append mid into bot: [bot][mid]
    collapse_queue_buf_append_all(&old_bot, &old_mid);
    q->top = old_mid;  // empty, becomes new top
    q->mid = old_top;
    q->bot = old_bot;
  }
  q->max_pri = q->max_pri + 1;
}

fn void collapse_queue_push(CollapseQueue *q, CollapseQueueItem it) {
  u8 p = it.pri & 63;  // clamp to 0..63

  if (!q->has_pri) {
    q->max_pri = p;
    q->has_pri = 1;
    collapse_queue_buf_push(&q->top, it);
    return;
  }

  int d = (int)p - (int)q->max_pri;
  if (d <= -2) {
    collapse_queue_buf_push(&q->bot, it);
  } else if (d == -1) {
    collapse_queue_buf_push(&q->mid, it);
  } else if (d == 0) {
    collapse_queue_buf_push(&q->top, it);
  } else {
    // d > 0: slide up until max_pri == p
    for (int i = 0; i < d; i++) {
      collapse_queue_slide_up(q);
    }
    collapse_queue_buf_push(&q->top, it);
  }
}

fn u8 collapse_queue_pop(CollapseQueue *q, CollapseQueueItem *out) {
  // Pop order: bot (FIFO), mid (LIFO), top (FIFO)
  // This ensures lower numeric priority is processed first
  if (q->bot.size > 0) return collapse_queue_buf_pop(&q->bot, out);
  if (q->mid.size > 0) return collapse_queue_buf_pop_back(&q->mid, out);
  if (q->top.size > 0) return collapse_queue_buf_pop(&q->top, out);
  return 0;
}

fn u8 collapse_queue_is_empty(CollapseQueue *q) {
  return (q->top.size | q->mid.size | q->bot.size) == 0;
}
