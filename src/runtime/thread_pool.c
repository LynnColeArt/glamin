#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>

#define GLAMIN_OK 0
#define GLAMIN_ERR_INVALID_ARG 2
#define GLAMIN_ERR_OOM 3
#define GLAMIN_ERR_NOT_READY 4

#define GLAMIN_REQUEST_COMPLETED 2

typedef void (*glamin_job_fn)(void *);

typedef struct {
  glamin_job_fn function;
  void *context;
} glamin_job;

typedef struct {
  glamin_job *jobs;
  int32_t capacity;
  int32_t head;
  int32_t tail;
  int32_t count;
  int32_t is_shutdown;
  pthread_mutex_t mutex;
  pthread_cond_t has_jobs;
} glamin_job_queue;

typedef struct {
  pthread_t *threads;
  int32_t thread_count;
  glamin_job_queue queue;
} glamin_thread_pool;

typedef struct {
  int64_t request_id;
  glamin_job_fn function;
  void *context;
} glamin_request_context;

extern void glamin_mark_request_status(int64_t request_id, int32_t status, int32_t error_code);

static int glamin_job_queue_init(glamin_job_queue *queue, int32_t capacity) {
  if (!queue || capacity <= 0) {
    return GLAMIN_ERR_INVALID_ARG;
  }

  queue->jobs = calloc((size_t)capacity, sizeof(glamin_job));
  if (!queue->jobs) {
    return GLAMIN_ERR_OOM;
  }

  queue->capacity = capacity;
  queue->head = 0;
  queue->tail = 0;
  queue->count = 0;
  queue->is_shutdown = 0;

  if (pthread_mutex_init(&queue->mutex, NULL) != 0) {
    free(queue->jobs);
    queue->jobs = NULL;
    return GLAMIN_ERR_OOM;
  }

  if (pthread_cond_init(&queue->has_jobs, NULL) != 0) {
    pthread_mutex_destroy(&queue->mutex);
    free(queue->jobs);
    queue->jobs = NULL;
    return GLAMIN_ERR_OOM;
  }

  return GLAMIN_OK;
}

static void glamin_job_queue_shutdown(glamin_job_queue *queue) {
  if (!queue) {
    return;
  }

  pthread_mutex_lock(&queue->mutex);
  queue->is_shutdown = 1;
  pthread_cond_broadcast(&queue->has_jobs);
  pthread_mutex_unlock(&queue->mutex);
}

static void glamin_job_queue_destroy(glamin_job_queue *queue) {
  if (!queue) {
    return;
  }

  pthread_mutex_destroy(&queue->mutex);
  pthread_cond_destroy(&queue->has_jobs);
  free(queue->jobs);
  queue->jobs = NULL;
  queue->capacity = 0;
  queue->head = 0;
  queue->tail = 0;
  queue->count = 0;
  queue->is_shutdown = 0;
}

static int glamin_job_queue_push(glamin_job_queue *queue, glamin_job job) {
  int status = GLAMIN_OK;

  if (!queue) {
    return GLAMIN_ERR_INVALID_ARG;
  }

  pthread_mutex_lock(&queue->mutex);
  if (queue->is_shutdown) {
    status = GLAMIN_ERR_NOT_READY;
  } else if (queue->count >= queue->capacity) {
    status = GLAMIN_ERR_NOT_READY;
  } else {
    queue->jobs[queue->tail] = job;
    queue->tail = (queue->tail + 1) % queue->capacity;
    queue->count += 1;
    pthread_cond_signal(&queue->has_jobs);
  }
  pthread_mutex_unlock(&queue->mutex);

  return status;
}

static int glamin_job_queue_pop(glamin_job_queue *queue, glamin_job *job) {
  if (!queue || !job) {
    return 0;
  }

  pthread_mutex_lock(&queue->mutex);
  while (queue->count == 0 && !queue->is_shutdown) {
    pthread_cond_wait(&queue->has_jobs, &queue->mutex);
  }

  if (queue->count == 0 && queue->is_shutdown) {
    pthread_mutex_unlock(&queue->mutex);
    return 0;
  }

  *job = queue->jobs[queue->head];
  queue->head = (queue->head + 1) % queue->capacity;
  queue->count -= 1;
  pthread_mutex_unlock(&queue->mutex);

  return 1;
}

static void *glamin_worker_main(void *arg) {
  glamin_thread_pool *pool = (glamin_thread_pool *)arg;
  glamin_job job;

  if (!pool) {
    return NULL;
  }

  while (glamin_job_queue_pop(&pool->queue, &job)) {
    if (job.function) {
      job.function(job.context);
    }
  }

  return NULL;
}

glamin_thread_pool *glamin_thread_pool_create(int32_t thread_count, int32_t capacity) {
  glamin_thread_pool *pool = NULL;
  int32_t i;
  int status;

  if (thread_count <= 0 || capacity <= 0) {
    return NULL;
  }

  pool = calloc(1, sizeof(*pool));
  if (!pool) {
    return NULL;
  }

  pool->threads = calloc((size_t)thread_count, sizeof(pthread_t));
  if (!pool->threads) {
    free(pool);
    return NULL;
  }

  status = glamin_job_queue_init(&pool->queue, capacity);
  if (status != GLAMIN_OK) {
    free(pool->threads);
    free(pool);
    return NULL;
  }

  pool->thread_count = thread_count;
  for (i = 0; i < thread_count; i++) {
    if (pthread_create(&pool->threads[i], NULL, glamin_worker_main, pool) != 0) {
      glamin_job_queue_shutdown(&pool->queue);
      while (i > 0) {
        i -= 1;
        pthread_join(pool->threads[i], NULL);
      }
      glamin_job_queue_destroy(&pool->queue);
      free(pool->threads);
      free(pool);
      return NULL;
    }
  }

  return pool;
}

void glamin_thread_pool_destroy(glamin_thread_pool *pool) {
  int32_t i;

  if (!pool) {
    return;
  }

  glamin_job_queue_shutdown(&pool->queue);
  for (i = 0; i < pool->thread_count; i++) {
    pthread_join(pool->threads[i], NULL);
  }

  glamin_job_queue_destroy(&pool->queue);
  free(pool->threads);
  free(pool);
}

int glamin_thread_pool_submit(glamin_thread_pool *pool, glamin_job_fn callback, void *context) {
  glamin_job job;

  if (!pool || !callback) {
    return GLAMIN_ERR_INVALID_ARG;
  }

  job.function = callback;
  job.context = context;
  return glamin_job_queue_push(&pool->queue, job);
}

static void glamin_complete_request_job(void *context) {
  glamin_request_context *request_context = (glamin_request_context *)context;
  int32_t error_code = GLAMIN_OK;

  if (!request_context) {
    return;
  }

  if (request_context->function) {
    request_context->function(request_context->context);
  }

  glamin_mark_request_status(
    request_context->request_id,
    GLAMIN_REQUEST_COMPLETED,
    error_code
  );
  free(request_context);
}

int glamin_thread_pool_submit_request(glamin_thread_pool *pool, int64_t request_id) {
  glamin_request_context *context = NULL;
  int status;

  if (!pool || request_id <= 0) {
    return GLAMIN_ERR_INVALID_ARG;
  }

  context = calloc(1, sizeof(*context));
  if (!context) {
    return GLAMIN_ERR_OOM;
  }

  context->request_id = request_id;
  context->function = NULL;
  context->context = NULL;
  status = glamin_thread_pool_submit(pool, glamin_complete_request_job, context);
  if (status != GLAMIN_OK) {
    free(context);
  }

  return status;
}

int glamin_thread_pool_submit_request_with_job(
  glamin_thread_pool *pool,
  int64_t request_id,
  glamin_job_fn callback,
  void *context_ptr
) {
  glamin_request_context *context = NULL;
  int status;

  if (!pool || request_id <= 0) {
    return GLAMIN_ERR_INVALID_ARG;
  }

  context = calloc(1, sizeof(*context));
  if (!context) {
    return GLAMIN_ERR_OOM;
  }

  context->request_id = request_id;
  context->function = callback;
  context->context = context_ptr;
  status = glamin_thread_pool_submit(pool, glamin_complete_request_job, context);
  if (status != GLAMIN_OK) {
    free(context);
  }

  return status;
}
