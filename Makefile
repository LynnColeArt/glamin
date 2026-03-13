FC = gfortran
CC = cc
AR = ar

BUILD_DIR ?= build
OBJ_DIR ?= $(BUILD_DIR)/obj
MOD_DIR ?= $(BUILD_DIR)/mod
VENV_DIR ?= $(BUILD_DIR)/venv
SPEC ?= docs/geometry_spec.yaml
SPEC_OUT ?= $(BUILD_DIR)/specs
SPEC_CANON ?= $(SPEC_OUT)/spec.json
SPEC_DOT ?= $(SPEC_OUT)/spec.dot
SPEC_LAYOUT ?= $(SPEC_OUT)/vector_layout.json
VENV_PY ?= $(VENV_DIR)/bin/python

FFLAGS ?= -std=f2018 -O2 -Wall -Wextra -J$(MOD_DIR) -I$(MOD_DIR)
CFLAGS ?= -O2 -Wall -Wextra -pthread
ARFLAGS ?= rcs

F90_SOURCES = \
  src/common/mod_errors.f90 \
  src/common/mod_metrics.f90 \
  src/common/mod_embedder.f90 \
  src/common/mod_status.f90 \
  src/common/mod_types.f90 \
  src/common/mod_memory.f90 \
  src/runtime/mod_queue.f90 \
  src/runtime/mod_worker_pool.f90 \
  src/index/mod_flat.f90 \
  src/kernels/mod_distance.f90 \
  src/index/mod_ivf.f90 \
  src/index/mod_pq.f90 \
  src/index/mod_ivfpq.f90 \
  src/index/mod_hnsw.f90 \
  src/io/mod_stream.f90 \
  src/io/mod_vector_io.f90 \
  src/io/mod_geometry_layout.f90 \
  src/io/mod_geometry_loader.f90 \
  src/io/mod_faiss_io.f90 \
  src/runtime/mod_pipeline.f90 \
  src/runtime/mod_async.f90 \
  src/runtime/mod_runtime.f90 \
  src/gpu/mod_gpu_backend.f90

C_SOURCES = \
  src/runtime/thread_pool.c

OBJECTS = \
  $(F90_SOURCES:%.f90=$(OBJ_DIR)/%.o) \
  $(C_SOURCES:%.c=$(OBJ_DIR)/%.o)

LIBRARY = $(BUILD_DIR)/libglamin.a

all: $(LIBRARY)

$(LIBRARY): $(OBJECTS)
	$(AR) $(ARFLAGS) $@ $^

$(OBJ_DIR)/%.o: %.f90 | $(MOD_DIR)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJ_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(MOD_DIR):
	@mkdir -p $(MOD_DIR)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: spec-venv spec-validate spec-compile spec-canonicalize spec-visualize spec-embed

spec-venv:
	python3 -m venv $(VENV_DIR)
	$(VENV_PY) -m pip install -r tools/requirements.txt

spec-validate: spec-venv
	$(VENV_PY) tools/geometry_spec_tool.py validate $(SPEC)

spec-compile: spec-venv
	$(VENV_PY) tools/geometry_spec_tool.py compile $(SPEC) --out-dir $(SPEC_OUT)

spec-canonicalize: spec-venv
	$(VENV_PY) tools/geometry_spec_tool.py canonicalize $(SPEC) --output $(SPEC_CANON)

spec-visualize: spec-venv
	$(VENV_PY) tools/geometry_spec_visualize.py $(SPEC) --output $(SPEC_DOT)

spec-embed: spec-compile
	$(VENV_PY) tools/geometry_embedder_cpu.py $(SPEC) --output $(SPEC_OUT)/vectors.bin

.PHONY: all clean
