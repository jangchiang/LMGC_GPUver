
cmake_minimum_required(VERSION 3.16.0)
project(LMGC90_dev Fortran C CXX)

find_package(CUDA REQUIRED)
set(CUDA_NVCC_FLAGS "${CUDA_NVCC_FLAGS} -arch=sm_75")
include_directories(${CUDA_INCLUDE_DIRS})

set(LMGC90_CUDA_SRCS src/kernel/gpu_module.cu)

cuda_add_library(lmgc90_gpu ${LMGC90_CUDA_SRCS})

add_library(lmgc_core_kernel_2d src/kernel/mod_nlgs.f90)
target_link_libraries(lmgc_core_kernel_2d lmgc90_gpu)
