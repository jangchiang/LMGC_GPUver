
__global__ void matmul_kernel(double *A, double *B, double *C, int N) {
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < N && j < N) {
    double sum = 0.0;
    for (int k = 0; k < N; k++) {
      sum += A[i * N + k] * B[k * N + j];
    }
    C[i * N + j] = sum;
  }
}

extern "C" void matmul_cuda(double *A, double *B, double *C, int N) {
  // Allocate device memory
  double *d_A, *d_B, *d_C;
  cudaMalloc((void**)&d_A, N*N*sizeof(double));
  cudaMalloc((void**)&d_B, N*N*sizeof(double));
  cudaMalloc((void**)&d_C, N*N*sizeof(double));

  // Copy data to device
  cudaMemcpy(d_A, A, N*N*sizeof(double), cudaMemcpyHostToDevice);
  cudaMemcpy(d_B, B, N*N*sizeof(double), cudaMemcpyHostToDevice);

  // Define block and grid sizes
  dim3 threadsPerBlock(16, 16);
  dim3 numBlocks((N + threadsPerBlock.x - 1) / threadsPerBlock.x, 
                 (N + threadsPerBlock.y - 1) / threadsPerBlock.y);

  // Launch kernel
  matmul_kernel<<<numBlocks, threadsPerBlock>>>(d_A, d_B, d_C, N);

  // Copy result back to host
  cudaMemcpy(C, d_C, N*N*sizeof(double), cudaMemcpyDeviceToHost);

  // Free device memory
  cudaFree(d_A);
  cudaFree(d_B);
  cudaFree(d_C);
}
