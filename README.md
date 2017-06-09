# Numeric linear algebra in Accelerate

[![Build Status](https://travis-ci.org/tmcdonell/accelerate-blas.svg?branch=master)](https://travis-ci.org/tmcdonell/accelerate-blas)

Linear systems, matrix decompositions, and other numerical computations for use
in Accelerate. Most operations are implemented efficiently via FFI calls to BLAS
and LAPACK. For details on Accelerate, refer to the [main repository][GitHub].

Please get in touch to let me know which missing operations you would like see
added to the library. Contributions are also welcome!


## FFI bindings

  * **accelerate-llvm-native:** Bindings are provided by [blas-hs] package,
    which has several options for which underlying BLAS library to link against;
    see that package for setup details.

  * **accelerate-llvm-ptx:** FFI bindings to NVIDIA [cuBLAS] library.

## Complex numbers

Due to Accelerate's struct-of-array representation of complex numbers, compared
to the C-style array-of-struct representation, calling foreign implementations
of complex-valued operations entails an extra data marshalling step.


  [GitHub]:     https://github.com/AccelerateHS/accelerate
  [blas-hs]:    http://hackage.haskell.org/package/blas-hs
  [cuBLAS]:     http://docs.nvidia.com/cuda/cublas/index.html

