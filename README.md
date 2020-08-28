<div align="center">
<img width="450" src="https://github.com/AccelerateHS/accelerate/raw/master/images/accelerate-logo-text-v.png?raw=true" alt="henlo, my name is Theia"/>

# Numeric linear algebra in Accelerate

[![GitHub CI](https://github.com/tmcdonell/accelerate-blas/workflows/CI/badge.svg)](https://github.com/tmcdonell/accelerate-blas/actions)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/AccelerateHS/Lobby)
<br>
[![Stackage LTS](https://stackage.org/package/accelerate-blas/badge/lts)](https://stackage.org/lts/package/accelerate-blas)
[![Stackage Nightly](https://stackage.org/package/accelerate-blas/badge/nightly)](https://stackage.org/nightly/package/accelerate-blas)
[![Hackage](https://img.shields.io/hackage/v/accelerate-blas.svg)](https://hackage.haskell.org/package/accelerate-blas)

</div>

Linear systems, matrix decompositions, and other numerical computations for use
in Accelerate. Most operations are implemented efficiently via FFI calls to BLAS
and LAPACK. For details on Accelerate, refer to the [main repository][GitHub].

The following build flags control whether optimised implementations are used.
Note that enabling these (which is the default) will require the corresponding
Accelerate backend as a dependency:

  - `llvm-ptx`: For NVIDIA GPUs
  - `llvm-cpu`: For multicore CPUs

Contributions and bug reports are welcome! Please get in touch to let us know
which missing operations you would like to see added to the library. Please feel
free to contact me through [GitHub][GitHub] or [gitter.im][gitter.im].

  [GitHub]:     https://github.com/AccelerateHS/accelerate
  [gitter.im]:  https://gitter.im/AccelerateHS/Lobby
  [blas-hs]:    http://hackage.haskell.org/package/blas-hs
  [cuBLAS]:     http://docs.nvidia.com/cuda/cublas/index.html

