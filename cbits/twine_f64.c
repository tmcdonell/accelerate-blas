/*
 * Module      : Twine
 * Copyright   : [2016] Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 *
 * Convert between Accelerate's Struct-of-Array representation of complex
 * numbers and the Array-of-Struct representation used by BLAS.
 */

#include <complex.h>
#include "HsFFI.h"

#ifdef __cplusplus
extern "C" {
#endif

void interleave_f64
(
    const StgInt inf,
    const StgInt sup,
    complex double * __restrict__ cplx,
    const double * __restrict__ real,
    const double * __restrict__ imag
)
{
    for (StgInt i = inf; i < sup; ++i) {
        const double re = real[i];
        const double im = imag[i];

        cplx[i] = CMPLX( re, im );
    }
}

void deinterleave_f64
(
    const StgInt inf,
    const StgInt sup,
    double * __restrict__ real,
    double * __restrict__ imag,
    const complex double * __restrict__ cplx
)
{
    for (StgInt i = inf; i < sup; ++i) {
        const complex double c = cplx[i];

        real[i] = creal(c);
        imag[i] = cimag(c);
    }
}

#ifdef __cplusplus
}
#endif

