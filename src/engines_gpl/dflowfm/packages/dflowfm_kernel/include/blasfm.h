!! BLAS routines come either from Intel MKL, or standard BLAS from SPARSKIT. Select at compile time.
!#if defined __INTEL_COMPILER && __INTEL_COMPILER > 1000
!! MKL available
!#define HAVE_MKL 1
!
!#define dnrm2(N, X, INCX) dnrm2(N, X, INCX)
!#define ddot(N, DX, INCX, DY, INCY) ddot(N, DX, INCX, DY, INCY)
!#define daxpy(N, DA, DX, INCX, DY, INCY) daxpy(N, DA, DX, INCX, DY, INCY)
!#define amux(n, x, y, a, ja, ia) mkl_dcsrgemv('N', n, a, ia, ja, x, y)
!
!#define EXTERNAL_DNRM2 double precision, external :: dnrm2
!#define EXTERNAL_DDOT  double precision, external :: ddot
!
!#else
!! No MKL, BLAS from SPARSKIT
#define HAVE_MKL 0

#define dnrm2(N, X, INCX) dnrm2XXX(n, vv, 1)
#define ddot(N, DX, INCX, DY, INCY) ddotXXX(N, DX, INCX, DY, INCY)
#define daxpy(N, DA, DX, INCX, DY, INCY) daxpyXXX(N, DA, DX, INCX, DY, INCY)
#define amux(n, x, y, a, ja, ia) amuxXXX(n, x, y, a, ja, ia) 

#define EXTERNAL_DNRM2 double precision, external :: dnrm2XXX
#define EXTERNAL_DDOT  double precision, external :: ddotXXX

!#endif