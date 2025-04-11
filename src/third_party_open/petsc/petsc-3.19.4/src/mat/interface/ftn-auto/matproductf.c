#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matproduct.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductnumeric_ MATPRODUCTNUMERIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductnumeric_ matproductnumeric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductsymbolic_ MATPRODUCTSYMBOLIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductsymbolic_ matproductsymbolic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductsetfill_ MATPRODUCTSETFILL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductsetfill_ matproductsetfill
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductsetalgorithm_ MATPRODUCTSETALGORITHM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductsetalgorithm_ matproductsetalgorithm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductsettype_ MATPRODUCTSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductsettype_ matproductsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductclear_ MATPRODUCTCLEAR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductclear_ matproductclear
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductcreatewithmat_ MATPRODUCTCREATEWITHMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductcreatewithmat_ matproductcreatewithmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductcreate_ MATPRODUCTCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductcreate_ matproductcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductgettype_ MATPRODUCTGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductgettype_ matproductgettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matproductgetmats_ MATPRODUCTGETMATS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matproductgetmats_ matproductgetmats
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matproductnumeric_(Mat mat, int *__ierr)
{
*__ierr = MatProductNumeric(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  matproductsymbolic_(Mat mat, int *__ierr)
{
*__ierr = MatProductSymbolic(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  matproductsetfill_(Mat mat,PetscReal *fill, int *__ierr)
{
*__ierr = MatProductSetFill(
	(Mat)PetscToPointer((mat) ),*fill);
}
PETSC_EXTERN void  matproductsetalgorithm_(Mat mat,MatProductAlgorithm *alg, int *__ierr)
{
*__ierr = MatProductSetAlgorithm(
	(Mat)PetscToPointer((mat) ),*alg);
}
PETSC_EXTERN void  matproductsettype_(Mat mat,MatProductType *productype, int *__ierr)
{
*__ierr = MatProductSetType(
	(Mat)PetscToPointer((mat) ),*productype);
}
PETSC_EXTERN void  matproductclear_(Mat mat, int *__ierr)
{
*__ierr = MatProductClear(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  matproductcreatewithmat_(Mat A,Mat B,Mat C,Mat D, int *__ierr)
{
*__ierr = MatProductCreateWithMat(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((C) ),
	(Mat)PetscToPointer((D) ));
}
PETSC_EXTERN void  matproductcreate_(Mat A,Mat B,Mat C,Mat *D, int *__ierr)
{
*__ierr = MatProductCreate(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((C) ),D);
}
PETSC_EXTERN void  matproductgettype_(Mat mat,MatProductType *mtype, int *__ierr)
{
*__ierr = MatProductGetType(
	(Mat)PetscToPointer((mat) ),mtype);
}
PETSC_EXTERN void  matproductgetmats_(Mat mat,Mat *A,Mat *B,Mat *C, int *__ierr)
{
*__ierr = MatProductGetMats(
	(Mat)PetscToPointer((mat) ),A,B,C);
}
#if defined(__cplusplus)
}
#endif
