#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mcomposite.c */
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
#define matcreatecomposite_ MATCREATECOMPOSITE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatecomposite_ matcreatecomposite
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositeaddmat_ MATCOMPOSITEADDMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositeaddmat_ matcompositeaddmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositesettype_ MATCOMPOSITESETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositesettype_ matcompositesettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositegettype_ MATCOMPOSITEGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositegettype_ matcompositegettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositesetmatstructure_ MATCOMPOSITESETMATSTRUCTURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositesetmatstructure_ matcompositesetmatstructure
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositegetmatstructure_ MATCOMPOSITEGETMATSTRUCTURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositegetmatstructure_ matcompositegetmatstructure
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositesetmergetype_ MATCOMPOSITESETMERGETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositesetmergetype_ matcompositesetmergetype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositemerge_ MATCOMPOSITEMERGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositemerge_ matcompositemerge
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositegetnumbermat_ MATCOMPOSITEGETNUMBERMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositegetnumbermat_ matcompositegetnumbermat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositegetmat_ MATCOMPOSITEGETMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositegetmat_ matcompositegetmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcompositesetscalings_ MATCOMPOSITESETSCALINGS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositesetscalings_ matcompositesetscalings
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcreatecomposite_(MPI_Fint * comm,PetscInt *nmat, Mat *mats,Mat *mat, int *__ierr)
{
*__ierr = MatCreateComposite(
	MPI_Comm_f2c(*(comm)),*nmat,mats,mat);
}
PETSC_EXTERN void  matcompositeaddmat_(Mat mat,Mat smat, int *__ierr)
{
*__ierr = MatCompositeAddMat(
	(Mat)PetscToPointer((mat) ),
	(Mat)PetscToPointer((smat) ));
}
PETSC_EXTERN void  matcompositesettype_(Mat mat,MatCompositeType *type, int *__ierr)
{
*__ierr = MatCompositeSetType(
	(Mat)PetscToPointer((mat) ),*type);
}
PETSC_EXTERN void  matcompositegettype_(Mat mat,MatCompositeType *type, int *__ierr)
{
*__ierr = MatCompositeGetType(
	(Mat)PetscToPointer((mat) ),type);
}
PETSC_EXTERN void  matcompositesetmatstructure_(Mat mat,MatStructure *str, int *__ierr)
{
*__ierr = MatCompositeSetMatStructure(
	(Mat)PetscToPointer((mat) ),*str);
}
PETSC_EXTERN void  matcompositegetmatstructure_(Mat mat,MatStructure *str, int *__ierr)
{
*__ierr = MatCompositeGetMatStructure(
	(Mat)PetscToPointer((mat) ),str);
}
PETSC_EXTERN void  matcompositesetmergetype_(Mat mat,MatCompositeMergeType *type, int *__ierr)
{
*__ierr = MatCompositeSetMergeType(
	(Mat)PetscToPointer((mat) ),*type);
}
PETSC_EXTERN void  matcompositemerge_(Mat mat, int *__ierr)
{
*__ierr = MatCompositeMerge(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  matcompositegetnumbermat_(Mat mat,PetscInt *nmat, int *__ierr)
{
*__ierr = MatCompositeGetNumberMat(
	(Mat)PetscToPointer((mat) ),nmat);
}
PETSC_EXTERN void  matcompositegetmat_(Mat mat,PetscInt *i,Mat *Ai, int *__ierr)
{
*__ierr = MatCompositeGetMat(
	(Mat)PetscToPointer((mat) ),*i,Ai);
}
PETSC_EXTERN void  matcompositesetscalings_(Mat mat, PetscScalar *scalings, int *__ierr)
{
*__ierr = MatCompositeSetScalings(
	(Mat)PetscToPointer((mat) ),scalings);
}
#if defined(__cplusplus)
}
#endif
