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
#define matcompositemerge_ MATCOMPOSITEMERGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcompositemerge_ matcompositemerge
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matcreatecomposite_(MPI_Fint * comm,PetscInt *nmat, Mat *mats,Mat *mat, int *__ierr){
*__ierr = MatCreateComposite(
	MPI_Comm_f2c(*(comm)),*nmat,mats,mat);
}
PETSC_EXTERN void PETSC_STDCALL  matcompositeaddmat_(Mat mat,Mat smat, int *__ierr){
*__ierr = MatCompositeAddMat(
	(Mat)PetscToPointer((mat) ),
	(Mat)PetscToPointer((smat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matcompositesettype_(Mat mat,MatCompositeType *type, int *__ierr){
*__ierr = MatCompositeSetType(
	(Mat)PetscToPointer((mat) ),*type);
}
PETSC_EXTERN void PETSC_STDCALL  matcompositemerge_(Mat mat, int *__ierr){
*__ierr = MatCompositeMerge(
	(Mat)PetscToPointer((mat) ));
}
#if defined(__cplusplus)
}
#endif
