#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snesmfj.c */
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

#include "petscsnes.h"
#include "petscdm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsnesmfgetsnes_ MATSNESMFGETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsnesmfgetsnes_ matsnesmfgetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsnesmfsetreusebase_ MATSNESMFSETREUSEBASE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsnesmfsetreusebase_ matsnesmfsetreusebase
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsnesmfgetreusebase_ MATSNESMFGETREUSEBASE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsnesmfgetreusebase_ matsnesmfgetreusebase
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatesnesmf_ MATCREATESNESMF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatesnesmf_ matcreatesnesmf
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matsnesmfgetsnes_(Mat J,SNES *snes, int *__ierr)
{
*__ierr = MatSNESMFGetSNES(
	(Mat)PetscToPointer((J) ),snes);
}
PETSC_EXTERN void  matsnesmfsetreusebase_(Mat J,PetscBool *use, int *__ierr)
{
*__ierr = MatSNESMFSetReuseBase(
	(Mat)PetscToPointer((J) ),*use);
}
PETSC_EXTERN void  matsnesmfgetreusebase_(Mat J,PetscBool *use, int *__ierr)
{
*__ierr = MatSNESMFGetReuseBase(
	(Mat)PetscToPointer((J) ),use);
}
PETSC_EXTERN void  matcreatesnesmf_(SNES snes,Mat *J, int *__ierr)
{
*__ierr = MatCreateSNESMF(
	(SNES)PetscToPointer((snes) ),J);
}
#if defined(__cplusplus)
}
#endif
