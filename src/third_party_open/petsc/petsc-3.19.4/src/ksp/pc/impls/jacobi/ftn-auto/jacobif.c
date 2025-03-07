#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* jacobi.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcjacobisetuseabs_ PCJACOBISETUSEABS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcjacobisetuseabs_ pcjacobisetuseabs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcjacobigetuseabs_ PCJACOBIGETUSEABS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcjacobigetuseabs_ pcjacobigetuseabs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcjacobisetfixdiagonal_ PCJACOBISETFIXDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcjacobisetfixdiagonal_ pcjacobisetfixdiagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcjacobigetfixdiagonal_ PCJACOBIGETFIXDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcjacobigetfixdiagonal_ pcjacobigetfixdiagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcjacobisettype_ PCJACOBISETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcjacobisettype_ pcjacobisettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcjacobigettype_ PCJACOBIGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcjacobigettype_ pcjacobigettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcjacobisetuseabs_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCJacobiSetUseAbs(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcjacobigetuseabs_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCJacobiGetUseAbs(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcjacobisetfixdiagonal_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCJacobiSetFixDiagonal(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcjacobigetfixdiagonal_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCJacobiGetFixDiagonal(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcjacobisettype_(PC pc,PCJacobiType *type, int *__ierr)
{
*__ierr = PCJacobiSetType(
	(PC)PetscToPointer((pc) ),*type);
}
PETSC_EXTERN void  pcjacobigettype_(PC pc,PCJacobiType *type, int *__ierr)
{
*__ierr = PCJacobiGetType(
	(PC)PetscToPointer((pc) ),type);
}
#if defined(__cplusplus)
}
#endif
