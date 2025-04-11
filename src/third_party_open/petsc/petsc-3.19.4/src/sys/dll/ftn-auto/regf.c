#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* reg.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfunctionlistdestroy_ PETSCFUNCTIONLISTDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfunctionlistdestroy_ petscfunctionlistdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfunctionlistclear_ PETSCFUNCTIONLISTCLEAR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfunctionlistclear_ petscfunctionlistclear
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfunctionlistview_ PETSCFUNCTIONLISTVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfunctionlistview_ petscfunctionlistview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfunctionlistduplicate_ PETSCFUNCTIONLISTDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfunctionlistduplicate_ petscfunctionlistduplicate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscfunctionlistdestroy_(PetscFunctionList *fl, int *__ierr)
{
*__ierr = PetscFunctionListDestroy(fl);
}
PETSC_EXTERN void  petscfunctionlistclear_(PetscFunctionList fl, int *__ierr)
{
*__ierr = PetscFunctionListClear(
	(PetscFunctionList)PetscToPointer((fl) ));
}
PETSC_EXTERN void  petscfunctionlistview_(PetscFunctionList list,PetscViewer viewer, int *__ierr)
{
*__ierr = PetscFunctionListView(
	(PetscFunctionList)PetscToPointer((list) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscfunctionlistduplicate_(PetscFunctionList fl,PetscFunctionList *nl, int *__ierr)
{
*__ierr = PetscFunctionListDuplicate(
	(PetscFunctionList)PetscToPointer((fl) ),nl);
}
#if defined(__cplusplus)
}
#endif
