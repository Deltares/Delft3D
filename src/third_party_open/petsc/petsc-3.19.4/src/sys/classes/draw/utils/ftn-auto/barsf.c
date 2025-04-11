#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bars.c */
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

#include "petscdraw.h"
#include "petscviewer.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawbardraw_ PETSCDRAWBARDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawbardraw_ petscdrawbardraw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawbarsave_ PETSCDRAWBARSAVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawbarsave_ petscdrawbarsave
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawbarsetcolor_ PETSCDRAWBARSETCOLOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawbarsetcolor_ petscdrawbarsetcolor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawbarsort_ PETSCDRAWBARSORT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawbarsort_ petscdrawbarsort
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawbarsetlimits_ PETSCDRAWBARSETLIMITS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawbarsetlimits_ petscdrawbarsetlimits
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawbarsetfromoptions_ PETSCDRAWBARSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawbarsetfromoptions_ petscdrawbarsetfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawbardraw_(PetscDrawBar bar, int *__ierr)
{
*__ierr = PetscDrawBarDraw(
	(PetscDrawBar)PetscToPointer((bar) ));
}
PETSC_EXTERN void  petscdrawbarsave_(PetscDrawBar bar, int *__ierr)
{
*__ierr = PetscDrawBarSave(
	(PetscDrawBar)PetscToPointer((bar) ));
}
PETSC_EXTERN void  petscdrawbarsetcolor_(PetscDrawBar bar,int *color, int *__ierr)
{
*__ierr = PetscDrawBarSetColor(
	(PetscDrawBar)PetscToPointer((bar) ),*color);
}
PETSC_EXTERN void  petscdrawbarsort_(PetscDrawBar bar,PetscBool *sort,PetscReal *tolerance, int *__ierr)
{
*__ierr = PetscDrawBarSort(
	(PetscDrawBar)PetscToPointer((bar) ),*sort,*tolerance);
}
PETSC_EXTERN void  petscdrawbarsetlimits_(PetscDrawBar bar,PetscReal *y_min,PetscReal *y_max, int *__ierr)
{
*__ierr = PetscDrawBarSetLimits(
	(PetscDrawBar)PetscToPointer((bar) ),*y_min,*y_max);
}
PETSC_EXTERN void  petscdrawbarsetfromoptions_(PetscDrawBar bar, int *__ierr)
{
*__ierr = PetscDrawBarSetFromOptions(
	(PetscDrawBar)PetscToPointer((bar) ));
}
#if defined(__cplusplus)
}
#endif
