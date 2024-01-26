#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matcoloring.c */
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
#define matcoloringcreate_ MATCOLORINGCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringcreate_ matcoloringcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringdestroy_ MATCOLORINGDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringdestroy_ matcoloringdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringsetfromoptions_ MATCOLORINGSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringsetfromoptions_ matcoloringsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringsetdistance_ MATCOLORINGSETDISTANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringsetdistance_ matcoloringsetdistance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringgetdistance_ MATCOLORINGGETDISTANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringgetdistance_ matcoloringgetdistance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringsetmaxcolors_ MATCOLORINGSETMAXCOLORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringsetmaxcolors_ matcoloringsetmaxcolors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringgetmaxcolors_ MATCOLORINGGETMAXCOLORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringgetmaxcolors_ matcoloringgetmaxcolors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringapply_ MATCOLORINGAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringapply_ matcoloringapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringview_ MATCOLORINGVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringview_ matcoloringview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoloringsetweighttype_ MATCOLORINGSETWEIGHTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoloringsetweighttype_ matcoloringsetweighttype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcoloringcreate_(Mat m,MatColoring *mcptr, int *__ierr)
{
*__ierr = MatColoringCreate(
	(Mat)PetscToPointer((m) ),mcptr);
}
PETSC_EXTERN void  matcoloringdestroy_(MatColoring *mc, int *__ierr)
{
*__ierr = MatColoringDestroy(mc);
}
PETSC_EXTERN void  matcoloringsetfromoptions_(MatColoring mc, int *__ierr)
{
*__ierr = MatColoringSetFromOptions(
	(MatColoring)PetscToPointer((mc) ));
}
PETSC_EXTERN void  matcoloringsetdistance_(MatColoring mc,PetscInt *dist, int *__ierr)
{
*__ierr = MatColoringSetDistance(
	(MatColoring)PetscToPointer((mc) ),*dist);
}
PETSC_EXTERN void  matcoloringgetdistance_(MatColoring mc,PetscInt *dist, int *__ierr)
{
*__ierr = MatColoringGetDistance(
	(MatColoring)PetscToPointer((mc) ),dist);
}
PETSC_EXTERN void  matcoloringsetmaxcolors_(MatColoring mc,PetscInt *maxcolors, int *__ierr)
{
*__ierr = MatColoringSetMaxColors(
	(MatColoring)PetscToPointer((mc) ),*maxcolors);
}
PETSC_EXTERN void  matcoloringgetmaxcolors_(MatColoring mc,PetscInt *maxcolors, int *__ierr)
{
*__ierr = MatColoringGetMaxColors(
	(MatColoring)PetscToPointer((mc) ),maxcolors);
}
PETSC_EXTERN void  matcoloringapply_(MatColoring mc,ISColoring *coloring, int *__ierr)
{
*__ierr = MatColoringApply(
	(MatColoring)PetscToPointer((mc) ),coloring);
}
PETSC_EXTERN void  matcoloringview_(MatColoring mc,PetscViewer viewer, int *__ierr)
{
*__ierr = MatColoringView(
	(MatColoring)PetscToPointer((mc) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  matcoloringsetweighttype_(MatColoring mc,MatColoringWeightType *wt, int *__ierr)
{
*__ierr = MatColoringSetWeightType(
	(MatColoring)PetscToPointer((mc) ),*wt);
}
#if defined(__cplusplus)
}
#endif
