#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* lgc.c */
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlggetaxis_ PETSCDRAWLGGETAXIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlggetaxis_ petscdrawlggetaxis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlggetdraw_ PETSCDRAWLGGETDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlggetdraw_ petscdrawlggetdraw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgspdraw_ PETSCDRAWLGSPDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgspdraw_ petscdrawlgspdraw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgcreate_ PETSCDRAWLGCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgcreate_ petscdrawlgcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgsetcolors_ PETSCDRAWLGSETCOLORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgsetcolors_ petscdrawlgsetcolors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlggetdimension_ PETSCDRAWLGGETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlggetdimension_ petscdrawlggetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgsetdimension_ PETSCDRAWLGSETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgsetdimension_ petscdrawlgsetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgsetlimits_ PETSCDRAWLGSETLIMITS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgsetlimits_ petscdrawlgsetlimits
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgreset_ PETSCDRAWLGRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgreset_ petscdrawlgreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgdestroy_ PETSCDRAWLGDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgdestroy_ petscdrawlgdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgsetusemarkers_ PETSCDRAWLGSETUSEMARKERS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgsetusemarkers_ petscdrawlgsetusemarkers
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgdraw_ PETSCDRAWLGDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgdraw_ petscdrawlgdraw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgsave_ PETSCDRAWLGSAVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgsave_ petscdrawlgsave
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgview_ PETSCDRAWLGVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgview_ petscdrawlgview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlgsetfromoptions_ PETSCDRAWLGSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlgsetfromoptions_ petscdrawlgsetfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawlggetaxis_(PetscDrawLG lg,PetscDrawAxis *axis, int *__ierr)
{
*__ierr = PetscDrawLGGetAxis(
	(PetscDrawLG)PetscToPointer((lg) ),axis);
}
PETSC_EXTERN void  petscdrawlggetdraw_(PetscDrawLG lg,PetscDraw *draw, int *__ierr)
{
*__ierr = PetscDrawLGGetDraw(
	(PetscDrawLG)PetscToPointer((lg) ),draw);
}
PETSC_EXTERN void  petscdrawlgspdraw_(PetscDrawLG lg,PetscDrawSP spin, int *__ierr)
{
*__ierr = PetscDrawLGSPDraw(
	(PetscDrawLG)PetscToPointer((lg) ),
	(PetscDrawSP)PetscToPointer((spin) ));
}
PETSC_EXTERN void  petscdrawlgcreate_(PetscDraw draw,PetscInt *dim,PetscDrawLG *outlg, int *__ierr)
{
*__ierr = PetscDrawLGCreate(
	(PetscDraw)PetscToPointer((draw) ),*dim,outlg);
}
PETSC_EXTERN void  petscdrawlgsetcolors_(PetscDrawLG lg, int colors[], int *__ierr)
{
*__ierr = PetscDrawLGSetColors(
	(PetscDrawLG)PetscToPointer((lg) ),colors);
}
PETSC_EXTERN void  petscdrawlggetdimension_(PetscDrawLG lg,PetscInt *dim, int *__ierr)
{
*__ierr = PetscDrawLGGetDimension(
	(PetscDrawLG)PetscToPointer((lg) ),dim);
}
PETSC_EXTERN void  petscdrawlgsetdimension_(PetscDrawLG lg,PetscInt *dim, int *__ierr)
{
*__ierr = PetscDrawLGSetDimension(
	(PetscDrawLG)PetscToPointer((lg) ),*dim);
}
PETSC_EXTERN void  petscdrawlgsetlimits_(PetscDrawLG lg,PetscReal *x_min,PetscReal *x_max,PetscReal *y_min,PetscReal *y_max, int *__ierr)
{
*__ierr = PetscDrawLGSetLimits(
	(PetscDrawLG)PetscToPointer((lg) ),*x_min,*x_max,*y_min,*y_max);
}
PETSC_EXTERN void  petscdrawlgreset_(PetscDrawLG lg, int *__ierr)
{
*__ierr = PetscDrawLGReset(
	(PetscDrawLG)PetscToPointer((lg) ));
}
PETSC_EXTERN void  petscdrawlgdestroy_(PetscDrawLG *lg, int *__ierr)
{
*__ierr = PetscDrawLGDestroy(lg);
}
PETSC_EXTERN void  petscdrawlgsetusemarkers_(PetscDrawLG lg,PetscBool *flg, int *__ierr)
{
*__ierr = PetscDrawLGSetUseMarkers(
	(PetscDrawLG)PetscToPointer((lg) ),*flg);
}
PETSC_EXTERN void  petscdrawlgdraw_(PetscDrawLG lg, int *__ierr)
{
*__ierr = PetscDrawLGDraw(
	(PetscDrawLG)PetscToPointer((lg) ));
}
PETSC_EXTERN void  petscdrawlgsave_(PetscDrawLG lg, int *__ierr)
{
*__ierr = PetscDrawLGSave(
	(PetscDrawLG)PetscToPointer((lg) ));
}
PETSC_EXTERN void  petscdrawlgview_(PetscDrawLG lg,PetscViewer viewer, int *__ierr)
{
*__ierr = PetscDrawLGView(
	(PetscDrawLG)PetscToPointer((lg) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscdrawlgsetfromoptions_(PetscDrawLG lg, int *__ierr)
{
*__ierr = PetscDrawLGSetFromOptions(
	(PetscDrawLG)PetscToPointer((lg) ));
}
#if defined(__cplusplus)
}
#endif
