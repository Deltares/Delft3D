#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmshell.c */
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

#include "petscdmshell.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmglobaltolocalbegindefaultshell_ DMGLOBALTOLOCALBEGINDEFAULTSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmglobaltolocalbegindefaultshell_ dmglobaltolocalbegindefaultshell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmglobaltolocalenddefaultshell_ DMGLOBALTOLOCALENDDEFAULTSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmglobaltolocalenddefaultshell_ dmglobaltolocalenddefaultshell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlocaltoglobalbegindefaultshell_ DMLOCALTOGLOBALBEGINDEFAULTSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocaltoglobalbegindefaultshell_ dmlocaltoglobalbegindefaultshell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlocaltoglobalenddefaultshell_ DMLOCALTOGLOBALENDDEFAULTSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocaltoglobalenddefaultshell_ dmlocaltoglobalenddefaultshell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlocaltolocalbegindefaultshell_ DMLOCALTOLOCALBEGINDEFAULTSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocaltolocalbegindefaultshell_ dmlocaltolocalbegindefaultshell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlocaltolocalenddefaultshell_ DMLOCALTOLOCALENDDEFAULTSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocaltolocalenddefaultshell_ dmlocaltolocalenddefaultshell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetdestroycontext_ DMSHELLSETDESTROYCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetdestroycontext_ dmshellsetdestroycontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetcontext_ DMSHELLSETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetcontext_ dmshellsetcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellgetcontext_ DMSHELLGETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellgetcontext_ dmshellgetcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetmatrix_ DMSHELLSETMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetmatrix_ dmshellsetmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetglobalvector_ DMSHELLSETGLOBALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetglobalvector_ dmshellsetglobalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellgetglobalvector_ DMSHELLGETGLOBALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellgetglobalvector_ dmshellgetglobalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetlocalvector_ DMSHELLSETLOCALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetlocalvector_ dmshellsetlocalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetglobaltolocalvecscatter_ DMSHELLSETGLOBALTOLOCALVECSCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetglobaltolocalvecscatter_ dmshellsetglobaltolocalvecscatter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetlocaltoglobalvecscatter_ DMSHELLSETLOCALTOGLOBALVECSCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetlocaltoglobalvecscatter_ dmshellsetlocaltoglobalvecscatter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellsetlocaltolocalvecscatter_ DMSHELLSETLOCALTOLOCALVECSCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellsetlocaltolocalvecscatter_ dmshellsetlocaltolocalvecscatter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmshellcreate_ DMSHELLCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmshellcreate_ dmshellcreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmglobaltolocalbegindefaultshell_(DM dm,Vec g,InsertMode *mode,Vec l, int *__ierr)
{
*__ierr = DMGlobalToLocalBeginDefaultShell(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((g) ),*mode,
	(Vec)PetscToPointer((l) ));
}
PETSC_EXTERN void  dmglobaltolocalenddefaultshell_(DM dm,Vec g,InsertMode *mode,Vec l, int *__ierr)
{
*__ierr = DMGlobalToLocalEndDefaultShell(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((g) ),*mode,
	(Vec)PetscToPointer((l) ));
}
PETSC_EXTERN void  dmlocaltoglobalbegindefaultshell_(DM dm,Vec l,InsertMode *mode,Vec g, int *__ierr)
{
*__ierr = DMLocalToGlobalBeginDefaultShell(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((l) ),*mode,
	(Vec)PetscToPointer((g) ));
}
PETSC_EXTERN void  dmlocaltoglobalenddefaultshell_(DM dm,Vec l,InsertMode *mode,Vec g, int *__ierr)
{
*__ierr = DMLocalToGlobalEndDefaultShell(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((l) ),*mode,
	(Vec)PetscToPointer((g) ));
}
PETSC_EXTERN void  dmlocaltolocalbegindefaultshell_(DM dm,Vec g,InsertMode *mode,Vec l, int *__ierr)
{
*__ierr = DMLocalToLocalBeginDefaultShell(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((g) ),*mode,
	(Vec)PetscToPointer((l) ));
}
PETSC_EXTERN void  dmlocaltolocalenddefaultshell_(DM dm,Vec g,InsertMode *mode,Vec l, int *__ierr)
{
*__ierr = DMLocalToLocalEndDefaultShell(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((g) ),*mode,
	(Vec)PetscToPointer((l) ));
}
PETSC_EXTERN void  dmshellsetdestroycontext_(DM dm,PetscErrorCode (*destroyctx)(void *), int *__ierr)
{
*__ierr = DMShellSetDestroyContext(
	(DM)PetscToPointer((dm) ),destroyctx);
}
PETSC_EXTERN void  dmshellsetcontext_(DM dm,void*ctx, int *__ierr)
{
*__ierr = DMShellSetContext(
	(DM)PetscToPointer((dm) ),ctx);
}
PETSC_EXTERN void  dmshellgetcontext_(DM dm,void*ctx, int *__ierr)
{
*__ierr = DMShellGetContext(
	(DM)PetscToPointer((dm) ),ctx);
}
PETSC_EXTERN void  dmshellsetmatrix_(DM dm,Mat J, int *__ierr)
{
*__ierr = DMShellSetMatrix(
	(DM)PetscToPointer((dm) ),
	(Mat)PetscToPointer((J) ));
}
PETSC_EXTERN void  dmshellsetglobalvector_(DM dm,Vec X, int *__ierr)
{
*__ierr = DMShellSetGlobalVector(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ));
}
PETSC_EXTERN void  dmshellgetglobalvector_(DM dm,Vec *X, int *__ierr)
{
*__ierr = DMShellGetGlobalVector(
	(DM)PetscToPointer((dm) ),X);
}
PETSC_EXTERN void  dmshellsetlocalvector_(DM dm,Vec X, int *__ierr)
{
*__ierr = DMShellSetLocalVector(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ));
}
PETSC_EXTERN void  dmshellsetglobaltolocalvecscatter_(DM dm,VecScatter gtol, int *__ierr)
{
*__ierr = DMShellSetGlobalToLocalVecScatter(
	(DM)PetscToPointer((dm) ),
	(VecScatter)PetscToPointer((gtol) ));
}
PETSC_EXTERN void  dmshellsetlocaltoglobalvecscatter_(DM dm,VecScatter ltog, int *__ierr)
{
*__ierr = DMShellSetLocalToGlobalVecScatter(
	(DM)PetscToPointer((dm) ),
	(VecScatter)PetscToPointer((ltog) ));
}
PETSC_EXTERN void  dmshellsetlocaltolocalvecscatter_(DM dm,VecScatter ltol, int *__ierr)
{
*__ierr = DMShellSetLocalToLocalVecScatter(
	(DM)PetscToPointer((dm) ),
	(VecScatter)PetscToPointer((ltol) ));
}
PETSC_EXTERN void  dmshellcreate_(MPI_Fint * comm,DM *dm, int *__ierr)
{
*__ierr = DMShellCreate(
	MPI_Comm_f2c(*(comm)),dm);
}
#if defined(__cplusplus)
}
#endif
