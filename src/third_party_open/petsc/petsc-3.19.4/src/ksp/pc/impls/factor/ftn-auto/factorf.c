#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* factor.c */
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
#define pcfactorsetupmatsolvertype_ PCFACTORSETUPMATSOLVERTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetupmatsolvertype_ pcfactorsetupmatsolvertype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetzeropivot_ PCFACTORSETZEROPIVOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetzeropivot_ pcfactorsetzeropivot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetshifttype_ PCFACTORSETSHIFTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetshifttype_ pcfactorsetshifttype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetshiftamount_ PCFACTORSETSHIFTAMOUNT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetshiftamount_ pcfactorsetshiftamount
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetdroptolerance_ PCFACTORSETDROPTOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetdroptolerance_ pcfactorsetdroptolerance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetzeropivot_ PCFACTORGETZEROPIVOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetzeropivot_ pcfactorgetzeropivot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetshiftamount_ PCFACTORGETSHIFTAMOUNT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetshiftamount_ pcfactorgetshiftamount
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetshifttype_ PCFACTORGETSHIFTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetshifttype_ pcfactorgetshifttype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetlevels_ PCFACTORGETLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetlevels_ pcfactorgetlevels
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetlevels_ PCFACTORSETLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetlevels_ pcfactorsetlevels
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetallowdiagonalfill_ PCFACTORSETALLOWDIAGONALFILL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetallowdiagonalfill_ pcfactorsetallowdiagonalfill
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetallowdiagonalfill_ PCFACTORGETALLOWDIAGONALFILL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetallowdiagonalfill_ pcfactorgetallowdiagonalfill
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorreorderfornonzerodiagonal_ PCFACTORREORDERFORNONZERODIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorreorderfornonzerodiagonal_ pcfactorreorderfornonzerodiagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetfill_ PCFACTORSETFILL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetfill_ pcfactorsetfill
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetuseinplace_ PCFACTORSETUSEINPLACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetuseinplace_ pcfactorsetuseinplace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetuseinplace_ PCFACTORGETUSEINPLACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetuseinplace_ pcfactorgetuseinplace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetcolumnpivot_ PCFACTORSETCOLUMNPIVOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetcolumnpivot_ pcfactorsetcolumnpivot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetpivotinblocks_ PCFACTORSETPIVOTINBLOCKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetpivotinblocks_ pcfactorsetpivotinblocks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetreusefill_ PCFACTORSETREUSEFILL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetreusefill_ pcfactorsetreusefill
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcfactorsetupmatsolvertype_(PC pc, int *__ierr)
{
*__ierr = PCFactorSetUpMatSolverType(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcfactorsetzeropivot_(PC pc,PetscReal *zero, int *__ierr)
{
*__ierr = PCFactorSetZeroPivot(
	(PC)PetscToPointer((pc) ),*zero);
}
PETSC_EXTERN void  pcfactorsetshifttype_(PC pc,MatFactorShiftType *shifttype, int *__ierr)
{
*__ierr = PCFactorSetShiftType(
	(PC)PetscToPointer((pc) ),*shifttype);
}
PETSC_EXTERN void  pcfactorsetshiftamount_(PC pc,PetscReal *shiftamount, int *__ierr)
{
*__ierr = PCFactorSetShiftAmount(
	(PC)PetscToPointer((pc) ),*shiftamount);
}
PETSC_EXTERN void  pcfactorsetdroptolerance_(PC pc,PetscReal *dt,PetscReal *dtcol,PetscInt *maxrowcount, int *__ierr)
{
*__ierr = PCFactorSetDropTolerance(
	(PC)PetscToPointer((pc) ),*dt,*dtcol,*maxrowcount);
}
PETSC_EXTERN void  pcfactorgetzeropivot_(PC pc,PetscReal *pivot, int *__ierr)
{
*__ierr = PCFactorGetZeroPivot(
	(PC)PetscToPointer((pc) ),pivot);
}
PETSC_EXTERN void  pcfactorgetshiftamount_(PC pc,PetscReal *shift, int *__ierr)
{
*__ierr = PCFactorGetShiftAmount(
	(PC)PetscToPointer((pc) ),shift);
}
PETSC_EXTERN void  pcfactorgetshifttype_(PC pc,MatFactorShiftType *type, int *__ierr)
{
*__ierr = PCFactorGetShiftType(
	(PC)PetscToPointer((pc) ),type);
}
PETSC_EXTERN void  pcfactorgetlevels_(PC pc,PetscInt *levels, int *__ierr)
{
*__ierr = PCFactorGetLevels(
	(PC)PetscToPointer((pc) ),levels);
}
PETSC_EXTERN void  pcfactorsetlevels_(PC pc,PetscInt *levels, int *__ierr)
{
*__ierr = PCFactorSetLevels(
	(PC)PetscToPointer((pc) ),*levels);
}
PETSC_EXTERN void  pcfactorsetallowdiagonalfill_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFactorSetAllowDiagonalFill(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcfactorgetallowdiagonalfill_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFactorGetAllowDiagonalFill(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcfactorreorderfornonzerodiagonal_(PC pc,PetscReal *rtol, int *__ierr)
{
*__ierr = PCFactorReorderForNonzeroDiagonal(
	(PC)PetscToPointer((pc) ),*rtol);
}
PETSC_EXTERN void  pcfactorsetfill_(PC pc,PetscReal *fill, int *__ierr)
{
*__ierr = PCFactorSetFill(
	(PC)PetscToPointer((pc) ),*fill);
}
PETSC_EXTERN void  pcfactorsetuseinplace_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFactorSetUseInPlace(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcfactorgetuseinplace_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFactorGetUseInPlace(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcfactorsetcolumnpivot_(PC pc,PetscReal *dtcol, int *__ierr)
{
*__ierr = PCFactorSetColumnPivot(
	(PC)PetscToPointer((pc) ),*dtcol);
}
PETSC_EXTERN void  pcfactorsetpivotinblocks_(PC pc,PetscBool *pivot, int *__ierr)
{
*__ierr = PCFactorSetPivotInBlocks(
	(PC)PetscToPointer((pc) ),*pivot);
}
PETSC_EXTERN void  pcfactorsetreusefill_(PC pc,PetscBool *flag, int *__ierr)
{
*__ierr = PCFactorSetReuseFill(
	(PC)PetscToPointer((pc) ),*flag);
}
#if defined(__cplusplus)
}
#endif
