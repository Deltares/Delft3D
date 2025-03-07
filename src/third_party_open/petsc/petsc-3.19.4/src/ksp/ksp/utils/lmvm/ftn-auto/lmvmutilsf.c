#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* lmvmutils.c */
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

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmupdate_ MATLMVMUPDATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmupdate_ matlmvmupdate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmclearj0_ MATLMVMCLEARJ0
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmclearj0_ matlmvmclearj0
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsetj0scale_ MATLMVMSETJ0SCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsetj0scale_ matlmvmsetj0scale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsetj0diag_ MATLMVMSETJ0DIAG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsetj0diag_ matlmvmsetj0diag
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsetj0_ MATLMVMSETJ0
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsetj0_ matlmvmsetj0
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsetj0pc_ MATLMVMSETJ0PC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsetj0pc_ matlmvmsetj0pc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsetj0ksp_ MATLMVMSETJ0KSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsetj0ksp_ matlmvmsetj0ksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmgetj0_ MATLMVMGETJ0
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmgetj0_ matlmvmgetj0
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmgetj0pc_ MATLMVMGETJ0PC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmgetj0pc_ matlmvmgetj0pc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmgetj0ksp_ MATLMVMGETJ0KSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmgetj0ksp_ matlmvmgetj0ksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmapplyj0fwd_ MATLMVMAPPLYJ0FWD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmapplyj0fwd_ matlmvmapplyj0fwd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmapplyj0inv_ MATLMVMAPPLYJ0INV
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmapplyj0inv_ matlmvmapplyj0inv
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmisallocated_ MATLMVMISALLOCATED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmisallocated_ matlmvmisallocated
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmallocate_ MATLMVMALLOCATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmallocate_ matlmvmallocate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmresetshift_ MATLMVMRESETSHIFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmresetshift_ matlmvmresetshift
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmreset_ MATLMVMRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmreset_ matlmvmreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsethistorysize_ MATLMVMSETHISTORYSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsethistorysize_ matlmvmsethistorysize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmgetupdatecount_ MATLMVMGETUPDATECOUNT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmgetupdatecount_ matlmvmgetupdatecount
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmgetrejectcount_ MATLMVMGETREJECTCOUNT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmgetrejectcount_ matlmvmgetrejectcount
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matlmvmupdate_(Mat B,Vec X,Vec F, int *__ierr)
{
*__ierr = MatLMVMUpdate(
	(Mat)PetscToPointer((B) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ));
}
PETSC_EXTERN void  matlmvmclearj0_(Mat B, int *__ierr)
{
*__ierr = MatLMVMClearJ0(
	(Mat)PetscToPointer((B) ));
}
PETSC_EXTERN void  matlmvmsetj0scale_(Mat B,PetscReal *scale, int *__ierr)
{
*__ierr = MatLMVMSetJ0Scale(
	(Mat)PetscToPointer((B) ),*scale);
}
PETSC_EXTERN void  matlmvmsetj0diag_(Mat B,Vec V, int *__ierr)
{
*__ierr = MatLMVMSetJ0Diag(
	(Mat)PetscToPointer((B) ),
	(Vec)PetscToPointer((V) ));
}
PETSC_EXTERN void  matlmvmsetj0_(Mat B,Mat J0, int *__ierr)
{
*__ierr = MatLMVMSetJ0(
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((J0) ));
}
PETSC_EXTERN void  matlmvmsetj0pc_(Mat B,PC J0pc, int *__ierr)
{
*__ierr = MatLMVMSetJ0PC(
	(Mat)PetscToPointer((B) ),
	(PC)PetscToPointer((J0pc) ));
}
PETSC_EXTERN void  matlmvmsetj0ksp_(Mat B,KSP J0ksp, int *__ierr)
{
*__ierr = MatLMVMSetJ0KSP(
	(Mat)PetscToPointer((B) ),
	(KSP)PetscToPointer((J0ksp) ));
}
PETSC_EXTERN void  matlmvmgetj0_(Mat B,Mat *J0, int *__ierr)
{
*__ierr = MatLMVMGetJ0(
	(Mat)PetscToPointer((B) ),J0);
}
PETSC_EXTERN void  matlmvmgetj0pc_(Mat B,PC *J0pc, int *__ierr)
{
*__ierr = MatLMVMGetJ0PC(
	(Mat)PetscToPointer((B) ),J0pc);
}
PETSC_EXTERN void  matlmvmgetj0ksp_(Mat B,KSP *J0ksp, int *__ierr)
{
*__ierr = MatLMVMGetJ0KSP(
	(Mat)PetscToPointer((B) ),J0ksp);
}
PETSC_EXTERN void  matlmvmapplyj0fwd_(Mat B,Vec X,Vec Y, int *__ierr)
{
*__ierr = MatLMVMApplyJ0Fwd(
	(Mat)PetscToPointer((B) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Y) ));
}
PETSC_EXTERN void  matlmvmapplyj0inv_(Mat B,Vec X,Vec Y, int *__ierr)
{
*__ierr = MatLMVMApplyJ0Inv(
	(Mat)PetscToPointer((B) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Y) ));
}

PETSC_EXTERN void  matlmvmisallocated_(Mat B,PetscBool *flg, int *__ierr)
{
*__ierr = MatLMVMIsAllocated(
	(Mat)PetscToPointer((B) ),flg);
}
PETSC_EXTERN void  matlmvmallocate_(Mat B,Vec X,Vec F, int *__ierr)
{
*__ierr = MatLMVMAllocate(
	(Mat)PetscToPointer((B) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ));
}
PETSC_EXTERN void  matlmvmresetshift_(Mat B, int *__ierr)
{
*__ierr = MatLMVMResetShift(
	(Mat)PetscToPointer((B) ));
}
PETSC_EXTERN void  matlmvmreset_(Mat B,PetscBool *destructive, int *__ierr)
{
*__ierr = MatLMVMReset(
	(Mat)PetscToPointer((B) ),*destructive);
}
PETSC_EXTERN void  matlmvmsethistorysize_(Mat B,PetscInt *hist_size, int *__ierr)
{
*__ierr = MatLMVMSetHistorySize(
	(Mat)PetscToPointer((B) ),*hist_size);
}
PETSC_EXTERN void  matlmvmgetupdatecount_(Mat B,PetscInt *nupdates, int *__ierr)
{
*__ierr = MatLMVMGetUpdateCount(
	(Mat)PetscToPointer((B) ),nupdates);
}
PETSC_EXTERN void  matlmvmgetrejectcount_(Mat B,PetscInt *nrejects, int *__ierr)
{
*__ierr = MatLMVMGetRejectCount(
	(Mat)PetscToPointer((B) ),nrejects);
}
#if defined(__cplusplus)
}
#endif
