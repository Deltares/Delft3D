#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fieldsplit.c */
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
#define pcfieldsplitsetdiaguseamat_ PCFIELDSPLITSETDIAGUSEAMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetdiaguseamat_ pcfieldsplitsetdiaguseamat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitgetdiaguseamat_ PCFIELDSPLITGETDIAGUSEAMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitgetdiaguseamat_ pcfieldsplitgetdiaguseamat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetoffdiaguseamat_ PCFIELDSPLITSETOFFDIAGUSEAMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetoffdiaguseamat_ pcfieldsplitsetoffdiaguseamat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitgetoffdiaguseamat_ PCFIELDSPLITGETOFFDIAGUSEAMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitgetoffdiaguseamat_ pcfieldsplitgetoffdiaguseamat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetblocksize_ PCFIELDSPLITSETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetblocksize_ pcfieldsplitsetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetschurpre_ PCFIELDSPLITSETSCHURPRE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetschurpre_ pcfieldsplitsetschurpre
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitgetschurpre_ PCFIELDSPLITGETSCHURPRE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitgetschurpre_ pcfieldsplitgetschurpre
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitschurgets_ PCFIELDSPLITSCHURGETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitschurgets_ pcfieldsplitschurgets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitschurrestores_ PCFIELDSPLITSCHURRESTORES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitschurrestores_ pcfieldsplitschurrestores
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetschurfacttype_ PCFIELDSPLITSETSCHURFACTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetschurfacttype_ pcfieldsplitsetschurfacttype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetschurscale_ PCFIELDSPLITSETSCHURSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetschurscale_ pcfieldsplitsetschurscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetgkbtol_ PCFIELDSPLITSETGKBTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetgkbtol_ pcfieldsplitsetgkbtol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetgkbmaxit_ PCFIELDSPLITSETGKBMAXIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetgkbmaxit_ pcfieldsplitsetgkbmaxit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetgkbdelay_ PCFIELDSPLITSETGKBDELAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetgkbdelay_ pcfieldsplitsetgkbdelay
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetgkbnu_ PCFIELDSPLITSETGKBNU
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetgkbnu_ pcfieldsplitsetgkbnu
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsettype_ PCFIELDSPLITSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsettype_ pcfieldsplitsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitgettype_ PCFIELDSPLITGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitgettype_ pcfieldsplitgettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetdmsplits_ PCFIELDSPLITSETDMSPLITS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetdmsplits_ pcfieldsplitsetdmsplits
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitgetdmsplits_ PCFIELDSPLITGETDMSPLITS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitgetdmsplits_ pcfieldsplitgetdmsplits
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitgetdetectsaddlepoint_ PCFIELDSPLITGETDETECTSADDLEPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitgetdetectsaddlepoint_ pcfieldsplitgetdetectsaddlepoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfieldsplitsetdetectsaddlepoint_ PCFIELDSPLITSETDETECTSADDLEPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfieldsplitsetdetectsaddlepoint_ pcfieldsplitsetdetectsaddlepoint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcfieldsplitsetdiaguseamat_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitSetDiagUseAmat(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcfieldsplitgetdiaguseamat_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitGetDiagUseAmat(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcfieldsplitsetoffdiaguseamat_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitSetOffDiagUseAmat(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcfieldsplitgetoffdiaguseamat_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitGetOffDiagUseAmat(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcfieldsplitsetblocksize_(PC pc,PetscInt *bs, int *__ierr)
{
*__ierr = PCFieldSplitSetBlockSize(
	(PC)PetscToPointer((pc) ),*bs);
}
PETSC_EXTERN void  pcfieldsplitsetschurpre_(PC pc,PCFieldSplitSchurPreType *ptype,Mat pre, int *__ierr)
{
*__ierr = PCFieldSplitSetSchurPre(
	(PC)PetscToPointer((pc) ),*ptype,
	(Mat)PetscToPointer((pre) ));
}
PETSC_EXTERN void  pcfieldsplitgetschurpre_(PC pc,PCFieldSplitSchurPreType *ptype,Mat *pre, int *__ierr)
{
*__ierr = PCFieldSplitGetSchurPre(
	(PC)PetscToPointer((pc) ),ptype,pre);
}
PETSC_EXTERN void  pcfieldsplitschurgets_(PC pc,Mat *S, int *__ierr)
{
*__ierr = PCFieldSplitSchurGetS(
	(PC)PetscToPointer((pc) ),S);
}
PETSC_EXTERN void  pcfieldsplitschurrestores_(PC pc,Mat *S, int *__ierr)
{
*__ierr = PCFieldSplitSchurRestoreS(
	(PC)PetscToPointer((pc) ),S);
}
PETSC_EXTERN void  pcfieldsplitsetschurfacttype_(PC pc,PCFieldSplitSchurFactType *ftype, int *__ierr)
{
*__ierr = PCFieldSplitSetSchurFactType(
	(PC)PetscToPointer((pc) ),*ftype);
}
PETSC_EXTERN void  pcfieldsplitsetschurscale_(PC pc,PetscScalar *scale, int *__ierr)
{
*__ierr = PCFieldSplitSetSchurScale(
	(PC)PetscToPointer((pc) ),*scale);
}
PETSC_EXTERN void  pcfieldsplitsetgkbtol_(PC pc,PetscReal *tolerance, int *__ierr)
{
*__ierr = PCFieldSplitSetGKBTol(
	(PC)PetscToPointer((pc) ),*tolerance);
}
PETSC_EXTERN void  pcfieldsplitsetgkbmaxit_(PC pc,PetscInt *maxit, int *__ierr)
{
*__ierr = PCFieldSplitSetGKBMaxit(
	(PC)PetscToPointer((pc) ),*maxit);
}
PETSC_EXTERN void  pcfieldsplitsetgkbdelay_(PC pc,PetscInt *delay, int *__ierr)
{
*__ierr = PCFieldSplitSetGKBDelay(
	(PC)PetscToPointer((pc) ),*delay);
}
PETSC_EXTERN void  pcfieldsplitsetgkbnu_(PC pc,PetscReal *nu, int *__ierr)
{
*__ierr = PCFieldSplitSetGKBNu(
	(PC)PetscToPointer((pc) ),*nu);
}
PETSC_EXTERN void  pcfieldsplitsettype_(PC pc,PCCompositeType *type, int *__ierr)
{
*__ierr = PCFieldSplitSetType(
	(PC)PetscToPointer((pc) ),*type);
}
PETSC_EXTERN void  pcfieldsplitgettype_(PC pc,PCCompositeType *type, int *__ierr)
{
*__ierr = PCFieldSplitGetType(
	(PC)PetscToPointer((pc) ),type);
}
PETSC_EXTERN void  pcfieldsplitsetdmsplits_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitSetDMSplits(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcfieldsplitgetdmsplits_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitGetDMSplits(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcfieldsplitgetdetectsaddlepoint_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitGetDetectSaddlePoint(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcfieldsplitsetdetectsaddlepoint_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCFieldSplitSetDetectSaddlePoint(
	(PC)PetscToPointer((pc) ),*flg);
}
#if defined(__cplusplus)
}
#endif
