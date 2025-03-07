#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* precon.c */
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
#define pcreset_ PCRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcreset_ pcreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetdiagonalscale_ PCSETDIAGONALSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetdiagonalscale_ pcsetdiagonalscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdiagonalscaleleft_ PCDIAGONALSCALELEFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdiagonalscaleleft_ pcdiagonalscaleleft
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdiagonalscaleright_ PCDIAGONALSCALERIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdiagonalscaleright_ pcdiagonalscaleright
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetuseamat_ PCSETUSEAMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetuseamat_ pcsetuseamat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcseterroriffailure_ PCSETERRORIFFAILURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcseterroriffailure_ pcseterroriffailure
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetuseamat_ PCGETUSEAMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetuseamat_ pcgetuseamat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pccreate_ PCCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pccreate_ pccreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapply_ PCAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapply_ pcapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmatapply_ PCMATAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmatapply_ pcmatapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplysymmetricleft_ PCAPPLYSYMMETRICLEFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplysymmetricleft_ pcapplysymmetricleft
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplysymmetricright_ PCAPPLYSYMMETRICRIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplysymmetricright_ pcapplysymmetricright
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplytranspose_ PCAPPLYTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplytranspose_ pcapplytranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplytransposeexists_ PCAPPLYTRANSPOSEEXISTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplytransposeexists_ pcapplytransposeexists
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplybaorab_ PCAPPLYBAORAB
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplybaorab_ pcapplybaorab
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplybaorabtranspose_ PCAPPLYBAORABTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplybaorabtranspose_ pcapplybaorabtranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplyrichardsonexists_ PCAPPLYRICHARDSONEXISTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplyrichardsonexists_ pcapplyrichardsonexists
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcapplyrichardson_ PCAPPLYRICHARDSON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcapplyrichardson_ pcapplyrichardson
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetfailedreason_ PCSETFAILEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetfailedreason_ pcsetfailedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetfailedreason_ PCGETFAILEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetfailedreason_ pcgetfailedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetfailedreasonrank_ PCGETFAILEDREASONRANK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetfailedreasonrank_ pcgetfailedreasonrank
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetup_ PCSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetup_ pcsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetuponblocks_ PCSETUPONBLOCKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetuponblocks_ pcsetuponblocks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetoperators_ PCSETOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetoperators_ pcsetoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetreusepreconditioner_ PCSETREUSEPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetreusepreconditioner_ pcsetreusepreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetreusepreconditioner_ PCGETREUSEPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetreusepreconditioner_ pcgetreusepreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetoperators_ PCGETOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetoperators_ pcgetoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorgetmatrix_ PCFACTORGETMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorgetmatrix_ pcfactorgetmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcpresolve_ PCPRESOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcpresolve_ pcpresolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcpostsolve_ PCPOSTSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcpostsolve_ pcpostsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pccomputeoperator_ PCCOMPUTEOPERATOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pccomputeoperator_ pccomputeoperator
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetcoordinates_ PCSETCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetcoordinates_ pcsetcoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetinterpolations_ PCGETINTERPOLATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetinterpolations_ pcgetinterpolations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetcoarseoperators_ PCGETCOARSEOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetcoarseoperators_ pcgetcoarseoperators
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcreset_(PC pc, int *__ierr)
{
*__ierr = PCReset(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcsetdiagonalscale_(PC pc,Vec s, int *__ierr)
{
*__ierr = PCSetDiagonalScale(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((s) ));
}
PETSC_EXTERN void  pcdiagonalscaleleft_(PC pc,Vec in,Vec out, int *__ierr)
{
*__ierr = PCDiagonalScaleLeft(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((in) ),
	(Vec)PetscToPointer((out) ));
}
PETSC_EXTERN void  pcdiagonalscaleright_(PC pc,Vec in,Vec out, int *__ierr)
{
*__ierr = PCDiagonalScaleRight(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((in) ),
	(Vec)PetscToPointer((out) ));
}
PETSC_EXTERN void  pcsetuseamat_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCSetUseAmat(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcseterroriffailure_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCSetErrorIfFailure(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcgetuseamat_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCGetUseAmat(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pccreate_(MPI_Fint * comm,PC *newpc, int *__ierr)
{
*__ierr = PCCreate(
	MPI_Comm_f2c(*(comm)),newpc);
}
PETSC_EXTERN void  pcapply_(PC pc,Vec x,Vec y, int *__ierr)
{
*__ierr = PCApply(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  pcmatapply_(PC pc,Mat X,Mat Y, int *__ierr)
{
*__ierr = PCMatApply(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((X) ),
	(Mat)PetscToPointer((Y) ));
}
PETSC_EXTERN void  pcapplysymmetricleft_(PC pc,Vec x,Vec y, int *__ierr)
{
*__ierr = PCApplySymmetricLeft(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  pcapplysymmetricright_(PC pc,Vec x,Vec y, int *__ierr)
{
*__ierr = PCApplySymmetricRight(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  pcapplytranspose_(PC pc,Vec x,Vec y, int *__ierr)
{
*__ierr = PCApplyTranspose(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  pcapplytransposeexists_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCApplyTransposeExists(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void  pcapplybaorab_(PC pc,PCSide *side,Vec x,Vec y,Vec work, int *__ierr)
{
*__ierr = PCApplyBAorAB(
	(PC)PetscToPointer((pc) ),*side,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((work) ));
}
PETSC_EXTERN void  pcapplybaorabtranspose_(PC pc,PCSide *side,Vec x,Vec y,Vec work, int *__ierr)
{
*__ierr = PCApplyBAorABTranspose(
	(PC)PetscToPointer((pc) ),*side,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((work) ));
}
PETSC_EXTERN void  pcapplyrichardsonexists_(PC pc,PetscBool *exists, int *__ierr)
{
*__ierr = PCApplyRichardsonExists(
	(PC)PetscToPointer((pc) ),exists);
}
PETSC_EXTERN void  pcapplyrichardson_(PC pc,Vec b,Vec y,Vec w,PetscReal *rtol,PetscReal *abstol,PetscReal *dtol,PetscInt *its,PetscBool *guesszero,PetscInt *outits,PCRichardsonConvergedReason *reason, int *__ierr)
{
*__ierr = PCApplyRichardson(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((w) ),*rtol,*abstol,*dtol,*its,*guesszero,outits,reason);
}
PETSC_EXTERN void  pcsetfailedreason_(PC pc,PCFailedReason *reason, int *__ierr)
{
*__ierr = PCSetFailedReason(
	(PC)PetscToPointer((pc) ),*reason);
}
PETSC_EXTERN void  pcgetfailedreason_(PC pc,PCFailedReason *reason, int *__ierr)
{
*__ierr = PCGetFailedReason(
	(PC)PetscToPointer((pc) ),reason);
}
PETSC_EXTERN void  pcgetfailedreasonrank_(PC pc,PCFailedReason *reason, int *__ierr)
{
*__ierr = PCGetFailedReasonRank(
	(PC)PetscToPointer((pc) ),reason);
}
PETSC_EXTERN void  pcsetup_(PC pc, int *__ierr)
{
*__ierr = PCSetUp(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcsetuponblocks_(PC pc, int *__ierr)
{
*__ierr = PCSetUpOnBlocks(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcsetoperators_(PC pc,Mat Amat,Mat Pmat, int *__ierr)
{
*__ierr = PCSetOperators(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((Amat) ),
	(Mat)PetscToPointer((Pmat) ));
}
PETSC_EXTERN void  pcsetreusepreconditioner_(PC pc,PetscBool *flag, int *__ierr)
{
*__ierr = PCSetReusePreconditioner(
	(PC)PetscToPointer((pc) ),*flag);
}
PETSC_EXTERN void  pcgetreusepreconditioner_(PC pc,PetscBool *flag, int *__ierr)
{
*__ierr = PCGetReusePreconditioner(
	(PC)PetscToPointer((pc) ),flag);
}
PETSC_EXTERN void  pcgetoperators_(PC pc,Mat *Amat,Mat *Pmat, int *__ierr)
{
*__ierr = PCGetOperators(
	(PC)PetscToPointer((pc) ),Amat,Pmat);
}
PETSC_EXTERN void  pcfactorgetmatrix_(PC pc,Mat *mat, int *__ierr)
{
*__ierr = PCFactorGetMatrix(
	(PC)PetscToPointer((pc) ),mat);
}
PETSC_EXTERN void  pcpresolve_(PC pc,KSP ksp, int *__ierr)
{
*__ierr = PCPreSolve(
	(PC)PetscToPointer((pc) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  pcpostsolve_(PC pc,KSP ksp, int *__ierr)
{
*__ierr = PCPostSolve(
	(PC)PetscToPointer((pc) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  pccomputeoperator_(PC pc,MatType *mattype,Mat *mat, int *__ierr)
{
*__ierr = PCComputeOperator(
	(PC)PetscToPointer((pc) ),*mattype,mat);
}
PETSC_EXTERN void  pcsetcoordinates_(PC pc,PetscInt *dim,PetscInt *nloc,PetscReal coords[], int *__ierr)
{
*__ierr = PCSetCoordinates(
	(PC)PetscToPointer((pc) ),*dim,*nloc,coords);
}
PETSC_EXTERN void  pcgetinterpolations_(PC pc,PetscInt *num_levels,Mat *interpolations[], int *__ierr)
{
*__ierr = PCGetInterpolations(
	(PC)PetscToPointer((pc) ),num_levels,interpolations);
}
PETSC_EXTERN void  pcgetcoarseoperators_(PC pc,PetscInt *num_levels,Mat *coarseOperators[], int *__ierr)
{
*__ierr = PCGetCoarseOperators(
	(PC)PetscToPointer((pc) ),num_levels,coarseOperators);
}
#if defined(__cplusplus)
}
#endif
