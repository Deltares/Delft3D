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
#define pcdestroy_ PCDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdestroy_ pcdestroy
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
#define pcgetsetupfailedreason_ PCGETSETUPFAILEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetsetupfailedreason_ pcgetsetupfailedreason
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
#define pccomputeexplicitoperator_ PCCOMPUTEEXPLICITOPERATOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pccomputeexplicitoperator_ pccomputeexplicitoperator
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetcoordinates_ PCSETCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetcoordinates_ pcsetcoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pcreset_(PC pc, int *__ierr){
*__ierr = PCReset(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcdestroy_(PC *pc, int *__ierr){
*__ierr = PCDestroy(pc);
}
PETSC_EXTERN void PETSC_STDCALL  pcsetdiagonalscale_(PC pc,Vec s, int *__ierr){
*__ierr = PCSetDiagonalScale(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((s) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcdiagonalscaleleft_(PC pc,Vec in,Vec out, int *__ierr){
*__ierr = PCDiagonalScaleLeft(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((in) ),
	(Vec)PetscToPointer((out) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcdiagonalscaleright_(PC pc,Vec in,Vec out, int *__ierr){
*__ierr = PCDiagonalScaleRight(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((in) ),
	(Vec)PetscToPointer((out) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcsetuseamat_(PC pc,PetscBool *flg, int *__ierr){
*__ierr = PCSetUseAmat(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  pcseterroriffailure_(PC pc,PetscBool *flg, int *__ierr){
*__ierr = PCSetErrorIfFailure(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  pcgetuseamat_(PC pc,PetscBool *flg, int *__ierr){
*__ierr = PCGetUseAmat(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  pccreate_(MPI_Fint * comm,PC *newpc, int *__ierr){
*__ierr = PCCreate(
	MPI_Comm_f2c(*(comm)),newpc);
}
PETSC_EXTERN void PETSC_STDCALL  pcapply_(PC pc,Vec x,Vec y, int *__ierr){
*__ierr = PCApply(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcapplysymmetricleft_(PC pc,Vec x,Vec y, int *__ierr){
*__ierr = PCApplySymmetricLeft(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcapplysymmetricright_(PC pc,Vec x,Vec y, int *__ierr){
*__ierr = PCApplySymmetricRight(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcapplytranspose_(PC pc,Vec x,Vec y, int *__ierr){
*__ierr = PCApplyTranspose(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcapplytransposeexists_(PC pc,PetscBool  *flg, int *__ierr){
*__ierr = PCApplyTransposeExists(
	(PC)PetscToPointer((pc) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  pcapplybaorab_(PC pc,PCSide *side,Vec x,Vec y,Vec work, int *__ierr){
*__ierr = PCApplyBAorAB(
	(PC)PetscToPointer((pc) ),*side,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((work) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcapplybaorabtranspose_(PC pc,PCSide *side,Vec x,Vec y,Vec work, int *__ierr){
*__ierr = PCApplyBAorABTranspose(
	(PC)PetscToPointer((pc) ),*side,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((work) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcapplyrichardsonexists_(PC pc,PetscBool  *exists, int *__ierr){
*__ierr = PCApplyRichardsonExists(
	(PC)PetscToPointer((pc) ),exists);
}
PETSC_EXTERN void PETSC_STDCALL  pcapplyrichardson_(PC pc,Vec b,Vec y,Vec w,PetscReal *rtol,PetscReal *abstol,PetscReal *dtol,PetscInt *its,PetscBool *guesszero,PetscInt *outits,PCRichardsonConvergedReason *reason, int *__ierr){
*__ierr = PCApplyRichardson(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((w) ),*rtol,*abstol,*dtol,*its,*guesszero,outits,
	(PCRichardsonConvergedReason* )PetscToPointer((reason) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcgetsetupfailedreason_(PC pc,PCFailedReason *reason, int *__ierr){
*__ierr = PCGetSetUpFailedReason(
	(PC)PetscToPointer((pc) ),reason);
}
PETSC_EXTERN void PETSC_STDCALL  pcsetup_(PC pc, int *__ierr){
*__ierr = PCSetUp(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcsetuponblocks_(PC pc, int *__ierr){
*__ierr = PCSetUpOnBlocks(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcsetoperators_(PC pc,Mat Amat,Mat Pmat, int *__ierr){
*__ierr = PCSetOperators(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((Amat) ),
	(Mat)PetscToPointer((Pmat) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcsetreusepreconditioner_(PC pc,PetscBool *flag, int *__ierr){
*__ierr = PCSetReusePreconditioner(
	(PC)PetscToPointer((pc) ),*flag);
}
PETSC_EXTERN void PETSC_STDCALL  pcgetreusepreconditioner_(PC pc,PetscBool *flag, int *__ierr){
*__ierr = PCGetReusePreconditioner(
	(PC)PetscToPointer((pc) ),flag);
}
PETSC_EXTERN void PETSC_STDCALL  pcgetoperators_(PC pc,Mat *Amat,Mat *Pmat, int *__ierr){
*__ierr = PCGetOperators(
	(PC)PetscToPointer((pc) ),Amat,Pmat);
}
PETSC_EXTERN void PETSC_STDCALL  pcfactorgetmatrix_(PC pc,Mat *mat, int *__ierr){
*__ierr = PCFactorGetMatrix(
	(PC)PetscToPointer((pc) ),mat);
}
PETSC_EXTERN void PETSC_STDCALL  pcpresolve_(PC pc,KSP ksp, int *__ierr){
*__ierr = PCPreSolve(
	(PC)PetscToPointer((pc) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcpostsolve_(PC pc,KSP ksp, int *__ierr){
*__ierr = PCPostSolve(
	(PC)PetscToPointer((pc) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void PETSC_STDCALL  pccomputeexplicitoperator_(PC pc,Mat *mat, int *__ierr){
*__ierr = PCComputeExplicitOperator(
	(PC)PetscToPointer((pc) ),mat);
}
PETSC_EXTERN void PETSC_STDCALL  pcsetcoordinates_(PC pc,PetscInt *dim,PetscInt *nloc,PetscReal *coords, int *__ierr){
*__ierr = PCSetCoordinates(
	(PC)PetscToPointer((pc) ),*dim,*nloc,coords);
}
#if defined(__cplusplus)
}
#endif
