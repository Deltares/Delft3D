#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* schurm.c */
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
#define matcreateschurcomplement_ MATCREATESCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateschurcomplement_ matcreateschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementsetsubmatrices_ MATSCHURCOMPLEMENTSETSUBMATRICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementsetsubmatrices_ matschurcomplementsetsubmatrices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementgetksp_ MATSCHURCOMPLEMENTGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementgetksp_ matschurcomplementgetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementsetksp_ MATSCHURCOMPLEMENTSETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementsetksp_ matschurcomplementsetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementupdatesubmatrices_ MATSCHURCOMPLEMENTUPDATESUBMATRICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementupdatesubmatrices_ matschurcomplementupdatesubmatrices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementcomputeexplicitoperator_ MATSCHURCOMPLEMENTCOMPUTEEXPLICITOPERATOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementcomputeexplicitoperator_ matschurcomplementcomputeexplicitoperator
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetschurcomplement_ MATGETSCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetschurcomplement_ matgetschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementsetainvtype_ MATSCHURCOMPLEMENTSETAINVTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementsetainvtype_ matschurcomplementsetainvtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementgetainvtype_ MATSCHURCOMPLEMENTGETAINVTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementgetainvtype_ matschurcomplementgetainvtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateschurcomplementpmat_ MATCREATESCHURCOMPLEMENTPMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateschurcomplementpmat_ matcreateschurcomplementpmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matschurcomplementgetpmat_ MATSCHURCOMPLEMENTGETPMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matschurcomplementgetpmat_ matschurcomplementgetpmat
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcreateschurcomplement_(Mat A00,Mat Ap00,Mat A01,Mat A10,Mat A11,Mat *S, int *__ierr)
{
*__ierr = MatCreateSchurComplement(
	(Mat)PetscToPointer((A00) ),
	(Mat)PetscToPointer((Ap00) ),
	(Mat)PetscToPointer((A01) ),
	(Mat)PetscToPointer((A10) ),
	(Mat)PetscToPointer((A11) ),S);
}
PETSC_EXTERN void  matschurcomplementsetsubmatrices_(Mat S,Mat A00,Mat Ap00,Mat A01,Mat A10,Mat A11, int *__ierr)
{
*__ierr = MatSchurComplementSetSubMatrices(
	(Mat)PetscToPointer((S) ),
	(Mat)PetscToPointer((A00) ),
	(Mat)PetscToPointer((Ap00) ),
	(Mat)PetscToPointer((A01) ),
	(Mat)PetscToPointer((A10) ),
	(Mat)PetscToPointer((A11) ));
}
PETSC_EXTERN void  matschurcomplementgetksp_(Mat S,KSP *ksp, int *__ierr)
{
*__ierr = MatSchurComplementGetKSP(
	(Mat)PetscToPointer((S) ),ksp);
}
PETSC_EXTERN void  matschurcomplementsetksp_(Mat S,KSP ksp, int *__ierr)
{
*__ierr = MatSchurComplementSetKSP(
	(Mat)PetscToPointer((S) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  matschurcomplementupdatesubmatrices_(Mat S,Mat A00,Mat Ap00,Mat A01,Mat A10,Mat A11, int *__ierr)
{
*__ierr = MatSchurComplementUpdateSubMatrices(
	(Mat)PetscToPointer((S) ),
	(Mat)PetscToPointer((A00) ),
	(Mat)PetscToPointer((Ap00) ),
	(Mat)PetscToPointer((A01) ),
	(Mat)PetscToPointer((A10) ),
	(Mat)PetscToPointer((A11) ));
}
PETSC_EXTERN void  matschurcomplementcomputeexplicitoperator_(Mat A,Mat *S, int *__ierr)
{
*__ierr = MatSchurComplementComputeExplicitOperator(
	(Mat)PetscToPointer((A) ),S);
}
PETSC_EXTERN void  matgetschurcomplement_(Mat A,IS isrow0,IS iscol0,IS isrow1,IS iscol1,MatReuse *mreuse,Mat *S,MatSchurComplementAinvType *ainvtype,MatReuse *preuse,Mat *Sp, int *__ierr)
{
*__ierr = MatGetSchurComplement(
	(Mat)PetscToPointer((A) ),
	(IS)PetscToPointer((isrow0) ),
	(IS)PetscToPointer((iscol0) ),
	(IS)PetscToPointer((isrow1) ),
	(IS)PetscToPointer((iscol1) ),*mreuse,S,*ainvtype,*preuse,Sp);
}
PETSC_EXTERN void  matschurcomplementsetainvtype_(Mat S,MatSchurComplementAinvType *ainvtype, int *__ierr)
{
*__ierr = MatSchurComplementSetAinvType(
	(Mat)PetscToPointer((S) ),*ainvtype);
}
PETSC_EXTERN void  matschurcomplementgetainvtype_(Mat S,MatSchurComplementAinvType *ainvtype, int *__ierr)
{
*__ierr = MatSchurComplementGetAinvType(
	(Mat)PetscToPointer((S) ),ainvtype);
}
PETSC_EXTERN void  matcreateschurcomplementpmat_(Mat A00,Mat A01,Mat A10,Mat A11,MatSchurComplementAinvType *ainvtype,MatReuse *preuse,Mat *Sp, int *__ierr)
{
*__ierr = MatCreateSchurComplementPmat(
	(Mat)PetscToPointer((A00) ),
	(Mat)PetscToPointer((A01) ),
	(Mat)PetscToPointer((A10) ),
	(Mat)PetscToPointer((A11) ),*ainvtype,*preuse,Sp);
}
PETSC_EXTERN void  matschurcomplementgetpmat_(Mat S,MatReuse *preuse,Mat *Sp, int *__ierr)
{
*__ierr = MatSchurComplementGetPmat(
	(Mat)PetscToPointer((S) ),*preuse,Sp);
}
#if defined(__cplusplus)
}
#endif
