#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gcreate.c */
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
#define matcreate_ MATCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreate_ matcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matseterroriffailure_ MATSETERRORIFFAILURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseterroriffailure_ matseterroriffailure
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetsizes_ MATSETSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetsizes_ matsetsizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetfromoptions_ MATSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetfromoptions_ matsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matbindtocpu_ MATBINDTOCPU
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matbindtocpu_ matbindtocpu
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matboundtocpu_ MATBOUNDTOCPU
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matboundtocpu_ matboundtocpu
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvaluescoo_ MATSETVALUESCOO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvaluescoo_ matsetvaluescoo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetbindingpropagates_ MATSETBINDINGPROPAGATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetbindingpropagates_ matsetbindingpropagates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetbindingpropagates_ MATGETBINDINGPROPAGATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetbindingpropagates_ matgetbindingpropagates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcreate_(MPI_Fint * comm,Mat *A, int *__ierr)
{
*__ierr = MatCreate(
	MPI_Comm_f2c(*(comm)),A);
}
PETSC_EXTERN void  matseterroriffailure_(Mat mat,PetscBool *flg, int *__ierr)
{
*__ierr = MatSetErrorIfFailure(
	(Mat)PetscToPointer((mat) ),*flg);
}
PETSC_EXTERN void  matsetsizes_(Mat A,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N, int *__ierr)
{
*__ierr = MatSetSizes(
	(Mat)PetscToPointer((A) ),*m,*n,*M,*N);
}
PETSC_EXTERN void  matsetfromoptions_(Mat B, int *__ierr)
{
*__ierr = MatSetFromOptions(
	(Mat)PetscToPointer((B) ));
}
PETSC_EXTERN void  matbindtocpu_(Mat A,PetscBool *flg, int *__ierr)
{
*__ierr = MatBindToCPU(
	(Mat)PetscToPointer((A) ),*flg);
}
PETSC_EXTERN void  matboundtocpu_(Mat A,PetscBool *flg, int *__ierr)
{
*__ierr = MatBoundToCPU(
	(Mat)PetscToPointer((A) ),flg);
}
PETSC_EXTERN void  matsetvaluescoo_(Mat A, PetscScalar coo_v[],InsertMode *imode, int *__ierr)
{
*__ierr = MatSetValuesCOO(
	(Mat)PetscToPointer((A) ),coo_v,*imode);
}
PETSC_EXTERN void  matsetbindingpropagates_(Mat A,PetscBool *flg, int *__ierr)
{
*__ierr = MatSetBindingPropagates(
	(Mat)PetscToPointer((A) ),*flg);
}
PETSC_EXTERN void  matgetbindingpropagates_(Mat A,PetscBool *flg, int *__ierr)
{
*__ierr = MatGetBindingPropagates(
	(Mat)PetscToPointer((A) ),flg);
}
#if defined(__cplusplus)
}
#endif
