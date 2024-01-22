#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gasm.c */
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
#define pcgasmsettotalsubdomains_ PCGASMSETTOTALSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgasmsettotalsubdomains_ pcgasmsettotalsubdomains
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgasmsetoverlap_ PCGASMSETOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgasmsetoverlap_ pcgasmsetoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgasmsettype_ PCGASMSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgasmsettype_ pcgasmsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgasmsetsortindices_ PCGASMSETSORTINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgasmsetsortindices_ pcgasmsetsortindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgasmsetusedmsubdomains_ PCGASMSETUSEDMSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgasmsetusedmsubdomains_ pcgasmsetusedmsubdomains
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgasmgetusedmsubdomains_ PCGASMGETUSEDMSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgasmgetusedmsubdomains_ pcgasmgetusedmsubdomains
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcgasmsettotalsubdomains_(PC pc,PetscInt *N, int *__ierr)
{
*__ierr = PCGASMSetTotalSubdomains(
	(PC)PetscToPointer((pc) ),*N);
}
PETSC_EXTERN void  pcgasmsetoverlap_(PC pc,PetscInt *ovl, int *__ierr)
{
*__ierr = PCGASMSetOverlap(
	(PC)PetscToPointer((pc) ),*ovl);
}
PETSC_EXTERN void  pcgasmsettype_(PC pc,PCGASMType *type, int *__ierr)
{
*__ierr = PCGASMSetType(
	(PC)PetscToPointer((pc) ),*type);
}
PETSC_EXTERN void  pcgasmsetsortindices_(PC pc,PetscBool *doSort, int *__ierr)
{
*__ierr = PCGASMSetSortIndices(
	(PC)PetscToPointer((pc) ),*doSort);
}
PETSC_EXTERN void  pcgasmsetusedmsubdomains_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCGASMSetUseDMSubdomains(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcgasmgetusedmsubdomains_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCGASMGetUseDMSubdomains(
	(PC)PetscToPointer((pc) ),flg);
}
#if defined(__cplusplus)
}
#endif
