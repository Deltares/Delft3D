#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* ao.c */
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

#include "petscao.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aodestroy_ AODESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aodestroy_ aodestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aopetsctoapplicationis_ AOPETSCTOAPPLICATIONIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aopetsctoapplicationis_ aopetsctoapplicationis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aoapplicationtopetscis_ AOAPPLICATIONTOPETSCIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aoapplicationtopetscis_ aoapplicationtopetscis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aopetsctoapplication_ AOPETSCTOAPPLICATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aopetsctoapplication_ aopetsctoapplication
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aoapplicationtopetsc_ AOAPPLICATIONTOPETSC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aoapplicationtopetsc_ aoapplicationtopetsc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aopetsctoapplicationpermuteint_ AOPETSCTOAPPLICATIONPERMUTEINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aopetsctoapplicationpermuteint_ aopetsctoapplicationpermuteint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aoapplicationtopetscpermuteint_ AOAPPLICATIONTOPETSCPERMUTEINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aoapplicationtopetscpermuteint_ aoapplicationtopetscpermuteint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aopetsctoapplicationpermutereal_ AOPETSCTOAPPLICATIONPERMUTEREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aopetsctoapplicationpermutereal_ aopetsctoapplicationpermutereal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aoapplicationtopetscpermutereal_ AOAPPLICATIONTOPETSCPERMUTEREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aoapplicationtopetscpermutereal_ aoapplicationtopetscpermutereal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aosetfromoptions_ AOSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aosetfromoptions_ aosetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aosetis_ AOSETIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aosetis_ aosetis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aocreate_ AOCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aocreate_ aocreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  aodestroy_(AO *ao, int *__ierr){
*__ierr = AODestroy(ao);
}
PETSC_EXTERN void PETSC_STDCALL  aopetsctoapplicationis_(AO ao,IS is, int *__ierr){
*__ierr = AOPetscToApplicationIS(
	(AO)PetscToPointer((ao) ),
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void PETSC_STDCALL  aoapplicationtopetscis_(AO ao,IS is, int *__ierr){
*__ierr = AOApplicationToPetscIS(
	(AO)PetscToPointer((ao) ),
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void PETSC_STDCALL  aopetsctoapplication_(AO ao,PetscInt *n,PetscInt ia[], int *__ierr){
*__ierr = AOPetscToApplication(
	(AO)PetscToPointer((ao) ),*n,ia);
}
PETSC_EXTERN void PETSC_STDCALL  aoapplicationtopetsc_(AO ao,PetscInt *n,PetscInt ia[], int *__ierr){
*__ierr = AOApplicationToPetsc(
	(AO)PetscToPointer((ao) ),*n,ia);
}
PETSC_EXTERN void PETSC_STDCALL  aopetsctoapplicationpermuteint_(AO ao,PetscInt *block,PetscInt array[], int *__ierr){
*__ierr = AOPetscToApplicationPermuteInt(
	(AO)PetscToPointer((ao) ),*block,array);
}
PETSC_EXTERN void PETSC_STDCALL  aoapplicationtopetscpermuteint_(AO ao,PetscInt *block,PetscInt array[], int *__ierr){
*__ierr = AOApplicationToPetscPermuteInt(
	(AO)PetscToPointer((ao) ),*block,array);
}
PETSC_EXTERN void PETSC_STDCALL  aopetsctoapplicationpermutereal_(AO ao,PetscInt *block,PetscReal array[], int *__ierr){
*__ierr = AOPetscToApplicationPermuteReal(
	(AO)PetscToPointer((ao) ),*block,array);
}
PETSC_EXTERN void PETSC_STDCALL  aoapplicationtopetscpermutereal_(AO ao,PetscInt *block,PetscReal array[], int *__ierr){
*__ierr = AOApplicationToPetscPermuteReal(
	(AO)PetscToPointer((ao) ),*block,array);
}
PETSC_EXTERN void PETSC_STDCALL  aosetfromoptions_(AO ao, int *__ierr){
*__ierr = AOSetFromOptions(
	(AO)PetscToPointer((ao) ));
}
PETSC_EXTERN void PETSC_STDCALL  aosetis_(AO ao,IS isapp,IS ispetsc, int *__ierr){
*__ierr = AOSetIS(
	(AO)PetscToPointer((ao) ),
	(IS)PetscToPointer((isapp) ),
	(IS)PetscToPointer((ispetsc) ));
}
PETSC_EXTERN void PETSC_STDCALL  aocreate_(MPI_Fint * comm,AO *ao, int *__ierr){
*__ierr = AOCreate(
	MPI_Comm_f2c(*(comm)),ao);
}
#if defined(__cplusplus)
}
#endif
