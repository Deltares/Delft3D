#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pbvec.c */
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

#include "petscvec.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veccreateghost_ VECCREATEGHOST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veccreateghost_ veccreateghost
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmpisetghost_ VECMPISETGHOST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmpisetghost_ vecmpisetghost
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veccreateghostblock_ VECCREATEGHOSTBLOCK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veccreateghostblock_ veccreateghostblock
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  veccreateghost_(MPI_Fint * comm,PetscInt *n,PetscInt *N,PetscInt *nghost, PetscInt ghosts[],Vec *vv, int *__ierr)
{
*__ierr = VecCreateGhost(
	MPI_Comm_f2c(*(comm)),*n,*N,*nghost,ghosts,vv);
}
PETSC_EXTERN void  vecmpisetghost_(Vec vv,PetscInt *nghost, PetscInt ghosts[], int *__ierr)
{
*__ierr = VecMPISetGhost(
	(Vec)PetscToPointer((vv) ),*nghost,ghosts);
}
PETSC_EXTERN void  veccreateghostblock_(MPI_Fint * comm,PetscInt *bs,PetscInt *n,PetscInt *N,PetscInt *nghost, PetscInt ghosts[],Vec *vv, int *__ierr)
{
*__ierr = VecCreateGhostBlock(
	MPI_Comm_f2c(*(comm)),*bs,*n,*N,*nghost,ghosts,vv);
}
#if defined(__cplusplus)
}
#endif
