#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexpoint.c */
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

#include "petscdmplex.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpointlocal_ DMPLEXGETPOINTLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpointlocal_ dmplexgetpointlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointlocalread_ DMPLEXPOINTLOCALREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointlocalread_ dmplexpointlocalread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointlocalref_ DMPLEXPOINTLOCALREF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointlocalref_ dmplexpointlocalref
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpointlocalfield_ DMPLEXGETPOINTLOCALFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpointlocalfield_ dmplexgetpointlocalfield
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointlocalfieldread_ DMPLEXPOINTLOCALFIELDREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointlocalfieldread_ dmplexpointlocalfieldread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointlocalfieldref_ DMPLEXPOINTLOCALFIELDREF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointlocalfieldref_ dmplexpointlocalfieldref
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpointglobal_ DMPLEXGETPOINTGLOBAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpointglobal_ dmplexgetpointglobal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointglobalread_ DMPLEXPOINTGLOBALREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointglobalread_ dmplexpointglobalread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointglobalref_ DMPLEXPOINTGLOBALREF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointglobalref_ dmplexpointglobalref
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpointglobalfield_ DMPLEXGETPOINTGLOBALFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpointglobalfield_ dmplexgetpointglobalfield
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointglobalfieldread_ DMPLEXPOINTGLOBALFIELDREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointglobalfieldread_ dmplexpointglobalfieldread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpointglobalfieldref_ DMPLEXPOINTGLOBALFIELDREF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpointglobalfieldref_ dmplexpointglobalfieldref
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexgetpointlocal_(DM dm,PetscInt *point,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = DMPlexGetPointLocal(
	(DM)PetscToPointer((dm) ),*point,start,end);
}
PETSC_EXTERN void  dmplexpointlocalread_(DM dm,PetscInt *point, PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointLocalRead(
	(DM)PetscToPointer((dm) ),*point,array,ptr);
}
PETSC_EXTERN void  dmplexpointlocalref_(DM dm,PetscInt *point,PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointLocalRef(
	(DM)PetscToPointer((dm) ),*point,array,ptr);
}
PETSC_EXTERN void  dmplexgetpointlocalfield_(DM dm,PetscInt *point,PetscInt *field,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = DMPlexGetPointLocalField(
	(DM)PetscToPointer((dm) ),*point,*field,start,end);
}
PETSC_EXTERN void  dmplexpointlocalfieldread_(DM dm,PetscInt *point,PetscInt *field, PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointLocalFieldRead(
	(DM)PetscToPointer((dm) ),*point,*field,array,ptr);
}
PETSC_EXTERN void  dmplexpointlocalfieldref_(DM dm,PetscInt *point,PetscInt *field,PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointLocalFieldRef(
	(DM)PetscToPointer((dm) ),*point,*field,array,ptr);
}
PETSC_EXTERN void  dmplexgetpointglobal_(DM dm,PetscInt *point,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = DMPlexGetPointGlobal(
	(DM)PetscToPointer((dm) ),*point,start,end);
}
PETSC_EXTERN void  dmplexpointglobalread_(DM dm,PetscInt *point, PetscScalar *array, void*ptr, int *__ierr)
{
*__ierr = DMPlexPointGlobalRead(
	(DM)PetscToPointer((dm) ),*point,array,ptr);
}
PETSC_EXTERN void  dmplexpointglobalref_(DM dm,PetscInt *point,PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointGlobalRef(
	(DM)PetscToPointer((dm) ),*point,array,ptr);
}
PETSC_EXTERN void  dmplexgetpointglobalfield_(DM dm,PetscInt *point,PetscInt *field,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = DMPlexGetPointGlobalField(
	(DM)PetscToPointer((dm) ),*point,*field,start,end);
}
PETSC_EXTERN void  dmplexpointglobalfieldread_(DM dm,PetscInt *point,PetscInt *field, PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointGlobalFieldRead(
	(DM)PetscToPointer((dm) ),*point,*field,array,ptr);
}
PETSC_EXTERN void  dmplexpointglobalfieldref_(DM dm,PetscInt *point,PetscInt *field,PetscScalar *array,void*ptr, int *__ierr)
{
*__ierr = DMPlexPointGlobalFieldRef(
	(DM)PetscToPointer((dm) ),*point,*field,array,ptr);
}
#if defined(__cplusplus)
}
#endif
