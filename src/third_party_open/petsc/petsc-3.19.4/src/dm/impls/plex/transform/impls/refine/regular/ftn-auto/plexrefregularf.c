#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexrefregular.c */
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

#include "petscdmplextransform.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexrefineregulargetaffinefacetransforms_ DMPLEXREFINEREGULARGETAFFINEFACETRANSFORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexrefineregulargetaffinefacetransforms_ dmplexrefineregulargetaffinefacetransforms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexrefineregulargetaffinetransforms_ DMPLEXREFINEREGULARGETAFFINETRANSFORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexrefineregulargetaffinetransforms_ dmplexrefineregulargetaffinetransforms
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexrefineregulargetaffinefacetransforms_(DMPlexTransform tr,DMPolytopeType *ct,PetscInt *Nf,PetscReal *v0[],PetscReal *J[],PetscReal *invJ[],PetscReal *detJ[], int *__ierr)
{
*__ierr = DMPlexRefineRegularGetAffineFaceTransforms(
	(DMPlexTransform)PetscToPointer((tr) ),*ct,Nf,v0,J,invJ,detJ);
}
PETSC_EXTERN void  dmplexrefineregulargetaffinetransforms_(DMPlexTransform tr,DMPolytopeType *ct,PetscInt *Nc,PetscReal *v0[],PetscReal *J[],PetscReal *invJ[], int *__ierr)
{
*__ierr = DMPlexRefineRegularGetAffineTransforms(
	(DMPlexTransform)PetscToPointer((tr) ),*ct,Nc,v0,J,invJ);
}
#if defined(__cplusplus)
}
#endif
