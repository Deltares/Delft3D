#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sfutils.c */
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

#include "petscsf.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetgraphlayout_ PETSCSFSETGRAPHLAYOUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetgraphlayout_ petscsfsetgraphlayout
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetgraphsection_ PETSCSFSETGRAPHSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetgraphsection_ petscsfsetgraphsection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfcreatebymatchingindices_ PETSCSFCREATEBYMATCHINGINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfcreatebymatchingindices_ petscsfcreatebymatchingindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfmerge_ PETSCSFMERGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfmerge_ petscsfmerge
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsfsetgraphlayout_(PetscSF sf,PetscLayout layout,PetscInt *nleaves,PetscInt *ilocal,PetscCopyMode *localmode, PetscInt *gremote, int *__ierr)
{
*__ierr = PetscSFSetGraphLayout(
	(PetscSF)PetscToPointer((sf) ),
	(PetscLayout)PetscToPointer((layout) ),*nleaves,ilocal,*localmode,gremote);
}
PETSC_EXTERN void  petscsfsetgraphsection_(PetscSF sf,PetscSection localSection,PetscSection globalSection, int *__ierr)
{
*__ierr = PetscSFSetGraphSection(
	(PetscSF)PetscToPointer((sf) ),
	(PetscSection)PetscToPointer((localSection) ),
	(PetscSection)PetscToPointer((globalSection) ));
}
PETSC_EXTERN void  petscsfcreatebymatchingindices_(PetscLayout layout,PetscInt *numRootIndices, PetscInt *rootIndices, PetscInt *rootLocalIndices,PetscInt *rootLocalOffset,PetscInt *numLeafIndices, PetscInt *leafIndices, PetscInt *leafLocalIndices,PetscInt *leafLocalOffset,PetscSF *sfA,PetscSF *sf, int *__ierr)
{
*__ierr = PetscSFCreateByMatchingIndices(
	(PetscLayout)PetscToPointer((layout) ),*numRootIndices,rootIndices,rootLocalIndices,*rootLocalOffset,*numLeafIndices,leafIndices,leafLocalIndices,*leafLocalOffset,sfA,sf);
}
PETSC_EXTERN void  petscsfmerge_(PetscSF sfa,PetscSF sfb,PetscSF *merged, int *__ierr)
{
*__ierr = PetscSFMerge(
	(PetscSF)PetscToPointer((sfa) ),
	(PetscSF)PetscToPointer((sfb) ),merged);
}
#if defined(__cplusplus)
}
#endif
