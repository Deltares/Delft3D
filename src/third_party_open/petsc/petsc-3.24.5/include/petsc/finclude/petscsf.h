#if !defined(PETSCPETSCSFDEF_H)
#define PETSCPETSCSFDEF_H

#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"

#define PetscSFPattern type(ePetscSFPattern)
#define PetscSFWindowSyncType type(ePetscSFWindowSyncType)
#define PetscSFWindowFlavorType type(ePetscSFWindowFlavorType)
#define PetscSFDuplicateOption type(ePetscSFDuplicateOption)
#define PetscSFConcatenateRootMode type(ePetscSFConcatenateRootMode)
#define PetscSFDirection type(ePetscSFDirection)
#define PetscSFOperation type(ePetscSFOperation)
#define PetscSFBackend type(ePetscSFBackend)
#define VecScatter PetscSF
#define VecScatterType PetscSFType
#define PetscSFNode type(sPetscSFNode)

#define PetscSFType CHARACTER(80)

#define PetscSF type(tPetscSF)
#define PetscSFLink type(tPetscSFLink)

#endif
