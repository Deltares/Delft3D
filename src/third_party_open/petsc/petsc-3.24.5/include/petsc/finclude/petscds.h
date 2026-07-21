#if !defined(PETSCPETSCDSDEF_H)
#define PETSCPETSCDSDEF_H

#include "petsc/finclude/petscfe.h"
#include "petsc/finclude/petscfv.h"

#define PetscDiscType type(ePetscDiscType)
#define PetscWeakFormKind type(ePetscWeakFormKind)
#define PetscFormKey type(sPetscFormKey)

#define PetscDSType CHARACTER(80)

#define PetscDS type(tPetscDS)
#define PetscWeakForm type(tPetscWeakForm)

#endif
