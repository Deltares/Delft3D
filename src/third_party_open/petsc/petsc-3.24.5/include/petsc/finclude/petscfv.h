#if !defined(PETSCPETSCFVDEF_H)
#define PETSCPETSCFVDEF_H

#include "petsc/finclude/petscdm.h"
#include "petsc/finclude/petscdt.h"
#include "petsc/finclude/petscspace.h"
#include "petsc/finclude/petscdualspace.h"
#include "petsc/finclude/petscds.h"

#define PetscFVFaceGeom PetscFortranAddr
#define PetscFVCellGeom type(sPetscFVCellGeom)

#define PetscLimiterType CHARACTER(80)
#define PetscFVType CHARACTER(80)

#define PetscLimiter type(tPetscLimiter)
#define PetscFV type(tPetscFV)

#endif
