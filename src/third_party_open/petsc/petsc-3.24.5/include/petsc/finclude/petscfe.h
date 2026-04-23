#if !defined(PETSCPETSCFEDEF_H)
#define PETSCPETSCFEDEF_H

#include "petsc/finclude/petscdm.h"
#include "petsc/finclude/petscdt.h"
#include "petsc/finclude/petscds.h"
#include "petsc/finclude/petscspace.h"
#include "petsc/finclude/petscdualspace.h"

#define PetscFEJacobianType type(ePetscFEJacobianType)
#define PetscFEGeomMode type(ePetscFEGeomMode)
#define PetscFEGeom PetscFortranAddr

#define PetscFEType CHARACTER(80)

#define PetscFE type(tPetscFE)

#endif
