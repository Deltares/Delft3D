#if !defined(PETSCPETSCDTDEF_H)
#define PETSCPETSCDTDEF_H

#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscdm.h"
#include "petsc/finclude/petscis.h"
#include "petsc/finclude/petscvec.h"

#define PetscGaussLobattoLegendreCreateType type(ePetscGaussLobattoLegendreCreateType)
#define PetscDTNodeType type(ePetscDTNodeType)
#define PetscDTSimplexQuadratureType type(ePetscDTSimplexQuadratureType)
#define DTProbDensityType type(eDTProbDensityType)


#define PetscQuadrature type(tPetscQuadrature)
#define PetscTabulation type(tPetscTabulation)

#endif
