#if !defined(PETSCPETSCDRAWDEF_H)
#define PETSCPETSCDRAWDEF_H

#include "petsc/finclude/petscsys.h"

#define PetscDrawMarkerType type(ePetscDrawMarkerType)
#define PetscDrawButton type(ePetscDrawButton)
#define PetscDrawViewPorts PetscFortranAddr

#define PetscDrawType CHARACTER(80)

#define PetscDraw type(tPetscDraw)
#define PetscDrawAxis type(tPetscDrawAxis)
#define PetscDrawLG type(tPetscDrawLG)
#define PetscDrawSP type(tPetscDrawSP)
#define PetscDrawHG type(tPetscDrawHG)
#define PetscDrawBar type(tPetscDrawBar)

#endif
