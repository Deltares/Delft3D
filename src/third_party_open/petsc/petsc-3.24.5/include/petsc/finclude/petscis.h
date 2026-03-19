#if !defined(PETSCPETSCISDEF_H)
#define PETSCPETSCISDEF_H

#include "petsc/finclude/petscisbase.h"
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscsf.h"
#include "petsc/finclude/petscsection.h"

#define ISInfo type(eISInfo)
#define ISInfoType type(eISInfoType)
#define ISGlobalToLocalMappingMode type(eISGlobalToLocalMappingMode)
#define ISColoringType type(eISColoringType)

#define ISType CHARACTER(80)
#define ISLocalToGlobalMappingType CHARACTER(80)

#define PetscKDTree type(tPetscKDTree)
#define IS type(tIS)
#define ISLocalToGlobalMapping type(tISLocalToGlobalMapping)
#define ISColoring type(tISColoring)
#define PetscLayout type(tPetscLayout)

#endif
