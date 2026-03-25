#if !defined(PETSCPETSCLOGDEF_H)
#define PETSCPETSCLOGDEF_H

#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petsctime.h"
#include "petsc/finclude/petscbt.h"

#define PetscInfoCommFlag type(ePetscInfoCommFlag)
#define PetscLogEvent integer4
#define PetscLogStage integer4
#define PetscLogClass integer4
#define PetscEventPerfInfo PetscFortranAddr
#define PetscLogEventInfo PetscFortranAddr
#define PetscLogClassInfo PetscFortranAddr
#define PetscLogStageInfo PetscFortranAddr

#define PetscLogHandlerType CHARACTER(80)

#define PetscLogHandler type(tPetscLogHandler)
#define PetscLogRegistry type(tPetscLogRegistry)
#define PetscLogState type(tPetscLogState)

#endif
