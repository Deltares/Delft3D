#if !defined(PETSCPETSCDMDEF_H)
#define PETSCPETSCDMDEF_H

#include "petsc/finclude/petscmat.h"
#include "petsc/finclude/petscdmlabel.h"
#include "petsc/finclude/petscfe.h"
#include "petsc/finclude/petscds.h"
#include "petsc/finclude/petscdt.h"

#define DMCopyLabelsMode type(eDMCopyLabelsMode)
#define DMBoundaryType type(eDMBoundaryType)
#define DMBoundaryConditionType type(eDMBoundaryConditionType)
#define DMPointLocationType type(eDMPointLocationType)
#define DMBlockingType type(eDMBlockingType)
#define DMAdaptationStrategy type(eDMAdaptationStrategy)
#define DMAdaptationCriterion type(eDMAdaptationCriterion)
#define DMAdaptFlag type(eDMAdaptFlag)
#define DMDirection type(eDMDirection)
#define DMEnclosureType type(eDMEnclosureType)
#define DMPolytopeType type(eDMPolytopeType)
#define PetscUnit type(ePetscUnit)
#define DMReorderDefaultFlag type(eDMReorderDefaultFlag)

#define DMType CHARACTER(80)

#define DMInterpolationInfo type(tDMInterpolationInfo)
#define DM type(tDM)
#define DMField type(tDMField)
#define DMUniversalLabel type(tDMUniversalLabel)
#define DMGeneratorFunctionList type(tDMGeneratorFunctionList)

#endif
