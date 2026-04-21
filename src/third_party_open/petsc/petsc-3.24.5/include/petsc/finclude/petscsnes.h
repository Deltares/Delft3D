#if !defined(PETSCPETSCSNESDEF_H)
#define PETSCPETSCSNESDEF_H

#include "petsc/finclude/petscksp.h"
#include "petsc/finclude/petscdm.h"
#include "petsc/finclude/petscfv.h"
#include "petsc/finclude/petscdmda.h"

#define SNESNewtonTRFallbackType type(eSNESNewtonTRFallbackType)
#define SNESNewtonTRQNType type(eSNESNewtonTRQNType)
#define SNESConvergedReason type(eSNESConvergedReason)
#define SNESNormSchedule type(eSNESNormSchedule)
#define SNESFunctionType type(eSNESFunctionType)
#define SNESLineSearchReason type(eSNESLineSearchReason)
#define SNESNGMRESRestartType type(eSNESNGMRESRestartType)
#define SNESNGMRESSelectType type(eSNESNGMRESSelectType)
#define SNESNCGType type(eSNESNCGType)
#define SNESQNScaleType type(eSNESQNScaleType)
#define SNESQNRestartType type(eSNESQNRestartType)
#define SNESQNType type(eSNESQNType)
#define SNESCompositeType type(eSNESCompositeType)
#define SNESFASType type(eSNESFASType)
#define SNESNewtonALCorrectionType type(eSNESNewtonALCorrectionType)

#define SNESType CHARACTER(80)
#define SNESLineSearchType CHARACTER(80)
#define SNESMSType CHARACTER(80)

#define SNESLineSearch type(tSNESLineSearch)
#define SNES type(tSNES)

#endif
