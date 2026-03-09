#if !defined(PETSCPETSCTAODEF_H)
#define PETSCPETSCTAODEF_H

#include "petsc/finclude/petscsnes.h"
#include "petsc/finclude/petsctaolinesearch.h"
#include "petsc/finclude/petsctao_deprecations.h"

#define TaoSubsetType type(eTaoSubsetType)
#define TaoADMMUpdateType type(eTaoADMMUpdateType)
#define TaoADMMRegularizerType type(eTaoADMMRegularizerType)
#define TaoALMMType type(eTaoALMMType)
#define TaoBNCGType type(eTaoBNCGType)
#define TaoConvergedReason type(eTaoConvergedReason)
#define TaoBRGNRegularizationType type(eTaoBRGNRegularizationType)

#define TaoType CHARACTER(80)

#define Tao type(tTao)
#define TaoMonitorDrawCtx type(tTaoMonitorDrawCtx)

#endif
