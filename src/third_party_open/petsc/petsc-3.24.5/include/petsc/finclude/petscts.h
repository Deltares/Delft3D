#if !defined(PETSCPETSCTSDEF_H)
#define PETSCPETSCTSDEF_H

#include "petsc/finclude/petscsnes.h"
#include "petsc/finclude/petscconvest.h"

#define TSProblemType type(eTSProblemType)
#define TSEquationType type(eTSEquationType)
#define TSConvergedReason type(eTSConvergedReason)
#define TSExactFinalTimeOption type(eTSExactFinalTimeOption)
#define TSTrajectoryMemoryType type(eTSTrajectoryMemoryType)
#define TSDGType type(eTSDGType)
#define TSSundialsLmmType type(eTSSundialsLmmType)
#define TSSundialsGramSchmidtType type(eTSSundialsGramSchmidtType)
#define TSMonitorDMDARayCtx type(sTSMonitorDMDARayCtx)

#define TSType CHARACTER(80)
#define TSTrajectoryType CHARACTER(80)
#define TSSSPType CHARACTER(80)
#define TSAdaptType CHARACTER(80)
#define TSGLLEAdaptType CHARACTER(80)
#define TSGLLEAcceptType CHARACTER(80)
#define TSGLLEType CHARACTER(80)
#define TSRKType CHARACTER(80)
#define TSMPRKType CHARACTER(80)
#define TSIRKType CHARACTER(80)
#define TSGLEEType CHARACTER(80)
#define TSARKIMEXType CHARACTER(80)
#define TSDIRKType CHARACTER(80)
#define TSRosWType CHARACTER(80)
#define TSBasicSymplecticType CHARACTER(80)

#define TS type(tTS)
#define TSTrajectory type(tTSTrajectory)
#define TSMonitorDrawCtx type(tTSMonitorDrawCtx)
#define TSMonitorSolutionCtx type(tTSMonitorSolutionCtx)
#define TSMonitorVTKCtx type(tTSMonitorVTKCtx)
#define TSMonitorLGCtx type(tTSMonitorLGCtx)
#define TSMonitorLGCtxNetwork type(tTSMonitorLGCtxNetwork)
#define TSMonitorEnvelopeCtx type(tTSMonitorEnvelopeCtx)
#define TSMonitorSPEigCtx type(tTSMonitorSPEigCtx)
#define TSMonitorSPCtx type(tTSMonitorSPCtx)
#define TSMonitorHGCtx type(tTSMonitorHGCtx)
#define TSAdapt type(tTSAdapt)
#define TSGLLEAdapt type(tTSGLLEAdapt)

#endif
