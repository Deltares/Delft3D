#if !defined(PETSCPETSCKSPDEF_H)
#define PETSCPETSCKSPDEF_H

#include "petsc/finclude/petscpc.h"
#include "petsc/finclude/petscds.h"

#define KSPChebyshevKind type(eKSPChebyshevKind)
#define KSPFCDTruncationType type(eKSPFCDTruncationType)
#define KSPHPDDMType type(eKSPHPDDMType)
#define KSPHPDDMPrecision type(eKSPHPDDMPrecision)
#define KSPGMRESCGSRefinementType type(eKSPGMRESCGSRefinementType)
#define KSPNormType type(eKSPNormType)
#define KSPConvergedReason type(eKSPConvergedReason)
#define KSPCGType type(eKSPCGType)
#define MatSchurComplementAinvType type(eMatSchurComplementAinvType)
#define MatLMVMMultAlgorithm type(eMatLMVMMultAlgorithm)
#define MatLMVMSymBroydenScaleType type(eMatLMVMSymBroydenScaleType)
#define MatLMVMDenseType type(eMatLMVMDenseType)

#define KSPType CHARACTER(80)
#define KSPGuessType CHARACTER(80)

#define KSP type(tKSP)
#define KSPGuess type(tKSPGuess)

#endif
