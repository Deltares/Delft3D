#if !defined(PETSCPETSCPCDEF_H)
#define PETSCPETSCPCDEF_H

#include "petsc/finclude/petscmat.h"
#include "petsc/finclude/petscdm.h"

#define PCSide type(ePCSide)
#define PCRichardsonConvergedReason type(ePCRichardsonConvergedReason)
#define PCJacobiType type(ePCJacobiType)
#define PCASMType type(ePCASMType)
#define PCGASMType type(ePCGASMType)
#define PCCompositeType type(ePCCompositeType)
#define PCFieldSplitSchurPreType type(ePCFieldSplitSchurPreType)
#define PCFieldSplitSchurFactType type(ePCFieldSplitSchurFactType)
#define PCPARMSGlobalType type(ePCPARMSGlobalType)
#define PCPARMSLocalType type(ePCPARMSLocalType)
#define PCMGType type(ePCMGType)
#define PCMGCycleType type(ePCMGCycleType)
#define PCMGGalerkinType type(ePCMGGalerkinType)
#define PCExoticType type(ePCExoticType)
#define PCBDDCInterfaceExtType type(ePCBDDCInterfaceExtType)
#define PCMGCoarseSpaceType type(ePCMGCoarseSpaceType)
#define PCPatchConstructType type(ePCPatchConstructType)
#define PCDeflationSpaceType type(ePCDeflationSpaceType)
#define PCHPDDMCoarseCorrectionType type(ePCHPDDMCoarseCorrectionType)
#define PCHPDDMSchurPreType type(ePCHPDDMSchurPreType)
#define PCFailedReason type(ePCFailedReason)
#define PCGAMGLayoutType type(ePCGAMGLayoutType)

#define PCType CHARACTER(80)
#define PCGAMGType CHARACTER(80)
#define PCGAMGClassicalType CHARACTER(80)

#define PC type(tPC)

#endif
