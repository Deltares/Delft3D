#if !defined(PETSCPETSCDMSWARMDEF_H)
#define PETSCPETSCDMSWARMDEF_H

#include "petsc/finclude/petscdm.h"
#include "petsc/finclude/petscdt.h"

#define DMSwarmType type(eDMSwarmType)
#define DMSwarmMigrateType type(eDMSwarmMigrateType)
#define DMSwarmCollectType type(eDMSwarmCollectType)
#define DMSwarmRemapType type(eDMSwarmRemapType)
#define DMSwarmPICLayoutType type(eDMSwarmPICLayoutType)


#define DMSwarmDataField type(tDMSwarmDataField)
#define DMSwarmDataBucket type(tDMSwarmDataBucket)
#define DMSwarmSort type(tDMSwarmSort)
#define DMSwarmCellDM type(tDMSwarmCellDM)

#endif
