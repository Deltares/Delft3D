#if !defined(PETSCPETSCDEVICEDEF_H)
#define PETSCPETSCDEVICEDEF_H

#include "petsc/finclude/petscviewer.h"

#define PetscMemType type(ePetscMemType)
#define PetscOffloadMask type(ePetscOffloadMask)
#define PetscDeviceInitType type(ePetscDeviceInitType)
#define PetscDeviceType type(ePetscDeviceType)
#define PetscDeviceAttribute type(ePetscDeviceAttribute)
#define PetscStreamType type(ePetscStreamType)
#define PetscDeviceContextJoinMode type(ePetscDeviceContextJoinMode)
#define PetscDeviceCopyMode type(ePetscDeviceCopyMode)
#define PetscMemoryAccessMode type(ePetscMemoryAccessMode)


#define PetscDevice type(tPetscDevice)
#define PetscDeviceContext type(tPetscDeviceContext)

#endif
