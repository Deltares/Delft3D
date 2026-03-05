#if !defined(PETSCPETSCSYSDEF_H)
#define PETSCPETSCSYSDEF_H

#include "petsc/finclude/petscsysbase.h"
#include "petsc/finclude/petscmacros.h"
#include "petsc/finclude/petscmath.h"
#include "petsc/finclude/petscerror.h"
#include "petsc/finclude/petscviewer.h"
#include "petsc/finclude/petscoptions.h"
#include "petsc/finclude/petsclog.h"
#include "petsc/finclude/petscstring.h"
#include "petsc/finclude/petscdevice.h"

#define PetscBool3 type(ePetscBool3)
#define PetscCopyMode type(ePetscCopyMode)
#define PetscDataType type(ePetscDataType)
#define PetscFileMode type(ePetscFileMode)
#define PetscDLMode type(ePetscDLMode)
#define PetscBinarySeekType type(ePetscBinarySeekType)
#define PetscBuildTwoSidedType type(ePetscBuildTwoSidedType)
#define InsertMode type(eInsertMode)
#define PetscSubcommType type(ePetscSubcommType)
#define PetscErrorCode integer4
#define PetscClassId integer4
#define PetscMPIInt integer4
#define PetscCount PetscInt64
#define PetscFloat PetscFortranFloat
#define PetscInt32 integer4
#define PetscCuBLASInt integer4
#define PetscHipBLASInt integer4
#define PetscExodusIIInt integer4
#define PetscExodusIIFloat PetscFortranFloat
#define PetscLogDouble PetscFortranDouble
#define PetscObjectId PetscInt64
#define PetscObjectState PetscInt64
#define PCMPIServerAddresses PetscFortranAddr

#define PetscRandomType CHARACTER(80)

#define PetscToken type(tPetscToken)
#define PetscObject type(tPetscObject)
#define PetscFunctionList type(tPetscFunctionList)
#define PetscObjectList type(tPetscObjectList)
#define PetscDLLibrary type(tPetscDLLibrary)
#define PetscContainer type(tPetscContainer)
#define PetscRandom type(tPetscRandom)
#define PetscSubcomm type(tPetscSubcomm)
#define PetscHeap type(tPetscHeap)
#define PetscShmComm type(tPetscShmComm)
#define PetscOmpCtrl type(tPetscOmpCtrl)
#define PetscSegBuffer type(tPetscSegBuffer)
#define PetscOptionsHelpPrinted type(tPetscOptionsHelpPrinted)
#define PetscNull type(tPetscNull)
#define PetscObjectCompose(a,b,c,z) PetscObjectComposeRaw(a%v,b,c%v,z)
#define PetscObjectQuery(a,b,c,z) PetscObjectQueryRaw(a%v,b,c%v,z)

#endif
