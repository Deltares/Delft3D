#if !defined(PETSCPETSCDMPLEXDEF_H)
#define PETSCPETSCDMPLEXDEF_H

#include "petsc/finclude/petscsection.h"
#include "petsc/finclude/petscpartitioner.h"
#include "petsc/finclude/petscdm.h"
#include "petsc/finclude/petscdt.h"
#include "petsc/finclude/petscfe.h"
#include "petsc/finclude/petscfv.h"
#include "petsc/finclude/petscds.h"
#include "petsc/finclude/petscsf.h"
#include "petsc/finclude/petscdmfield.h"
#include "petsc/finclude/petscviewer.h"

#define DMPlexInterpolatedFlag type(eDMPlexInterpolatedFlag)
#define DMPlexTPSType type(eDMPlexTPSType)
#define DMPlexShape type(eDMPlexShape)
#define DMPlexCoordMap type(eDMPlexCoordMap)
#define DMPlexCSRAlgorithm type(eDMPlexCSRAlgorithm)
#define JacActionCtx PetscFortranAddr


#define PetscGridHash type(tPetscGridHash)
#define DMPlexStorageVersion type(tDMPlexStorageVersion)
#define DMPlexPointQueue type(tDMPlexPointQueue)

#endif
