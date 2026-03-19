#if !defined(PETSCPETSCVECDEF_H)
#define PETSCPETSCVECDEF_H

#include "petsc/finclude/petscvecbase.h"
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscsf.h"
#include "petsc/finclude/petscis.h"
#include "petsc/finclude/petscdevice.h"
#include "petsc/finclude/petscviewer.h"

#define ScatterMode type(eScatterMode)
#define NormType type(eNormType)
#define ReductionType type(eReductionType)
#define VecOption type(eVecOption)
#define VecOperation type(eVecOperation)
#define VecTaggerCDFMethod type(eVecTaggerCDFMethod)
#define VecTaggerBox type(sVecTaggerBox)

#define VecType CHARACTER(80)
#define VecTaggerType CHARACTER(80)

#define Vec type(tVec)
#define Vecs type(tVecs)
#define PetscViennaCLIndices type(tPetscViennaCLIndices)
#define VecTagger type(tVecTagger)

#endif
