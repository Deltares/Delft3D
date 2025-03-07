#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexrefine.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petscdmplex.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreateprocesssf_ DMPLEXCREATEPROCESSSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreateprocesssf_ dmplexcreateprocesssf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatecoarsepointis_ DMPLEXCREATECOARSEPOINTIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatecoarsepointis_ dmplexcreatecoarsepointis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetrefinementuniform_ DMPLEXSETREFINEMENTUNIFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetrefinementuniform_ dmplexsetrefinementuniform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetrefinementuniform_ DMPLEXGETREFINEMENTUNIFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetrefinementuniform_ dmplexgetrefinementuniform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetrefinementlimit_ DMPLEXSETREFINEMENTLIMIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetrefinementlimit_ dmplexsetrefinementlimit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetrefinementlimit_ DMPLEXGETREFINEMENTLIMIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetrefinementlimit_ dmplexgetrefinementlimit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetrefinementfunction_ DMPLEXSETREFINEMENTFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetrefinementfunction_ dmplexsetrefinementfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetrefinementfunction_ DMPLEXGETREFINEMENTFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetrefinementfunction_ dmplexgetrefinementfunction
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexcreateprocesssf_(DM dm,PetscSF sfPoint,IS *processRanks,PetscSF *sfProcess, int *__ierr)
{
*__ierr = DMPlexCreateProcessSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((sfPoint) ),processRanks,sfProcess);
}
PETSC_EXTERN void  dmplexcreatecoarsepointis_(DM dm,IS *fpointIS, int *__ierr)
{
*__ierr = DMPlexCreateCoarsePointIS(
	(DM)PetscToPointer((dm) ),fpointIS);
}
PETSC_EXTERN void  dmplexsetrefinementuniform_(DM dm,PetscBool *refinementUniform, int *__ierr)
{
*__ierr = DMPlexSetRefinementUniform(
	(DM)PetscToPointer((dm) ),*refinementUniform);
}
PETSC_EXTERN void  dmplexgetrefinementuniform_(DM dm,PetscBool *refinementUniform, int *__ierr)
{
*__ierr = DMPlexGetRefinementUniform(
	(DM)PetscToPointer((dm) ),refinementUniform);
}
PETSC_EXTERN void  dmplexsetrefinementlimit_(DM dm,PetscReal *refinementLimit, int *__ierr)
{
*__ierr = DMPlexSetRefinementLimit(
	(DM)PetscToPointer((dm) ),*refinementLimit);
}
PETSC_EXTERN void  dmplexgetrefinementlimit_(DM dm,PetscReal *refinementLimit, int *__ierr)
{
*__ierr = DMPlexGetRefinementLimit(
	(DM)PetscToPointer((dm) ),refinementLimit);
}
PETSC_EXTERN void  dmplexsetrefinementfunction_(DM dm,PetscErrorCode (*refinementFunc)(const PetscReal[], PetscReal *), int *__ierr)
{
*__ierr = DMPlexSetRefinementFunction(
	(DM)PetscToPointer((dm) ),refinementFunc);
}
PETSC_EXTERN void  dmplexgetrefinementfunction_(DM dm,PetscErrorCode (**refinementFunc)(const PetscReal[], PetscReal *), int *__ierr)
{
*__ierr = DMPlexGetRefinementFunction(
	(DM)PetscToPointer((dm) ),refinementFunc);
}
#if defined(__cplusplus)
}
#endif
