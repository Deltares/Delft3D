#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* space.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesetfromoptions_ PETSCSPACESETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesetfromoptions_ petscspacesetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacedestroy_ PETSCSPACEDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacedestroy_ petscspacedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacecreate_ PETSCSPACECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacecreate_ petscspacecreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacegetdimension_ PETSCSPACEGETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacegetdimension_ petscspacegetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacegetdegree_ PETSCSPACEGETDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacegetdegree_ petscspacegetdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesetdegree_ PETSCSPACESETDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesetdegree_ petscspacesetdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacegetnumcomponents_ PETSCSPACEGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacegetnumcomponents_ petscspacegetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesetnumcomponents_ PETSCSPACESETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesetnumcomponents_ petscspacesetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesetnumvariables_ PETSCSPACESETNUMVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesetnumvariables_ petscspacesetnumvariables
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacegetnumvariables_ PETSCSPACEGETNUMVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacegetnumvariables_ petscspacegetnumvariables
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacegetheightsubspace_ PETSCSPACEGETHEIGHTSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacegetheightsubspace_ petscspacegetheightsubspace
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscspacesetfromoptions_(PetscSpace sp, int *__ierr)
{
*__ierr = PetscSpaceSetFromOptions(
	(PetscSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void  petscspacedestroy_(PetscSpace *sp, int *__ierr)
{
*__ierr = PetscSpaceDestroy(sp);
}
PETSC_EXTERN void  petscspacecreate_(MPI_Fint * comm,PetscSpace *sp, int *__ierr)
{
*__ierr = PetscSpaceCreate(
	MPI_Comm_f2c(*(comm)),sp);
}
PETSC_EXTERN void  petscspacegetdimension_(PetscSpace sp,PetscInt *dim, int *__ierr)
{
*__ierr = PetscSpaceGetDimension(
	(PetscSpace)PetscToPointer((sp) ),dim);
}
PETSC_EXTERN void  petscspacegetdegree_(PetscSpace sp,PetscInt *minDegree,PetscInt *maxDegree, int *__ierr)
{
*__ierr = PetscSpaceGetDegree(
	(PetscSpace)PetscToPointer((sp) ),minDegree,maxDegree);
}
PETSC_EXTERN void  petscspacesetdegree_(PetscSpace sp,PetscInt *degree,PetscInt *maxDegree, int *__ierr)
{
*__ierr = PetscSpaceSetDegree(
	(PetscSpace)PetscToPointer((sp) ),*degree,*maxDegree);
}
PETSC_EXTERN void  petscspacegetnumcomponents_(PetscSpace sp,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscSpaceGetNumComponents(
	(PetscSpace)PetscToPointer((sp) ),Nc);
}
PETSC_EXTERN void  petscspacesetnumcomponents_(PetscSpace sp,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscSpaceSetNumComponents(
	(PetscSpace)PetscToPointer((sp) ),*Nc);
}
PETSC_EXTERN void  petscspacesetnumvariables_(PetscSpace sp,PetscInt *n, int *__ierr)
{
*__ierr = PetscSpaceSetNumVariables(
	(PetscSpace)PetscToPointer((sp) ),*n);
}
PETSC_EXTERN void  petscspacegetnumvariables_(PetscSpace sp,PetscInt *n, int *__ierr)
{
*__ierr = PetscSpaceGetNumVariables(
	(PetscSpace)PetscToPointer((sp) ),n);
}
PETSC_EXTERN void  petscspacegetheightsubspace_(PetscSpace sp,PetscInt *height,PetscSpace *subsp, int *__ierr)
{
*__ierr = PetscSpaceGetHeightSubspace(
	(PetscSpace)PetscToPointer((sp) ),*height,subsp);
}
#if defined(__cplusplus)
}
#endif
