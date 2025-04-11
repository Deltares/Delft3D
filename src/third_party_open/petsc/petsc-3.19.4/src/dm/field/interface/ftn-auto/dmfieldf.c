#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmfield.c */
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

#include "petscdmfield.h"
#include "petscdmfield.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfielddestroy_ DMFIELDDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfielddestroy_ dmfielddestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldgetnumcomponents_ DMFIELDGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldgetnumcomponents_ dmfieldgetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldgetdm_ DMFIELDGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldgetdm_ dmfieldgetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldevaluate_ DMFIELDEVALUATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldevaluate_ dmfieldevaluate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldevaluatefe_ DMFIELDEVALUATEFE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldevaluatefe_ dmfieldevaluatefe
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldevaluatefv_ DMFIELDEVALUATEFV
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldevaluatefv_ dmfieldevaluatefv
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldgetdegree_ DMFIELDGETDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldgetdegree_ dmfieldgetdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmfieldcreatedefaultquadrature_ DMFIELDCREATEDEFAULTQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmfieldcreatedefaultquadrature_ dmfieldcreatedefaultquadrature
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmfielddestroy_(DMField *field, int *__ierr)
{
*__ierr = DMFieldDestroy(field);
}
PETSC_EXTERN void  dmfieldgetnumcomponents_(DMField field,PetscInt *nc, int *__ierr)
{
*__ierr = DMFieldGetNumComponents(
	(DMField)PetscToPointer((field) ),nc);
}
PETSC_EXTERN void  dmfieldgetdm_(DMField field,DM *dm, int *__ierr)
{
*__ierr = DMFieldGetDM(
	(DMField)PetscToPointer((field) ),dm);
}
PETSC_EXTERN void  dmfieldevaluate_(DMField field,Vec points,PetscDataType *datatype,void*B,void*D,void*H, int *__ierr)
{
*__ierr = DMFieldEvaluate(
	(DMField)PetscToPointer((field) ),
	(Vec)PetscToPointer((points) ),*datatype,B,D,H);
}
PETSC_EXTERN void  dmfieldevaluatefe_(DMField field,IS cellIS,PetscQuadrature points,PetscDataType *datatype,void*B,void*D,void*H, int *__ierr)
{
*__ierr = DMFieldEvaluateFE(
	(DMField)PetscToPointer((field) ),
	(IS)PetscToPointer((cellIS) ),
	(PetscQuadrature)PetscToPointer((points) ),*datatype,B,D,H);
}
PETSC_EXTERN void  dmfieldevaluatefv_(DMField field,IS cellIS,PetscDataType *datatype,void*B,void*D,void*H, int *__ierr)
{
*__ierr = DMFieldEvaluateFV(
	(DMField)PetscToPointer((field) ),
	(IS)PetscToPointer((cellIS) ),*datatype,B,D,H);
}
PETSC_EXTERN void  dmfieldgetdegree_(DMField field,IS cellIS,PetscInt *minDegree,PetscInt *maxDegree, int *__ierr)
{
*__ierr = DMFieldGetDegree(
	(DMField)PetscToPointer((field) ),
	(IS)PetscToPointer((cellIS) ),minDegree,maxDegree);
}
PETSC_EXTERN void  dmfieldcreatedefaultquadrature_(DMField field,IS pointIS,PetscQuadrature *quad, int *__ierr)
{
*__ierr = DMFieldCreateDefaultQuadrature(
	(DMField)PetscToPointer((field) ),
	(IS)PetscToPointer((pointIS) ),quad);
}
#if defined(__cplusplus)
}
#endif
