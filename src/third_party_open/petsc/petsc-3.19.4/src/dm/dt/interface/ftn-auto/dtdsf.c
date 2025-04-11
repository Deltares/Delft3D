#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dtds.c */
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

#include "petscds.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetfromoptions_ PETSCDSSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetfromoptions_ petscdssetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsdestroy_ PETSCDSDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsdestroy_ petscdsdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscreate_ PETSCDSCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscreate_ petscdscreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetnumfields_ PETSCDSGETNUMFIELDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetnumfields_ petscdsgetnumfields
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetspatialdimension_ PETSCDSGETSPATIALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetspatialdimension_ petscdsgetspatialdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcoordinatedimension_ PETSCDSGETCOORDINATEDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcoordinatedimension_ petscdsgetcoordinatedimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetcoordinatedimension_ PETSCDSSETCOORDINATEDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetcoordinatedimension_ petscdssetcoordinatedimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetforcequad_ PETSCDSGETFORCEQUAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetforcequad_ petscdsgetforcequad
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetforcequad_ PETSCDSSETFORCEQUAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetforcequad_ petscdssetforcequad
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsiscohesive_ PETSCDSISCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsiscohesive_ petscdsiscohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetnumcohesive_ PETSCDSGETNUMCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetnumcohesive_ petscdsgetnumcohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcohesive_ PETSCDSGETCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcohesive_ petscdsgetcohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetcohesive_ PETSCDSSETCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetcohesive_ petscdssetcohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgettotaldimension_ PETSCDSGETTOTALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgettotaldimension_ petscdsgettotaldimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgettotalcomponents_ PETSCDSGETTOTALCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgettotalcomponents_ petscdsgettotalcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetdiscretization_ PETSCDSGETDISCRETIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetdiscretization_ petscdsgetdiscretization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetdiscretization_ PETSCDSSETDISCRETIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetdiscretization_ petscdssetdiscretization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetweakform_ PETSCDSGETWEAKFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetweakform_ petscdsgetweakform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetweakform_ PETSCDSSETWEAKFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetweakform_ petscdssetweakform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsadddiscretization_ PETSCDSADDDISCRETIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsadddiscretization_ petscdsadddiscretization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetquadrature_ PETSCDSGETQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetquadrature_ petscdsgetquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetimplicit_ PETSCDSGETIMPLICIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetimplicit_ petscdsgetimplicit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetimplicit_ PETSCDSSETIMPLICIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetimplicit_ petscdssetimplicit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetjetdegree_ PETSCDSGETJETDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetjetdegree_ petscdsgetjetdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetjetdegree_ PETSCDSSETJETDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetjetdegree_ petscdssetjetdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdshasbdjacobian_ PETSCDSHASBDJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdshasbdjacobian_ petscdshasbdjacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdshasbdjacobianpreconditioner_ PETSCDSHASBDJACOBIANPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdshasbdjacobianpreconditioner_ petscdshasbdjacobianpreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldindex_ PETSCDSGETFIELDINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldindex_ petscdsgetfieldindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldsize_ PETSCDSGETFIELDSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldsize_ petscdsgetfieldsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldoffset_ PETSCDSGETFIELDOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldoffset_ petscdsgetfieldoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldoffsetcohesive_ PETSCDSGETFIELDOFFSETCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldoffsetcohesive_ petscdsgetfieldoffsetcohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetdimensions_ PETSCDSGETDIMENSIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetdimensions_ petscdsgetdimensions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponents_ PETSCDSGETCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponents_ petscdsgetcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentoffset_ PETSCDSGETCOMPONENTOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentoffset_ petscdsgetcomponentoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentoffsets_ PETSCDSGETCOMPONENTOFFSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentoffsets_ petscdsgetcomponentoffsets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentderivativeoffsets_ PETSCDSGETCOMPONENTDERIVATIVEOFFSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentderivativeoffsets_ petscdsgetcomponentderivativeoffsets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentoffsetscohesive_ PETSCDSGETCOMPONENTOFFSETSCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentoffsetscohesive_ petscdsgetcomponentoffsetscohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentderivativeoffsetscohesive_ PETSCDSGETCOMPONENTDERIVATIVEOFFSETSCOHESIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentderivativeoffsetscohesive_ petscdsgetcomponentderivativeoffsetscohesive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetnumboundary_ PETSCDSGETNUMBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetnumboundary_ petscdsgetnumboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyboundary_ PETSCDSCOPYBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyboundary_ petscdscopyboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsdestroyboundary_ PETSCDSDESTROYBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsdestroyboundary_ petscdsdestroyboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsselectdiscretizations_ PETSCDSSELECTDISCRETIZATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsselectdiscretizations_ petscdsselectdiscretizations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsselectequations_ PETSCDSSELECTEQUATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsselectequations_ petscdsselectequations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyequations_ PETSCDSCOPYEQUATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyequations_ petscdscopyequations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyconstants_ PETSCDSCOPYCONSTANTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyconstants_ petscdscopyconstants
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyexactsolutions_ PETSCDSCOPYEXACTSOLUTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyexactsolutions_ petscdscopyexactsolutions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdssetfromoptions_(PetscDS prob, int *__ierr)
{
*__ierr = PetscDSSetFromOptions(
	(PetscDS)PetscToPointer((prob) ));
}
PETSC_EXTERN void  petscdsdestroy_(PetscDS *ds, int *__ierr)
{
*__ierr = PetscDSDestroy(ds);
}
PETSC_EXTERN void  petscdscreate_(MPI_Fint * comm,PetscDS *ds, int *__ierr)
{
*__ierr = PetscDSCreate(
	MPI_Comm_f2c(*(comm)),ds);
}
PETSC_EXTERN void  petscdsgetnumfields_(PetscDS prob,PetscInt *Nf, int *__ierr)
{
*__ierr = PetscDSGetNumFields(
	(PetscDS)PetscToPointer((prob) ),Nf);
}
PETSC_EXTERN void  petscdsgetspatialdimension_(PetscDS prob,PetscInt *dim, int *__ierr)
{
*__ierr = PetscDSGetSpatialDimension(
	(PetscDS)PetscToPointer((prob) ),dim);
}
PETSC_EXTERN void  petscdsgetcoordinatedimension_(PetscDS prob,PetscInt *dimEmbed, int *__ierr)
{
*__ierr = PetscDSGetCoordinateDimension(
	(PetscDS)PetscToPointer((prob) ),dimEmbed);
}
PETSC_EXTERN void  petscdssetcoordinatedimension_(PetscDS prob,PetscInt *dimEmbed, int *__ierr)
{
*__ierr = PetscDSSetCoordinateDimension(
	(PetscDS)PetscToPointer((prob) ),*dimEmbed);
}
PETSC_EXTERN void  petscdsgetforcequad_(PetscDS ds,PetscBool *forceQuad, int *__ierr)
{
*__ierr = PetscDSGetForceQuad(
	(PetscDS)PetscToPointer((ds) ),forceQuad);
}
PETSC_EXTERN void  petscdssetforcequad_(PetscDS ds,PetscBool *forceQuad, int *__ierr)
{
*__ierr = PetscDSSetForceQuad(
	(PetscDS)PetscToPointer((ds) ),*forceQuad);
}
PETSC_EXTERN void  petscdsiscohesive_(PetscDS ds,PetscBool *isCohesive, int *__ierr)
{
*__ierr = PetscDSIsCohesive(
	(PetscDS)PetscToPointer((ds) ),isCohesive);
}
PETSC_EXTERN void  petscdsgetnumcohesive_(PetscDS ds,PetscInt *numCohesive, int *__ierr)
{
*__ierr = PetscDSGetNumCohesive(
	(PetscDS)PetscToPointer((ds) ),numCohesive);
}
PETSC_EXTERN void  petscdsgetcohesive_(PetscDS ds,PetscInt *f,PetscBool *isCohesive, int *__ierr)
{
*__ierr = PetscDSGetCohesive(
	(PetscDS)PetscToPointer((ds) ),*f,isCohesive);
}
PETSC_EXTERN void  petscdssetcohesive_(PetscDS ds,PetscInt *f,PetscBool *isCohesive, int *__ierr)
{
*__ierr = PetscDSSetCohesive(
	(PetscDS)PetscToPointer((ds) ),*f,*isCohesive);
}
PETSC_EXTERN void  petscdsgettotaldimension_(PetscDS prob,PetscInt *dim, int *__ierr)
{
*__ierr = PetscDSGetTotalDimension(
	(PetscDS)PetscToPointer((prob) ),dim);
}
PETSC_EXTERN void  petscdsgettotalcomponents_(PetscDS prob,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscDSGetTotalComponents(
	(PetscDS)PetscToPointer((prob) ),Nc);
}
PETSC_EXTERN void  petscdsgetdiscretization_(PetscDS prob,PetscInt *f,PetscObject *disc, int *__ierr)
{
*__ierr = PetscDSGetDiscretization(
	(PetscDS)PetscToPointer((prob) ),*f,disc);
}
PETSC_EXTERN void  petscdssetdiscretization_(PetscDS prob,PetscInt *f,PetscObject *disc, int *__ierr)
{
*__ierr = PetscDSSetDiscretization(
	(PetscDS)PetscToPointer((prob) ),*f,*disc);
}
PETSC_EXTERN void  petscdsgetweakform_(PetscDS ds,PetscWeakForm *wf, int *__ierr)
{
*__ierr = PetscDSGetWeakForm(
	(PetscDS)PetscToPointer((ds) ),wf);
}
PETSC_EXTERN void  petscdssetweakform_(PetscDS ds,PetscWeakForm wf, int *__ierr)
{
*__ierr = PetscDSSetWeakForm(
	(PetscDS)PetscToPointer((ds) ),
	(PetscWeakForm)PetscToPointer((wf) ));
}
PETSC_EXTERN void  petscdsadddiscretization_(PetscDS prob,PetscObject *disc, int *__ierr)
{
*__ierr = PetscDSAddDiscretization(
	(PetscDS)PetscToPointer((prob) ),*disc);
}
PETSC_EXTERN void  petscdsgetquadrature_(PetscDS prob,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscDSGetQuadrature(
	(PetscDS)PetscToPointer((prob) ),q);
}
PETSC_EXTERN void  petscdsgetimplicit_(PetscDS prob,PetscInt *f,PetscBool *implicit, int *__ierr)
{
*__ierr = PetscDSGetImplicit(
	(PetscDS)PetscToPointer((prob) ),*f,implicit);
}
PETSC_EXTERN void  petscdssetimplicit_(PetscDS prob,PetscInt *f,PetscBool *implicit, int *__ierr)
{
*__ierr = PetscDSSetImplicit(
	(PetscDS)PetscToPointer((prob) ),*f,*implicit);
}
PETSC_EXTERN void  petscdsgetjetdegree_(PetscDS ds,PetscInt *f,PetscInt *k, int *__ierr)
{
*__ierr = PetscDSGetJetDegree(
	(PetscDS)PetscToPointer((ds) ),*f,k);
}
PETSC_EXTERN void  petscdssetjetdegree_(PetscDS ds,PetscInt *f,PetscInt *k, int *__ierr)
{
*__ierr = PetscDSSetJetDegree(
	(PetscDS)PetscToPointer((ds) ),*f,*k);
}
PETSC_EXTERN void  petscdshasbdjacobian_(PetscDS ds,PetscBool *hasBdJac, int *__ierr)
{
*__ierr = PetscDSHasBdJacobian(
	(PetscDS)PetscToPointer((ds) ),hasBdJac);
}
PETSC_EXTERN void  petscdshasbdjacobianpreconditioner_(PetscDS ds,PetscBool *hasBdJacPre, int *__ierr)
{
*__ierr = PetscDSHasBdJacobianPreconditioner(
	(PetscDS)PetscToPointer((ds) ),hasBdJacPre);
}
PETSC_EXTERN void  petscdsgetfieldindex_(PetscDS prob,PetscObject *disc,PetscInt *f, int *__ierr)
{
*__ierr = PetscDSGetFieldIndex(
	(PetscDS)PetscToPointer((prob) ),*disc,f);
}
PETSC_EXTERN void  petscdsgetfieldsize_(PetscDS prob,PetscInt *f,PetscInt *size, int *__ierr)
{
*__ierr = PetscDSGetFieldSize(
	(PetscDS)PetscToPointer((prob) ),*f,size);
}
PETSC_EXTERN void  petscdsgetfieldoffset_(PetscDS prob,PetscInt *f,PetscInt *off, int *__ierr)
{
*__ierr = PetscDSGetFieldOffset(
	(PetscDS)PetscToPointer((prob) ),*f,off);
}
PETSC_EXTERN void  petscdsgetfieldoffsetcohesive_(PetscDS ds,PetscInt *f,PetscInt *off, int *__ierr)
{
*__ierr = PetscDSGetFieldOffsetCohesive(
	(PetscDS)PetscToPointer((ds) ),*f,off);
}
PETSC_EXTERN void  petscdsgetdimensions_(PetscDS prob,PetscInt *dimensions[], int *__ierr)
{
*__ierr = PetscDSGetDimensions(
	(PetscDS)PetscToPointer((prob) ),dimensions);
}
PETSC_EXTERN void  petscdsgetcomponents_(PetscDS prob,PetscInt *components[], int *__ierr)
{
*__ierr = PetscDSGetComponents(
	(PetscDS)PetscToPointer((prob) ),components);
}
PETSC_EXTERN void  petscdsgetcomponentoffset_(PetscDS prob,PetscInt *f,PetscInt *off, int *__ierr)
{
*__ierr = PetscDSGetComponentOffset(
	(PetscDS)PetscToPointer((prob) ),*f,off);
}
PETSC_EXTERN void  petscdsgetcomponentoffsets_(PetscDS prob,PetscInt *offsets[], int *__ierr)
{
*__ierr = PetscDSGetComponentOffsets(
	(PetscDS)PetscToPointer((prob) ),offsets);
}
PETSC_EXTERN void  petscdsgetcomponentderivativeoffsets_(PetscDS prob,PetscInt *offsets[], int *__ierr)
{
*__ierr = PetscDSGetComponentDerivativeOffsets(
	(PetscDS)PetscToPointer((prob) ),offsets);
}
PETSC_EXTERN void  petscdsgetcomponentoffsetscohesive_(PetscDS ds,PetscInt *s,PetscInt *offsets[], int *__ierr)
{
*__ierr = PetscDSGetComponentOffsetsCohesive(
	(PetscDS)PetscToPointer((ds) ),*s,offsets);
}
PETSC_EXTERN void  petscdsgetcomponentderivativeoffsetscohesive_(PetscDS ds,PetscInt *s,PetscInt *offsets[], int *__ierr)
{
*__ierr = PetscDSGetComponentDerivativeOffsetsCohesive(
	(PetscDS)PetscToPointer((ds) ),*s,offsets);
}
PETSC_EXTERN void  petscdsgetnumboundary_(PetscDS ds,PetscInt *numBd, int *__ierr)
{
*__ierr = PetscDSGetNumBoundary(
	(PetscDS)PetscToPointer((ds) ),numBd);
}
PETSC_EXTERN void  petscdscopyboundary_(PetscDS ds,PetscInt *numFields, PetscInt fields[],PetscDS newds, int *__ierr)
{
*__ierr = PetscDSCopyBoundary(
	(PetscDS)PetscToPointer((ds) ),*numFields,fields,
	(PetscDS)PetscToPointer((newds) ));
}
PETSC_EXTERN void  petscdsdestroyboundary_(PetscDS ds, int *__ierr)
{
*__ierr = PetscDSDestroyBoundary(
	(PetscDS)PetscToPointer((ds) ));
}
PETSC_EXTERN void  petscdsselectdiscretizations_(PetscDS prob,PetscInt *numFields, PetscInt fields[],PetscDS newprob, int *__ierr)
{
*__ierr = PetscDSSelectDiscretizations(
	(PetscDS)PetscToPointer((prob) ),*numFields,fields,
	(PetscDS)PetscToPointer((newprob) ));
}
PETSC_EXTERN void  petscdsselectequations_(PetscDS prob,PetscInt *numFields, PetscInt fields[],PetscDS newprob, int *__ierr)
{
*__ierr = PetscDSSelectEquations(
	(PetscDS)PetscToPointer((prob) ),*numFields,fields,
	(PetscDS)PetscToPointer((newprob) ));
}
PETSC_EXTERN void  petscdscopyequations_(PetscDS prob,PetscDS newprob, int *__ierr)
{
*__ierr = PetscDSCopyEquations(
	(PetscDS)PetscToPointer((prob) ),
	(PetscDS)PetscToPointer((newprob) ));
}
PETSC_EXTERN void  petscdscopyconstants_(PetscDS prob,PetscDS newprob, int *__ierr)
{
*__ierr = PetscDSCopyConstants(
	(PetscDS)PetscToPointer((prob) ),
	(PetscDS)PetscToPointer((newprob) ));
}
PETSC_EXTERN void  petscdscopyexactsolutions_(PetscDS ds,PetscDS newds, int *__ierr)
{
*__ierr = PetscDSCopyExactSolutions(
	(PetscDS)PetscToPointer((ds) ),
	(PetscDS)PetscToPointer((newds) ));
}
#if defined(__cplusplus)
}
#endif
