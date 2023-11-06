#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexmetric.c */
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
#define dmplexmetricsetisotropic_ DMPLEXMETRICSETISOTROPIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetisotropic_ dmplexmetricsetisotropic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricisisotropic_ DMPLEXMETRICISISOTROPIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricisisotropic_ dmplexmetricisisotropic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetuniform_ DMPLEXMETRICSETUNIFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetuniform_ dmplexmetricsetuniform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricisuniform_ DMPLEXMETRICISUNIFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricisuniform_ dmplexmetricisuniform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetrestrictanisotropyfirst_ DMPLEXMETRICSETRESTRICTANISOTROPYFIRST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetrestrictanisotropyfirst_ dmplexmetricsetrestrictanisotropyfirst
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricrestrictanisotropyfirst_ DMPLEXMETRICRESTRICTANISOTROPYFIRST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricrestrictanisotropyfirst_ dmplexmetricrestrictanisotropyfirst
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetnoinsertion_ DMPLEXMETRICSETNOINSERTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetnoinsertion_ dmplexmetricsetnoinsertion
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricnoinsertion_ DMPLEXMETRICNOINSERTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricnoinsertion_ dmplexmetricnoinsertion
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetnoswapping_ DMPLEXMETRICSETNOSWAPPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetnoswapping_ dmplexmetricsetnoswapping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricnoswapping_ DMPLEXMETRICNOSWAPPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricnoswapping_ dmplexmetricnoswapping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetnomovement_ DMPLEXMETRICSETNOMOVEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetnomovement_ dmplexmetricsetnomovement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricnomovement_ DMPLEXMETRICNOMOVEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricnomovement_ dmplexmetricnomovement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetnosurf_ DMPLEXMETRICSETNOSURF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetnosurf_ dmplexmetricsetnosurf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricnosurf_ DMPLEXMETRICNOSURF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricnosurf_ dmplexmetricnosurf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetminimummagnitude_ DMPLEXMETRICSETMINIMUMMAGNITUDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetminimummagnitude_ dmplexmetricsetminimummagnitude
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetminimummagnitude_ DMPLEXMETRICGETMINIMUMMAGNITUDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetminimummagnitude_ dmplexmetricgetminimummagnitude
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetmaximummagnitude_ DMPLEXMETRICSETMAXIMUMMAGNITUDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetmaximummagnitude_ dmplexmetricsetmaximummagnitude
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetmaximummagnitude_ DMPLEXMETRICGETMAXIMUMMAGNITUDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetmaximummagnitude_ dmplexmetricgetmaximummagnitude
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetmaximumanisotropy_ DMPLEXMETRICSETMAXIMUMANISOTROPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetmaximumanisotropy_ dmplexmetricsetmaximumanisotropy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetmaximumanisotropy_ DMPLEXMETRICGETMAXIMUMANISOTROPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetmaximumanisotropy_ dmplexmetricgetmaximumanisotropy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsettargetcomplexity_ DMPLEXMETRICSETTARGETCOMPLEXITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsettargetcomplexity_ dmplexmetricsettargetcomplexity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgettargetcomplexity_ DMPLEXMETRICGETTARGETCOMPLEXITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgettargetcomplexity_ dmplexmetricgettargetcomplexity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetnormalizationorder_ DMPLEXMETRICSETNORMALIZATIONORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetnormalizationorder_ dmplexmetricsetnormalizationorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetnormalizationorder_ DMPLEXMETRICGETNORMALIZATIONORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetnormalizationorder_ dmplexmetricgetnormalizationorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetgradationfactor_ DMPLEXMETRICSETGRADATIONFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetgradationfactor_ dmplexmetricsetgradationfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetgradationfactor_ DMPLEXMETRICGETGRADATIONFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetgradationfactor_ dmplexmetricgetgradationfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsethausdorffnumber_ DMPLEXMETRICSETHAUSDORFFNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsethausdorffnumber_ dmplexmetricsethausdorffnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgethausdorffnumber_ DMPLEXMETRICGETHAUSDORFFNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgethausdorffnumber_ dmplexmetricgethausdorffnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetverbosity_ DMPLEXMETRICSETVERBOSITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetverbosity_ dmplexmetricsetverbosity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetverbosity_ DMPLEXMETRICGETVERBOSITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetverbosity_ dmplexmetricgetverbosity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricsetnumiterations_ DMPLEXMETRICSETNUMITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricsetnumiterations_ dmplexmetricsetnumiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricgetnumiterations_ DMPLEXMETRICGETNUMITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricgetnumiterations_ dmplexmetricgetnumiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetriccreate_ DMPLEXMETRICCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetriccreate_ dmplexmetriccreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetriccreateuniform_ DMPLEXMETRICCREATEUNIFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetriccreateuniform_ dmplexmetriccreateuniform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetriccreateisotropic_ DMPLEXMETRICCREATEISOTROPIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetriccreateisotropic_ dmplexmetriccreateisotropic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricdeterminantcreate_ DMPLEXMETRICDETERMINANTCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricdeterminantcreate_ dmplexmetricdeterminantcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricenforcespd_ DMPLEXMETRICENFORCESPD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricenforcespd_ dmplexmetricenforcespd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricnormalize_ DMPLEXMETRICNORMALIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricnormalize_ dmplexmetricnormalize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricaverage_ DMPLEXMETRICAVERAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricaverage_ dmplexmetricaverage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricaverage2_ DMPLEXMETRICAVERAGE2
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricaverage2_ dmplexmetricaverage2
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricaverage3_ DMPLEXMETRICAVERAGE3
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricaverage3_ dmplexmetricaverage3
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricintersection_ DMPLEXMETRICINTERSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricintersection_ dmplexmetricintersection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricintersection2_ DMPLEXMETRICINTERSECTION2
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricintersection2_ dmplexmetricintersection2
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmetricintersection3_ DMPLEXMETRICINTERSECTION3
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmetricintersection3_ dmplexmetricintersection3
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexmetricsetisotropic_(DM dm,PetscBool *isotropic, int *__ierr)
{
*__ierr = DMPlexMetricSetIsotropic(
	(DM)PetscToPointer((dm) ),*isotropic);
}
PETSC_EXTERN void  dmplexmetricisisotropic_(DM dm,PetscBool *isotropic, int *__ierr)
{
*__ierr = DMPlexMetricIsIsotropic(
	(DM)PetscToPointer((dm) ),isotropic);
}
PETSC_EXTERN void  dmplexmetricsetuniform_(DM dm,PetscBool *uniform, int *__ierr)
{
*__ierr = DMPlexMetricSetUniform(
	(DM)PetscToPointer((dm) ),*uniform);
}
PETSC_EXTERN void  dmplexmetricisuniform_(DM dm,PetscBool *uniform, int *__ierr)
{
*__ierr = DMPlexMetricIsUniform(
	(DM)PetscToPointer((dm) ),uniform);
}
PETSC_EXTERN void  dmplexmetricsetrestrictanisotropyfirst_(DM dm,PetscBool *restrictAnisotropyFirst, int *__ierr)
{
*__ierr = DMPlexMetricSetRestrictAnisotropyFirst(
	(DM)PetscToPointer((dm) ),*restrictAnisotropyFirst);
}
PETSC_EXTERN void  dmplexmetricrestrictanisotropyfirst_(DM dm,PetscBool *restrictAnisotropyFirst, int *__ierr)
{
*__ierr = DMPlexMetricRestrictAnisotropyFirst(
	(DM)PetscToPointer((dm) ),restrictAnisotropyFirst);
}
PETSC_EXTERN void  dmplexmetricsetnoinsertion_(DM dm,PetscBool *noInsert, int *__ierr)
{
*__ierr = DMPlexMetricSetNoInsertion(
	(DM)PetscToPointer((dm) ),*noInsert);
}
PETSC_EXTERN void  dmplexmetricnoinsertion_(DM dm,PetscBool *noInsert, int *__ierr)
{
*__ierr = DMPlexMetricNoInsertion(
	(DM)PetscToPointer((dm) ),noInsert);
}
PETSC_EXTERN void  dmplexmetricsetnoswapping_(DM dm,PetscBool *noSwap, int *__ierr)
{
*__ierr = DMPlexMetricSetNoSwapping(
	(DM)PetscToPointer((dm) ),*noSwap);
}
PETSC_EXTERN void  dmplexmetricnoswapping_(DM dm,PetscBool *noSwap, int *__ierr)
{
*__ierr = DMPlexMetricNoSwapping(
	(DM)PetscToPointer((dm) ),noSwap);
}
PETSC_EXTERN void  dmplexmetricsetnomovement_(DM dm,PetscBool *noMove, int *__ierr)
{
*__ierr = DMPlexMetricSetNoMovement(
	(DM)PetscToPointer((dm) ),*noMove);
}
PETSC_EXTERN void  dmplexmetricnomovement_(DM dm,PetscBool *noMove, int *__ierr)
{
*__ierr = DMPlexMetricNoMovement(
	(DM)PetscToPointer((dm) ),noMove);
}
PETSC_EXTERN void  dmplexmetricsetnosurf_(DM dm,PetscBool *noSurf, int *__ierr)
{
*__ierr = DMPlexMetricSetNoSurf(
	(DM)PetscToPointer((dm) ),*noSurf);
}
PETSC_EXTERN void  dmplexmetricnosurf_(DM dm,PetscBool *noSurf, int *__ierr)
{
*__ierr = DMPlexMetricNoSurf(
	(DM)PetscToPointer((dm) ),noSurf);
}
PETSC_EXTERN void  dmplexmetricsetminimummagnitude_(DM dm,PetscReal *h_min, int *__ierr)
{
*__ierr = DMPlexMetricSetMinimumMagnitude(
	(DM)PetscToPointer((dm) ),*h_min);
}
PETSC_EXTERN void  dmplexmetricgetminimummagnitude_(DM dm,PetscReal *h_min, int *__ierr)
{
*__ierr = DMPlexMetricGetMinimumMagnitude(
	(DM)PetscToPointer((dm) ),h_min);
}
PETSC_EXTERN void  dmplexmetricsetmaximummagnitude_(DM dm,PetscReal *h_max, int *__ierr)
{
*__ierr = DMPlexMetricSetMaximumMagnitude(
	(DM)PetscToPointer((dm) ),*h_max);
}
PETSC_EXTERN void  dmplexmetricgetmaximummagnitude_(DM dm,PetscReal *h_max, int *__ierr)
{
*__ierr = DMPlexMetricGetMaximumMagnitude(
	(DM)PetscToPointer((dm) ),h_max);
}
PETSC_EXTERN void  dmplexmetricsetmaximumanisotropy_(DM dm,PetscReal *a_max, int *__ierr)
{
*__ierr = DMPlexMetricSetMaximumAnisotropy(
	(DM)PetscToPointer((dm) ),*a_max);
}
PETSC_EXTERN void  dmplexmetricgetmaximumanisotropy_(DM dm,PetscReal *a_max, int *__ierr)
{
*__ierr = DMPlexMetricGetMaximumAnisotropy(
	(DM)PetscToPointer((dm) ),a_max);
}
PETSC_EXTERN void  dmplexmetricsettargetcomplexity_(DM dm,PetscReal *targetComplexity, int *__ierr)
{
*__ierr = DMPlexMetricSetTargetComplexity(
	(DM)PetscToPointer((dm) ),*targetComplexity);
}
PETSC_EXTERN void  dmplexmetricgettargetcomplexity_(DM dm,PetscReal *targetComplexity, int *__ierr)
{
*__ierr = DMPlexMetricGetTargetComplexity(
	(DM)PetscToPointer((dm) ),targetComplexity);
}
PETSC_EXTERN void  dmplexmetricsetnormalizationorder_(DM dm,PetscReal *p, int *__ierr)
{
*__ierr = DMPlexMetricSetNormalizationOrder(
	(DM)PetscToPointer((dm) ),*p);
}
PETSC_EXTERN void  dmplexmetricgetnormalizationorder_(DM dm,PetscReal *p, int *__ierr)
{
*__ierr = DMPlexMetricGetNormalizationOrder(
	(DM)PetscToPointer((dm) ),p);
}
PETSC_EXTERN void  dmplexmetricsetgradationfactor_(DM dm,PetscReal *beta, int *__ierr)
{
*__ierr = DMPlexMetricSetGradationFactor(
	(DM)PetscToPointer((dm) ),*beta);
}
PETSC_EXTERN void  dmplexmetricgetgradationfactor_(DM dm,PetscReal *beta, int *__ierr)
{
*__ierr = DMPlexMetricGetGradationFactor(
	(DM)PetscToPointer((dm) ),beta);
}
PETSC_EXTERN void  dmplexmetricsethausdorffnumber_(DM dm,PetscReal *hausd, int *__ierr)
{
*__ierr = DMPlexMetricSetHausdorffNumber(
	(DM)PetscToPointer((dm) ),*hausd);
}
PETSC_EXTERN void  dmplexmetricgethausdorffnumber_(DM dm,PetscReal *hausd, int *__ierr)
{
*__ierr = DMPlexMetricGetHausdorffNumber(
	(DM)PetscToPointer((dm) ),hausd);
}
PETSC_EXTERN void  dmplexmetricsetverbosity_(DM dm,PetscInt *verbosity, int *__ierr)
{
*__ierr = DMPlexMetricSetVerbosity(
	(DM)PetscToPointer((dm) ),*verbosity);
}
PETSC_EXTERN void  dmplexmetricgetverbosity_(DM dm,PetscInt *verbosity, int *__ierr)
{
*__ierr = DMPlexMetricGetVerbosity(
	(DM)PetscToPointer((dm) ),verbosity);
}
PETSC_EXTERN void  dmplexmetricsetnumiterations_(DM dm,PetscInt *numIter, int *__ierr)
{
*__ierr = DMPlexMetricSetNumIterations(
	(DM)PetscToPointer((dm) ),*numIter);
}
PETSC_EXTERN void  dmplexmetricgetnumiterations_(DM dm,PetscInt *numIter, int *__ierr)
{
*__ierr = DMPlexMetricGetNumIterations(
	(DM)PetscToPointer((dm) ),numIter);
}
PETSC_EXTERN void  dmplexmetriccreate_(DM dm,PetscInt *f,Vec *metric, int *__ierr)
{
*__ierr = DMPlexMetricCreate(
	(DM)PetscToPointer((dm) ),*f,metric);
}
PETSC_EXTERN void  dmplexmetriccreateuniform_(DM dm,PetscInt *f,PetscReal *alpha,Vec *metric, int *__ierr)
{
*__ierr = DMPlexMetricCreateUniform(
	(DM)PetscToPointer((dm) ),*f,*alpha,metric);
}
PETSC_EXTERN void  dmplexmetriccreateisotropic_(DM dm,PetscInt *f,Vec indicator,Vec *metric, int *__ierr)
{
*__ierr = DMPlexMetricCreateIsotropic(
	(DM)PetscToPointer((dm) ),*f,
	(Vec)PetscToPointer((indicator) ),metric);
}
PETSC_EXTERN void  dmplexmetricdeterminantcreate_(DM dm,PetscInt *f,Vec *determinant,DM *dmDet, int *__ierr)
{
*__ierr = DMPlexMetricDeterminantCreate(
	(DM)PetscToPointer((dm) ),*f,determinant,dmDet);
}
PETSC_EXTERN void  dmplexmetricenforcespd_(DM dm,Vec metricIn,PetscBool *restrictSizes,PetscBool *restrictAnisotropy,Vec metricOut,Vec determinant, int *__ierr)
{
*__ierr = DMPlexMetricEnforceSPD(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((metricIn) ),*restrictSizes,*restrictAnisotropy,
	(Vec)PetscToPointer((metricOut) ),
	(Vec)PetscToPointer((determinant) ));
}
PETSC_EXTERN void  dmplexmetricnormalize_(DM dm,Vec metricIn,PetscBool *restrictSizes,PetscBool *restrictAnisotropy,Vec metricOut,Vec determinant, int *__ierr)
{
*__ierr = DMPlexMetricNormalize(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((metricIn) ),*restrictSizes,*restrictAnisotropy,
	(Vec)PetscToPointer((metricOut) ),
	(Vec)PetscToPointer((determinant) ));
}
PETSC_EXTERN void  dmplexmetricaverage_(DM dm,PetscInt *numMetrics,PetscReal weights[],Vec metrics[],Vec metricAvg, int *__ierr)
{
*__ierr = DMPlexMetricAverage(
	(DM)PetscToPointer((dm) ),*numMetrics,weights,metrics,
	(Vec)PetscToPointer((metricAvg) ));
}
PETSC_EXTERN void  dmplexmetricaverage2_(DM dm,Vec metric1,Vec metric2,Vec metricAvg, int *__ierr)
{
*__ierr = DMPlexMetricAverage2(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((metric1) ),
	(Vec)PetscToPointer((metric2) ),
	(Vec)PetscToPointer((metricAvg) ));
}
PETSC_EXTERN void  dmplexmetricaverage3_(DM dm,Vec metric1,Vec metric2,Vec metric3,Vec metricAvg, int *__ierr)
{
*__ierr = DMPlexMetricAverage3(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((metric1) ),
	(Vec)PetscToPointer((metric2) ),
	(Vec)PetscToPointer((metric3) ),
	(Vec)PetscToPointer((metricAvg) ));
}
PETSC_EXTERN void  dmplexmetricintersection_(DM dm,PetscInt *numMetrics,Vec metrics[],Vec metricInt, int *__ierr)
{
*__ierr = DMPlexMetricIntersection(
	(DM)PetscToPointer((dm) ),*numMetrics,metrics,
	(Vec)PetscToPointer((metricInt) ));
}
PETSC_EXTERN void  dmplexmetricintersection2_(DM dm,Vec metric1,Vec metric2,Vec metricInt, int *__ierr)
{
*__ierr = DMPlexMetricIntersection2(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((metric1) ),
	(Vec)PetscToPointer((metric2) ),
	(Vec)PetscToPointer((metricInt) ));
}
PETSC_EXTERN void  dmplexmetricintersection3_(DM dm,Vec metric1,Vec metric2,Vec metric3,Vec metricInt, int *__ierr)
{
*__ierr = DMPlexMetricIntersection3(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((metric1) ),
	(Vec)PetscToPointer((metric2) ),
	(Vec)PetscToPointer((metric3) ),
	(Vec)PetscToPointer((metricInt) ));
}
#if defined(__cplusplus)
}
#endif
