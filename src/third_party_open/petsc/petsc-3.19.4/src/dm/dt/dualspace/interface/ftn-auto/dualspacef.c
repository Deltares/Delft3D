#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dualspace.c */
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
#define petscdualspaceview_ PETSCDUALSPACEVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspaceview_ petscdualspaceview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesetfromoptions_ PETSCDUALSPACESETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesetfromoptions_ petscdualspacesetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesetup_ PETSCDUALSPACESETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesetup_ petscdualspacesetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacedestroy_ PETSCDUALSPACEDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacedestroy_ petscdualspacedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacecreate_ PETSCDUALSPACECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacecreate_ petscdualspacecreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspaceduplicate_ PETSCDUALSPACEDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspaceduplicate_ petscdualspaceduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetdm_ PETSCDUALSPACEGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetdm_ petscdualspacegetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesetdm_ PETSCDUALSPACESETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesetdm_ petscdualspacesetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetorder_ PETSCDUALSPACEGETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetorder_ petscdualspacegetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesetorder_ PETSCDUALSPACESETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesetorder_ petscdualspacesetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetnumcomponents_ PETSCDUALSPACEGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetnumcomponents_ petscdualspacegetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesetnumcomponents_ PETSCDUALSPACESETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesetnumcomponents_ petscdualspacesetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetfunctional_ PETSCDUALSPACEGETFUNCTIONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetfunctional_ petscdualspacegetfunctional
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetdimension_ PETSCDUALSPACEGETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetdimension_ petscdualspacegetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetinteriordimension_ PETSCDUALSPACEGETINTERIORDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetinteriordimension_ petscdualspacegetinteriordimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetuniform_ PETSCDUALSPACEGETUNIFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetuniform_ petscdualspacegetuniform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetsection_ PETSCDUALSPACEGETSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetsection_ petscdualspacegetsection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetalldata_ PETSCDUALSPACEGETALLDATA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetalldata_ petscdualspacegetalldata
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacecreatealldatadefault_ PETSCDUALSPACECREATEALLDATADEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacecreatealldatadefault_ petscdualspacecreatealldatadefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetinteriordata_ PETSCDUALSPACEGETINTERIORDATA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetinteriordata_ petscdualspacegetinteriordata
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacecreateinteriordatadefault_ PETSCDUALSPACECREATEINTERIORDATADEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacecreateinteriordatadefault_ petscdualspacecreateinteriordatadefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspaceequal_ PETSCDUALSPACEEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspaceequal_ petscdualspaceequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetheightsubspace_ PETSCDUALSPACEGETHEIGHTSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetheightsubspace_ petscdualspacegetheightsubspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetpointsubspace_ PETSCDUALSPACEGETPOINTSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetpointsubspace_ petscdualspacegetpointsubspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetformdegree_ PETSCDUALSPACEGETFORMDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetformdegree_ petscdualspacegetformdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesetformdegree_ PETSCDUALSPACESETFORMDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesetformdegree_ petscdualspacesetformdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacegetderahm_ PETSCDUALSPACEGETDERAHM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacegetderahm_ petscdualspacegetderahm
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdualspaceview_(PetscDualSpace sp,PetscViewer v, int *__ierr)
{
*__ierr = PetscDualSpaceView(
	(PetscDualSpace)PetscToPointer((sp) ),
	(PetscViewer)PetscToPointer((v) ));
}
PETSC_EXTERN void  petscdualspacesetfromoptions_(PetscDualSpace sp, int *__ierr)
{
*__ierr = PetscDualSpaceSetFromOptions(
	(PetscDualSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void  petscdualspacesetup_(PetscDualSpace sp, int *__ierr)
{
*__ierr = PetscDualSpaceSetUp(
	(PetscDualSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void  petscdualspacedestroy_(PetscDualSpace *sp, int *__ierr)
{
*__ierr = PetscDualSpaceDestroy(sp);
}
PETSC_EXTERN void  petscdualspacecreate_(MPI_Fint * comm,PetscDualSpace *sp, int *__ierr)
{
*__ierr = PetscDualSpaceCreate(
	MPI_Comm_f2c(*(comm)),sp);
}
PETSC_EXTERN void  petscdualspaceduplicate_(PetscDualSpace sp,PetscDualSpace *spNew, int *__ierr)
{
*__ierr = PetscDualSpaceDuplicate(
	(PetscDualSpace)PetscToPointer((sp) ),spNew);
}
PETSC_EXTERN void  petscdualspacegetdm_(PetscDualSpace sp,DM *dm, int *__ierr)
{
*__ierr = PetscDualSpaceGetDM(
	(PetscDualSpace)PetscToPointer((sp) ),dm);
}
PETSC_EXTERN void  petscdualspacesetdm_(PetscDualSpace sp,DM dm, int *__ierr)
{
*__ierr = PetscDualSpaceSetDM(
	(PetscDualSpace)PetscToPointer((sp) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  petscdualspacegetorder_(PetscDualSpace sp,PetscInt *order, int *__ierr)
{
*__ierr = PetscDualSpaceGetOrder(
	(PetscDualSpace)PetscToPointer((sp) ),order);
}
PETSC_EXTERN void  petscdualspacesetorder_(PetscDualSpace sp,PetscInt *order, int *__ierr)
{
*__ierr = PetscDualSpaceSetOrder(
	(PetscDualSpace)PetscToPointer((sp) ),*order);
}
PETSC_EXTERN void  petscdualspacegetnumcomponents_(PetscDualSpace sp,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscDualSpaceGetNumComponents(
	(PetscDualSpace)PetscToPointer((sp) ),Nc);
}
PETSC_EXTERN void  petscdualspacesetnumcomponents_(PetscDualSpace sp,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscDualSpaceSetNumComponents(
	(PetscDualSpace)PetscToPointer((sp) ),*Nc);
}
PETSC_EXTERN void  petscdualspacegetfunctional_(PetscDualSpace sp,PetscInt *i,PetscQuadrature *functional, int *__ierr)
{
*__ierr = PetscDualSpaceGetFunctional(
	(PetscDualSpace)PetscToPointer((sp) ),*i,functional);
}
PETSC_EXTERN void  petscdualspacegetdimension_(PetscDualSpace sp,PetscInt *dim, int *__ierr)
{
*__ierr = PetscDualSpaceGetDimension(
	(PetscDualSpace)PetscToPointer((sp) ),dim);
}
PETSC_EXTERN void  petscdualspacegetinteriordimension_(PetscDualSpace sp,PetscInt *intdim, int *__ierr)
{
*__ierr = PetscDualSpaceGetInteriorDimension(
	(PetscDualSpace)PetscToPointer((sp) ),intdim);
}
PETSC_EXTERN void  petscdualspacegetuniform_(PetscDualSpace sp,PetscBool *uniform, int *__ierr)
{
*__ierr = PetscDualSpaceGetUniform(
	(PetscDualSpace)PetscToPointer((sp) ),uniform);
}
PETSC_EXTERN void  petscdualspacegetsection_(PetscDualSpace sp,PetscSection *section, int *__ierr)
{
*__ierr = PetscDualSpaceGetSection(
	(PetscDualSpace)PetscToPointer((sp) ),section);
}
PETSC_EXTERN void  petscdualspacegetalldata_(PetscDualSpace sp,PetscQuadrature *allNodes,Mat *allMat, int *__ierr)
{
*__ierr = PetscDualSpaceGetAllData(
	(PetscDualSpace)PetscToPointer((sp) ),allNodes,allMat);
}
PETSC_EXTERN void  petscdualspacecreatealldatadefault_(PetscDualSpace sp,PetscQuadrature *allNodes,Mat *allMat, int *__ierr)
{
*__ierr = PetscDualSpaceCreateAllDataDefault(
	(PetscDualSpace)PetscToPointer((sp) ),allNodes,allMat);
}
PETSC_EXTERN void  petscdualspacegetinteriordata_(PetscDualSpace sp,PetscQuadrature *intNodes,Mat *intMat, int *__ierr)
{
*__ierr = PetscDualSpaceGetInteriorData(
	(PetscDualSpace)PetscToPointer((sp) ),intNodes,intMat);
}
PETSC_EXTERN void  petscdualspacecreateinteriordatadefault_(PetscDualSpace sp,PetscQuadrature *intNodes,Mat *intMat, int *__ierr)
{
*__ierr = PetscDualSpaceCreateInteriorDataDefault(
	(PetscDualSpace)PetscToPointer((sp) ),intNodes,intMat);
}
PETSC_EXTERN void  petscdualspaceequal_(PetscDualSpace A,PetscDualSpace B,PetscBool *equal, int *__ierr)
{
*__ierr = PetscDualSpaceEqual(
	(PetscDualSpace)PetscToPointer((A) ),
	(PetscDualSpace)PetscToPointer((B) ),equal);
}
PETSC_EXTERN void  petscdualspacegetheightsubspace_(PetscDualSpace sp,PetscInt *height,PetscDualSpace *subsp, int *__ierr)
{
*__ierr = PetscDualSpaceGetHeightSubspace(
	(PetscDualSpace)PetscToPointer((sp) ),*height,subsp);
}
PETSC_EXTERN void  petscdualspacegetpointsubspace_(PetscDualSpace sp,PetscInt *point,PetscDualSpace *bdsp, int *__ierr)
{
*__ierr = PetscDualSpaceGetPointSubspace(
	(PetscDualSpace)PetscToPointer((sp) ),*point,bdsp);
}
PETSC_EXTERN void  petscdualspacegetformdegree_(PetscDualSpace dsp,PetscInt *k, int *__ierr)
{
*__ierr = PetscDualSpaceGetFormDegree(
	(PetscDualSpace)PetscToPointer((dsp) ),k);
}
PETSC_EXTERN void  petscdualspacesetformdegree_(PetscDualSpace dsp,PetscInt *k, int *__ierr)
{
*__ierr = PetscDualSpaceSetFormDegree(
	(PetscDualSpace)PetscToPointer((dsp) ),*k);
}
PETSC_EXTERN void  petscdualspacegetderahm_(PetscDualSpace dsp,PetscInt *k, int *__ierr)
{
*__ierr = PetscDualSpaceGetDeRahm(
	(PetscDualSpace)PetscToPointer((dsp) ),k);
}
#if defined(__cplusplus)
}
#endif
