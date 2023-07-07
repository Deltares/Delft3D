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
#define petscdualspacelagrangegettensor_ PETSCDUALSPACELAGRANGEGETTENSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegettensor_ petscdualspacelagrangegettensor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesettensor_ PETSCDUALSPACELAGRANGESETTENSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesettensor_ petscdualspacelagrangesettensor
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
#define petscdualspacecreatereferencecell_ PETSCDUALSPACECREATEREFERENCECELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacecreatereferencecell_ petscdualspacecreatereferencecell
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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscdualspaceview_(PetscDualSpace sp,PetscViewer v, int *__ierr){
*__ierr = PetscDualSpaceView(
	(PetscDualSpace)PetscToPointer((sp) ),
	(PetscViewer)PetscToPointer((v) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacesetfromoptions_(PetscDualSpace sp, int *__ierr){
*__ierr = PetscDualSpaceSetFromOptions(
	(PetscDualSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacesetup_(PetscDualSpace sp, int *__ierr){
*__ierr = PetscDualSpaceSetUp(
	(PetscDualSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacedestroy_(PetscDualSpace *sp, int *__ierr){
*__ierr = PetscDualSpaceDestroy(sp);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacecreate_(MPI_Fint * comm,PetscDualSpace *sp, int *__ierr){
*__ierr = PetscDualSpaceCreate(
	MPI_Comm_f2c(*(comm)),sp);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspaceduplicate_(PetscDualSpace sp,PetscDualSpace *spNew, int *__ierr){
*__ierr = PetscDualSpaceDuplicate(
	(PetscDualSpace)PetscToPointer((sp) ),spNew);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetdm_(PetscDualSpace sp,DM *dm, int *__ierr){
*__ierr = PetscDualSpaceGetDM(
	(PetscDualSpace)PetscToPointer((sp) ),dm);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacesetdm_(PetscDualSpace sp,DM dm, int *__ierr){
*__ierr = PetscDualSpaceSetDM(
	(PetscDualSpace)PetscToPointer((sp) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetorder_(PetscDualSpace sp,PetscInt *order, int *__ierr){
*__ierr = PetscDualSpaceGetOrder(
	(PetscDualSpace)PetscToPointer((sp) ),order);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacesetorder_(PetscDualSpace sp,PetscInt *order, int *__ierr){
*__ierr = PetscDualSpaceSetOrder(
	(PetscDualSpace)PetscToPointer((sp) ),*order);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetnumcomponents_(PetscDualSpace sp,PetscInt *Nc, int *__ierr){
*__ierr = PetscDualSpaceGetNumComponents(
	(PetscDualSpace)PetscToPointer((sp) ),Nc);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacesetnumcomponents_(PetscDualSpace sp,PetscInt *Nc, int *__ierr){
*__ierr = PetscDualSpaceSetNumComponents(
	(PetscDualSpace)PetscToPointer((sp) ),*Nc);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacelagrangegettensor_(PetscDualSpace sp,PetscBool *tensor, int *__ierr){
*__ierr = PetscDualSpaceLagrangeGetTensor(
	(PetscDualSpace)PetscToPointer((sp) ),tensor);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacelagrangesettensor_(PetscDualSpace sp,PetscBool *tensor, int *__ierr){
*__ierr = PetscDualSpaceLagrangeSetTensor(
	(PetscDualSpace)PetscToPointer((sp) ),*tensor);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetfunctional_(PetscDualSpace sp,PetscInt *i,PetscQuadrature *functional, int *__ierr){
*__ierr = PetscDualSpaceGetFunctional(
	(PetscDualSpace)PetscToPointer((sp) ),*i,functional);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetdimension_(PetscDualSpace sp,PetscInt *dim, int *__ierr){
*__ierr = PetscDualSpaceGetDimension(
	(PetscDualSpace)PetscToPointer((sp) ),dim);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacecreatereferencecell_(PetscDualSpace sp,PetscInt *dim,PetscBool *simplex,DM *refdm, int *__ierr){
*__ierr = PetscDualSpaceCreateReferenceCell(
	(PetscDualSpace)PetscToPointer((sp) ),*dim,*simplex,refdm);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetheightsubspace_(PetscDualSpace sp,PetscInt *height,PetscDualSpace *subsp, int *__ierr){
*__ierr = PetscDualSpaceGetHeightSubspace(
	(PetscDualSpace)PetscToPointer((sp) ),*height,subsp);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacegetpointsubspace_(PetscDualSpace sp,PetscInt *point,PetscDualSpace *bdsp, int *__ierr){
*__ierr = PetscDualSpaceGetPointSubspace(
	(PetscDualSpace)PetscToPointer((sp) ),*point,bdsp);
}
#if defined(__cplusplus)
}
#endif
