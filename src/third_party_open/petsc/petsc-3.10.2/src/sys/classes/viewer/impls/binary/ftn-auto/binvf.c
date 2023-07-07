#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* binv.c */
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

#include "petscviewer.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinaryskipinfo_ PETSCVIEWERBINARYSKIPINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinaryskipinfo_ petscviewerbinaryskipinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarysetskipinfo_ PETSCVIEWERBINARYSETSKIPINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarysetskipinfo_ petscviewerbinarysetskipinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarygetskipinfo_ PETSCVIEWERBINARYGETSKIPINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarygetskipinfo_ petscviewerbinarygetskipinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarysetskipoptions_ PETSCVIEWERBINARYSETSKIPOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarysetskipoptions_ petscviewerbinarysetskipoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarygetskipoptions_ PETSCVIEWERBINARYGETSKIPOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarygetskipoptions_ petscviewerbinarygetskipoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarysetskipheader_ PETSCVIEWERBINARYSETSKIPHEADER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarysetskipheader_ petscviewerbinarysetskipheader
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarygetskipheader_ PETSCVIEWERBINARYGETSKIPHEADER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarygetskipheader_ petscviewerbinarygetskipheader
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarysetusempiio_ PETSCVIEWERBINARYSETUSEMPIIO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarysetusempiio_ petscviewerbinarysetusempiio
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinaryskipinfo_(PetscViewer viewer, int *__ierr){
*__ierr = PetscViewerBinarySkipInfo(
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarysetskipinfo_(PetscViewer viewer,PetscBool *skip, int *__ierr){
*__ierr = PetscViewerBinarySetSkipInfo(
	(PetscViewer)PetscToPointer((viewer) ),*skip);
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarygetskipinfo_(PetscViewer viewer,PetscBool *skip, int *__ierr){
*__ierr = PetscViewerBinaryGetSkipInfo(
	(PetscViewer)PetscToPointer((viewer) ),skip);
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarysetskipoptions_(PetscViewer viewer,PetscBool *skip, int *__ierr){
*__ierr = PetscViewerBinarySetSkipOptions(
	(PetscViewer)PetscToPointer((viewer) ),*skip);
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarygetskipoptions_(PetscViewer viewer,PetscBool *skip, int *__ierr){
*__ierr = PetscViewerBinaryGetSkipOptions(
	(PetscViewer)PetscToPointer((viewer) ),skip);
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarysetskipheader_(PetscViewer viewer,PetscBool *skip, int *__ierr){
*__ierr = PetscViewerBinarySetSkipHeader(
	(PetscViewer)PetscToPointer((viewer) ),*skip);
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarygetskipheader_(PetscViewer viewer,PetscBool  *skip, int *__ierr){
*__ierr = PetscViewerBinaryGetSkipHeader(
	(PetscViewer)PetscToPointer((viewer) ),skip);
}
PETSC_EXTERN void PETSC_STDCALL  petscviewerbinarysetusempiio_(PetscViewer viewer,PetscBool *flg, int *__ierr){
*__ierr = PetscViewerBinarySetUseMPIIO(
	(PetscViewer)PetscToPointer((viewer) ),*flg);
}
#if defined(__cplusplus)
}
#endif
