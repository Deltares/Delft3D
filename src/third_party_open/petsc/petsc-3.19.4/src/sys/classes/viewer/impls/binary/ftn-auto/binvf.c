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
#define petscviewerbinarysetusempiio_ PETSCVIEWERBINARYSETUSEMPIIO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarysetusempiio_ petscviewerbinarysetusempiio
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarygetusempiio_ PETSCVIEWERBINARYGETUSEMPIIO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarygetusempiio_ petscviewerbinarygetusempiio
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarysetflowcontrol_ PETSCVIEWERBINARYSETFLOWCONTROL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarysetflowcontrol_ petscviewerbinarysetflowcontrol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerbinarygetflowcontrol_ PETSCVIEWERBINARYGETFLOWCONTROL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerbinarygetflowcontrol_ petscviewerbinarygetflowcontrol
#endif
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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewerbinarysetusempiio_(PetscViewer viewer,PetscBool *use, int *__ierr)
{
*__ierr = PetscViewerBinarySetUseMPIIO(
	(PetscViewer)PetscToPointer((viewer) ),*use);
}
PETSC_EXTERN void  petscviewerbinarygetusempiio_(PetscViewer viewer,PetscBool *use, int *__ierr)
{
*__ierr = PetscViewerBinaryGetUseMPIIO(
	(PetscViewer)PetscToPointer((viewer) ),use);
}
PETSC_EXTERN void  petscviewerbinarysetflowcontrol_(PetscViewer viewer,PetscInt *fc, int *__ierr)
{
*__ierr = PetscViewerBinarySetFlowControl(
	(PetscViewer)PetscToPointer((viewer) ),*fc);
}
PETSC_EXTERN void  petscviewerbinarygetflowcontrol_(PetscViewer viewer,PetscInt *fc, int *__ierr)
{
*__ierr = PetscViewerBinaryGetFlowControl(
	(PetscViewer)PetscToPointer((viewer) ),fc);
}
PETSC_EXTERN void  petscviewerbinaryskipinfo_(PetscViewer viewer, int *__ierr)
{
*__ierr = PetscViewerBinarySkipInfo(
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscviewerbinarysetskipinfo_(PetscViewer viewer,PetscBool *skip, int *__ierr)
{
*__ierr = PetscViewerBinarySetSkipInfo(
	(PetscViewer)PetscToPointer((viewer) ),*skip);
}
PETSC_EXTERN void  petscviewerbinarygetskipinfo_(PetscViewer viewer,PetscBool *skip, int *__ierr)
{
*__ierr = PetscViewerBinaryGetSkipInfo(
	(PetscViewer)PetscToPointer((viewer) ),skip);
}
PETSC_EXTERN void  petscviewerbinarysetskipoptions_(PetscViewer viewer,PetscBool *skip, int *__ierr)
{
*__ierr = PetscViewerBinarySetSkipOptions(
	(PetscViewer)PetscToPointer((viewer) ),*skip);
}
PETSC_EXTERN void  petscviewerbinarygetskipoptions_(PetscViewer viewer,PetscBool *skip, int *__ierr)
{
*__ierr = PetscViewerBinaryGetSkipOptions(
	(PetscViewer)PetscToPointer((viewer) ),skip);
}
PETSC_EXTERN void  petscviewerbinarysetskipheader_(PetscViewer viewer,PetscBool *skip, int *__ierr)
{
*__ierr = PetscViewerBinarySetSkipHeader(
	(PetscViewer)PetscToPointer((viewer) ),*skip);
}
PETSC_EXTERN void  petscviewerbinarygetskipheader_(PetscViewer viewer,PetscBool *skip, int *__ierr)
{
*__ierr = PetscViewerBinaryGetSkipHeader(
	(PetscViewer)PetscToPointer((viewer) ),skip);
}
#if defined(__cplusplus)
}
#endif
