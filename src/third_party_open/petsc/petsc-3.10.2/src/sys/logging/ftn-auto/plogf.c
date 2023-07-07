#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plog.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogtracebegin_ PETSCLOGTRACEBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogtracebegin_ petsclogtracebegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogactions_ PETSCLOGACTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogactions_ petsclogactions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogobjects_ PETSCLOGOBJECTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogobjects_ petsclogobjects
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstagesetactive_ PETSCLOGSTAGESETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstagesetactive_ petsclogstagesetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstagegetactive_ PETSCLOGSTAGEGETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstagegetactive_ petsclogstagegetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstagesetvisible_ PETSCLOGSTAGESETVISIBLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstagesetvisible_ petsclogstagesetvisible
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstagegetvisible_ PETSCLOGSTAGEGETVISIBLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstagegetvisible_ petsclogstagegetvisible
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventsetcollective_ PETSCLOGEVENTSETCOLLECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventsetcollective_ petsclogeventsetcollective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventincludeclass_ PETSCLOGEVENTINCLUDECLASS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventincludeclass_ petsclogeventincludeclass
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventexcludeclass_ PETSCLOGEVENTEXCLUDECLASS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventexcludeclass_ petsclogeventexcludeclass
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventactivate_ PETSCLOGEVENTACTIVATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventactivate_ petsclogeventactivate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventdeactivate_ PETSCLOGEVENTDEACTIVATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventdeactivate_ petsclogeventdeactivate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventsetactiveall_ PETSCLOGEVENTSETACTIVEALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventsetactiveall_ petsclogeventsetactiveall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventactivateclass_ PETSCLOGEVENTACTIVATECLASS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventactivateclass_ petsclogeventactivateclass
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogeventdeactivateclass_ PETSCLOGEVENTDEACTIVATECLASS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogeventdeactivateclass_ petsclogeventdeactivateclass
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petsclogtracebegin_(FILE *file, int *__ierr){
FILE *_fp0 = stdout;
*__ierr = PetscLogTraceBegin(_fp0);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogactions_(PetscBool *flag, int *__ierr){
*__ierr = PetscLogActions(*flag);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogobjects_(PetscBool *flag, int *__ierr){
*__ierr = PetscLogObjects(*flag);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogstagesetactive_(PetscLogStage *stage,PetscBool *isActive, int *__ierr){
*__ierr = PetscLogStageSetActive(*stage,*isActive);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogstagegetactive_(PetscLogStage *stage,PetscBool  *isActive, int *__ierr){
*__ierr = PetscLogStageGetActive(*stage,isActive);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogstagesetvisible_(PetscLogStage *stage,PetscBool *isVisible, int *__ierr){
*__ierr = PetscLogStageSetVisible(*stage,*isVisible);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogstagegetvisible_(PetscLogStage *stage,PetscBool  *isVisible, int *__ierr){
*__ierr = PetscLogStageGetVisible(*stage,isVisible);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventsetcollective_(PetscLogEvent *event,PetscBool *collective, int *__ierr){
*__ierr = PetscLogEventSetCollective(*event,*collective);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventincludeclass_(PetscClassId *classid, int *__ierr){
*__ierr = PetscLogEventIncludeClass(*classid);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventexcludeclass_(PetscClassId *classid, int *__ierr){
*__ierr = PetscLogEventExcludeClass(*classid);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventactivate_(PetscLogEvent *event, int *__ierr){
*__ierr = PetscLogEventActivate(*event);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventdeactivate_(PetscLogEvent *event, int *__ierr){
*__ierr = PetscLogEventDeactivate(*event);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventsetactiveall_(PetscLogEvent *event,PetscBool *isActive, int *__ierr){
*__ierr = PetscLogEventSetActiveAll(*event,*isActive);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventactivateclass_(PetscClassId *classid, int *__ierr){
*__ierr = PetscLogEventActivateClass(*classid);
}
PETSC_EXTERN void PETSC_STDCALL  petsclogeventdeactivateclass_(PetscClassId *classid, int *__ierr){
*__ierr = PetscLogEventDeactivateClass(*classid);
}
#if defined(__cplusplus)
}
#endif
