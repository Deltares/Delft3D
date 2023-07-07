#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vsectionis.c */
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

#include "petscvec.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreate_ PETSCSECTIONCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreate_ petscsectioncreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncopy_ PETSCSECTIONCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncopy_ petscsectioncopy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionclone_ PETSCSECTIONCLONE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionclone_ petscsectionclone
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncompare_ PETSCSECTIONCOMPARE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncompare_ petscsectioncompare
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetnumfields_ PETSCSECTIONGETNUMFIELDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetnumfields_ petscsectiongetnumfields
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetnumfields_ PETSCSECTIONSETNUMFIELDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetnumfields_ petscsectionsetnumfields
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetfieldcomponents_ PETSCSECTIONGETFIELDCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfieldcomponents_ petscsectiongetfieldcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetfieldcomponents_ PETSCSECTIONSETFIELDCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetfieldcomponents_ petscsectionsetfieldcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetchart_ PETSCSECTIONGETCHART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetchart_ petscsectiongetchart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetchart_ PETSCSECTIONSETCHART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetchart_ petscsectionsetchart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetpermutation_ PETSCSECTIONGETPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetpermutation_ petscsectiongetpermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetpermutation_ PETSCSECTIONSETPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetpermutation_ petscsectionsetpermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetdof_ PETSCSECTIONGETDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetdof_ petscsectiongetdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetdof_ PETSCSECTIONSETDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetdof_ petscsectionsetdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionadddof_ PETSCSECTIONADDDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionadddof_ petscsectionadddof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetfielddof_ PETSCSECTIONGETFIELDDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfielddof_ petscsectiongetfielddof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetfielddof_ PETSCSECTIONSETFIELDDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetfielddof_ petscsectionsetfielddof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionaddfielddof_ PETSCSECTIONADDFIELDDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionaddfielddof_ petscsectionaddfielddof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetconstraintdof_ PETSCSECTIONGETCONSTRAINTDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetconstraintdof_ petscsectiongetconstraintdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetconstraintdof_ PETSCSECTIONSETCONSTRAINTDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetconstraintdof_ petscsectionsetconstraintdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionaddconstraintdof_ PETSCSECTIONADDCONSTRAINTDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionaddconstraintdof_ petscsectionaddconstraintdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetfieldconstraintdof_ PETSCSECTIONGETFIELDCONSTRAINTDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfieldconstraintdof_ petscsectiongetfieldconstraintdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetfieldconstraintdof_ PETSCSECTIONSETFIELDCONSTRAINTDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetfieldconstraintdof_ petscsectionsetfieldconstraintdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionaddfieldconstraintdof_ PETSCSECTIONADDFIELDCONSTRAINTDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionaddfieldconstraintdof_ petscsectionaddfieldconstraintdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetup_ PETSCSECTIONSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetup_ petscsectionsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetmaxdof_ PETSCSECTIONGETMAXDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetmaxdof_ petscsectiongetmaxdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetstoragesize_ PETSCSECTIONGETSTORAGESIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetstoragesize_ petscsectiongetstoragesize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetconstrainedstoragesize_ PETSCSECTIONGETCONSTRAINEDSTORAGESIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetconstrainedstoragesize_ petscsectiongetconstrainedstoragesize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreateglobalsection_ PETSCSECTIONCREATEGLOBALSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreateglobalsection_ petscsectioncreateglobalsection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreateglobalsectioncensored_ PETSCSECTIONCREATEGLOBALSECTIONCENSORED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreateglobalsectioncensored_ petscsectioncreateglobalsectioncensored
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetvaluelayout_ PETSCSECTIONGETVALUELAYOUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetvaluelayout_ petscsectiongetvaluelayout
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetoffset_ PETSCSECTIONGETOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetoffset_ petscsectiongetoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetoffset_ PETSCSECTIONSETOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetoffset_ petscsectionsetoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetfieldoffset_ PETSCSECTIONGETFIELDOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfieldoffset_ petscsectiongetfieldoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetfieldoffset_ PETSCSECTIONSETFIELDOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetfieldoffset_ petscsectionsetfieldoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetoffsetrange_ PETSCSECTIONGETOFFSETRANGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetoffsetrange_ petscsectiongetoffsetrange
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionreset_ PETSCSECTIONRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionreset_ petscsectionreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiondestroy_ PETSCSECTIONDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiondestroy_ petscsectiondestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionpermute_ PETSCSECTIONPERMUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionpermute_ petscsectionpermute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetclosureindex_ PETSCSECTIONSETCLOSUREINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetclosureindex_ petscsectionsetclosureindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetclosureindex_ PETSCSECTIONGETCLOSUREINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetclosureindex_ petscsectiongetclosureindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetclosurepermutation_ PETSCSECTIONSETCLOSUREPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetclosurepermutation_ petscsectionsetclosurepermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetclosurepermutation_ PETSCSECTIONGETCLOSUREPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetclosurepermutation_ petscsectiongetclosurepermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetclosureinversepermutation_ PETSCSECTIONGETCLOSUREINVERSEPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetclosureinversepermutation_ petscsectiongetclosureinversepermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetfield_ PETSCSECTIONGETFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfield_ petscsectiongetfield
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsymcreate_ PETSCSECTIONSYMCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsymcreate_ petscsectionsymcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsymdestroy_ PETSCSECTIONSYMDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsymdestroy_ petscsectionsymdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetsym_ PETSCSECTIONSETSYM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetsym_ petscsectionsetsym
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetsym_ PETSCSECTIONGETSYM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetsym_ petscsectiongetsym
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetfieldsym_ PETSCSECTIONSETFIELDSYM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetfieldsym_ petscsectionsetfieldsym
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetfieldsym_ PETSCSECTIONGETFIELDSYM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfieldsym_ petscsectiongetfieldsym
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetusefieldoffsets_ PETSCSECTIONGETUSEFIELDOFFSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetusefieldoffsets_ petscsectiongetusefieldoffsets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetusefieldoffsets_ PETSCSECTIONSETUSEFIELDOFFSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetusefieldoffsets_ petscsectionsetusefieldoffsets
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscsectioncreate_(MPI_Fint * comm,PetscSection *s, int *__ierr){
*__ierr = PetscSectionCreate(
	MPI_Comm_f2c(*(comm)),s);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectioncopy_(PetscSection section,PetscSection newSection, int *__ierr){
*__ierr = PetscSectionCopy(
	(PetscSection)PetscToPointer((section) ),
	(PetscSection)PetscToPointer((newSection) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionclone_(PetscSection section,PetscSection *newSection, int *__ierr){
*__ierr = PetscSectionClone(
	(PetscSection)PetscToPointer((section) ),newSection);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectioncompare_(PetscSection s1,PetscSection s2,PetscBool *congruent, int *__ierr){
*__ierr = PetscSectionCompare(
	(PetscSection)PetscToPointer((s1) ),
	(PetscSection)PetscToPointer((s2) ),congruent);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetnumfields_(PetscSection s,PetscInt *numFields, int *__ierr){
*__ierr = PetscSectionGetNumFields(
	(PetscSection)PetscToPointer((s) ),numFields);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetnumfields_(PetscSection s,PetscInt *numFields, int *__ierr){
*__ierr = PetscSectionSetNumFields(
	(PetscSection)PetscToPointer((s) ),*numFields);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetfieldcomponents_(PetscSection s,PetscInt *field,PetscInt *numComp, int *__ierr){
*__ierr = PetscSectionGetFieldComponents(
	(PetscSection)PetscToPointer((s) ),*field,numComp);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetfieldcomponents_(PetscSection s,PetscInt *field,PetscInt *numComp, int *__ierr){
*__ierr = PetscSectionSetFieldComponents(
	(PetscSection)PetscToPointer((s) ),*field,*numComp);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetchart_(PetscSection s,PetscInt *pStart,PetscInt *pEnd, int *__ierr){
*__ierr = PetscSectionGetChart(
	(PetscSection)PetscToPointer((s) ),pStart,pEnd);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetchart_(PetscSection s,PetscInt *pStart,PetscInt *pEnd, int *__ierr){
*__ierr = PetscSectionSetChart(
	(PetscSection)PetscToPointer((s) ),*pStart,*pEnd);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetpermutation_(PetscSection s,IS *perm, int *__ierr){
*__ierr = PetscSectionGetPermutation(
	(PetscSection)PetscToPointer((s) ),perm);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetpermutation_(PetscSection s,IS perm, int *__ierr){
*__ierr = PetscSectionSetPermutation(
	(PetscSection)PetscToPointer((s) ),
	(IS)PetscToPointer((perm) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionGetDof(
	(PetscSection)PetscToPointer((s) ),*point,numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionSetDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionadddof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionAddDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetfielddof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionGetFieldDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetfielddof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionSetFieldDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionaddfielddof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionAddFieldDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetconstraintdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionGetConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetconstraintdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionSetConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionaddconstraintdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionAddConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetfieldconstraintdof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionGetFieldConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetfieldconstraintdof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionSetFieldConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionaddfieldconstraintdof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr){
*__ierr = PetscSectionAddFieldConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetup_(PetscSection s, int *__ierr){
*__ierr = PetscSectionSetUp(
	(PetscSection)PetscToPointer((s) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetmaxdof_(PetscSection s,PetscInt *maxDof, int *__ierr){
*__ierr = PetscSectionGetMaxDof(
	(PetscSection)PetscToPointer((s) ),maxDof);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetstoragesize_(PetscSection s,PetscInt *size, int *__ierr){
*__ierr = PetscSectionGetStorageSize(
	(PetscSection)PetscToPointer((s) ),size);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetconstrainedstoragesize_(PetscSection s,PetscInt *size, int *__ierr){
*__ierr = PetscSectionGetConstrainedStorageSize(
	(PetscSection)PetscToPointer((s) ),size);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectioncreateglobalsection_(PetscSection s,PetscSF sf,PetscBool *includeConstraints,PetscBool *localOffsets,PetscSection *gsection, int *__ierr){
*__ierr = PetscSectionCreateGlobalSection(
	(PetscSection)PetscToPointer((s) ),
	(PetscSF)PetscToPointer((sf) ),*includeConstraints,*localOffsets,gsection);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectioncreateglobalsectioncensored_(PetscSection s,PetscSF sf,PetscBool *includeConstraints,PetscInt *numExcludes, PetscInt excludes[],PetscSection *gsection, int *__ierr){
*__ierr = PetscSectionCreateGlobalSectionCensored(
	(PetscSection)PetscToPointer((s) ),
	(PetscSF)PetscToPointer((sf) ),*includeConstraints,*numExcludes,excludes,gsection);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetvaluelayout_(MPI_Fint * comm,PetscSection s,PetscLayout *layout, int *__ierr){
*__ierr = PetscSectionGetValueLayout(
	MPI_Comm_f2c(*(comm)),
	(PetscSection)PetscToPointer((s) ),layout);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetoffset_(PetscSection s,PetscInt *point,PetscInt *offset, int *__ierr){
*__ierr = PetscSectionGetOffset(
	(PetscSection)PetscToPointer((s) ),*point,offset);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetoffset_(PetscSection s,PetscInt *point,PetscInt *offset, int *__ierr){
*__ierr = PetscSectionSetOffset(
	(PetscSection)PetscToPointer((s) ),*point,*offset);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetfieldoffset_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *offset, int *__ierr){
*__ierr = PetscSectionGetFieldOffset(
	(PetscSection)PetscToPointer((s) ),*point,*field,offset);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetfieldoffset_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *offset, int *__ierr){
*__ierr = PetscSectionSetFieldOffset(
	(PetscSection)PetscToPointer((s) ),*point,*field,*offset);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetoffsetrange_(PetscSection s,PetscInt *start,PetscInt *end, int *__ierr){
*__ierr = PetscSectionGetOffsetRange(
	(PetscSection)PetscToPointer((s) ),start,end);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionreset_(PetscSection s, int *__ierr){
*__ierr = PetscSectionReset(
	(PetscSection)PetscToPointer((s) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiondestroy_(PetscSection *s, int *__ierr){
*__ierr = PetscSectionDestroy(s);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionpermute_(PetscSection section,IS permutation,PetscSection *sectionNew, int *__ierr){
*__ierr = PetscSectionPermute(
	(PetscSection)PetscToPointer((section) ),
	(IS)PetscToPointer((permutation) ),sectionNew);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetclosureindex_(PetscSection section,PetscObject obj,PetscSection clSection,IS clPoints, int *__ierr){
*__ierr = PetscSectionSetClosureIndex(
	(PetscSection)PetscToPointer((section) ),
	(PetscObject)PetscToPointer((obj) ),
	(PetscSection)PetscToPointer((clSection) ),
	(IS)PetscToPointer((clPoints) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetclosureindex_(PetscSection section,PetscObject obj,PetscSection *clSection,IS *clPoints, int *__ierr){
*__ierr = PetscSectionGetClosureIndex(
	(PetscSection)PetscToPointer((section) ),
	(PetscObject)PetscToPointer((obj) ),clSection,clPoints);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetclosurepermutation_(PetscSection section,PetscObject obj,IS perm, int *__ierr){
*__ierr = PetscSectionSetClosurePermutation(
	(PetscSection)PetscToPointer((section) ),
	(PetscObject)PetscToPointer((obj) ),
	(IS)PetscToPointer((perm) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetclosurepermutation_(PetscSection section,PetscObject obj,IS *perm, int *__ierr){
*__ierr = PetscSectionGetClosurePermutation(
	(PetscSection)PetscToPointer((section) ),
	(PetscObject)PetscToPointer((obj) ),perm);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetclosureinversepermutation_(PetscSection section,PetscObject obj,IS *perm, int *__ierr){
*__ierr = PetscSectionGetClosureInversePermutation(
	(PetscSection)PetscToPointer((section) ),
	(PetscObject)PetscToPointer((obj) ),perm);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetfield_(PetscSection s,PetscInt *field,PetscSection *subs, int *__ierr){
*__ierr = PetscSectionGetField(
	(PetscSection)PetscToPointer((s) ),*field,subs);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsymcreate_(MPI_Fint * comm,PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionSymCreate(
	MPI_Comm_f2c(*(comm)),
	(PetscSectionSym* )PetscToPointer((sym) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsymdestroy_(PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionSymDestroy(
	(PetscSectionSym* )PetscToPointer((sym) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetsym_(PetscSection section,PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionSetSym(
	(PetscSection)PetscToPointer((section) ),*sym);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetsym_(PetscSection section,PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionGetSym(
	(PetscSection)PetscToPointer((section) ),
	(PetscSectionSym* )PetscToPointer((sym) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetfieldsym_(PetscSection section,PetscInt *field,PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionSetFieldSym(
	(PetscSection)PetscToPointer((section) ),*field,*sym);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetfieldsym_(PetscSection section,PetscInt *field,PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionGetFieldSym(
	(PetscSection)PetscToPointer((section) ),*field,
	(PetscSectionSym* )PetscToPointer((sym) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectiongetusefieldoffsets_(PetscSection s,PetscBool *flg, int *__ierr){
*__ierr = PetscSectionGetUseFieldOffsets(
	(PetscSection)PetscToPointer((s) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsetusefieldoffsets_(PetscSection s,PetscBool *flg, int *__ierr){
*__ierr = PetscSectionSetUseFieldOffsets(
	(PetscSection)PetscToPointer((s) ),*flg);
}
#if defined(__cplusplus)
}
#endif
