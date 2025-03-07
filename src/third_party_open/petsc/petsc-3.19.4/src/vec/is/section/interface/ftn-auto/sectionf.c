#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* section.c */
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

#include "petscsection.h"
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
#define petscsectionsetfromoptions_ PETSCSECTIONSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetfromoptions_ petscsectionsetfromoptions
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
#define petscsectiongetpointmajor_ PETSCSECTIONGETPOINTMAJOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetpointmajor_ petscsectiongetpointmajor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetpointmajor_ PETSCSECTIONSETPOINTMAJOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetpointmajor_ petscsectionsetpointmajor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetincludesconstraints_ PETSCSECTIONGETINCLUDESCONSTRAINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetincludesconstraints_ petscsectiongetincludesconstraints
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsetincludesconstraints_ PETSCSECTIONSETINCLUDESCONSTRAINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetincludesconstraints_ petscsectionsetincludesconstraints
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
#define petscsectionsetupbc_ PETSCSECTIONSETUPBC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsetupbc_ petscsectionsetupbc
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
#define petscsectiongetpointlayout_ PETSCSECTIONGETPOINTLAYOUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetpointlayout_ petscsectiongetpointlayout
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
#define petscsectiongetfieldpointoffset_ PETSCSECTIONGETFIELDPOINTOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetfieldpointoffset_ petscsectiongetfieldpointoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectiongetoffsetrange_ PETSCSECTIONGETOFFSETRANGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectiongetoffsetrange_ petscsectiongetoffsetrange
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreatesubsection_ PETSCSECTIONCREATESUBSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreatesubsection_ petscsectioncreatesubsection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreatesupersection_ PETSCSECTIONCREATESUPERSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreatesupersection_ petscsectioncreatesupersection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreatesubmeshsection_ PETSCSECTIONCREATESUBMESHSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreatesubmeshsection_ petscsectioncreatesubmeshsection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreatesubdomainsection_ PETSCSECTIONCREATESUBDOMAINSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreatesubdomainsection_ petscsectioncreatesubdomainsection
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
#define petscsectionsymcopy_ PETSCSECTIONSYMCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsymcopy_ petscsectionsymcopy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsymdistribute_ PETSCSECTIONSYMDISTRIBUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsymdistribute_ petscsectionsymdistribute
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionextractdofsfromarray_ PETSCSECTIONEXTRACTDOFSFROMARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionextractdofsfromarray_ petscsectionextractdofsfromarray
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsectioncreate_(MPI_Fint * comm,PetscSection *s, int *__ierr)
{
*__ierr = PetscSectionCreate(
	MPI_Comm_f2c(*(comm)),s);
}
PETSC_EXTERN void  petscsectioncopy_(PetscSection section,PetscSection newSection, int *__ierr)
{
*__ierr = PetscSectionCopy(
	(PetscSection)PetscToPointer((section) ),
	(PetscSection)PetscToPointer((newSection) ));
}
PETSC_EXTERN void  petscsectionclone_(PetscSection section,PetscSection *newSection, int *__ierr)
{
*__ierr = PetscSectionClone(
	(PetscSection)PetscToPointer((section) ),newSection);
}
PETSC_EXTERN void  petscsectionsetfromoptions_(PetscSection s, int *__ierr)
{
*__ierr = PetscSectionSetFromOptions(
	(PetscSection)PetscToPointer((s) ));
}
PETSC_EXTERN void  petscsectioncompare_(PetscSection s1,PetscSection s2,PetscBool *congruent, int *__ierr)
{
*__ierr = PetscSectionCompare(
	(PetscSection)PetscToPointer((s1) ),
	(PetscSection)PetscToPointer((s2) ),congruent);
}
PETSC_EXTERN void  petscsectiongetnumfields_(PetscSection s,PetscInt *numFields, int *__ierr)
{
*__ierr = PetscSectionGetNumFields(
	(PetscSection)PetscToPointer((s) ),numFields);
}
PETSC_EXTERN void  petscsectionsetnumfields_(PetscSection s,PetscInt *numFields, int *__ierr)
{
*__ierr = PetscSectionSetNumFields(
	(PetscSection)PetscToPointer((s) ),*numFields);
}
PETSC_EXTERN void  petscsectiongetfieldcomponents_(PetscSection s,PetscInt *field,PetscInt *numComp, int *__ierr)
{
*__ierr = PetscSectionGetFieldComponents(
	(PetscSection)PetscToPointer((s) ),*field,numComp);
}
PETSC_EXTERN void  petscsectionsetfieldcomponents_(PetscSection s,PetscInt *field,PetscInt *numComp, int *__ierr)
{
*__ierr = PetscSectionSetFieldComponents(
	(PetscSection)PetscToPointer((s) ),*field,*numComp);
}
PETSC_EXTERN void  petscsectiongetchart_(PetscSection s,PetscInt *pStart,PetscInt *pEnd, int *__ierr)
{
*__ierr = PetscSectionGetChart(
	(PetscSection)PetscToPointer((s) ),pStart,pEnd);
}
PETSC_EXTERN void  petscsectionsetchart_(PetscSection s,PetscInt *pStart,PetscInt *pEnd, int *__ierr)
{
*__ierr = PetscSectionSetChart(
	(PetscSection)PetscToPointer((s) ),*pStart,*pEnd);
}
PETSC_EXTERN void  petscsectiongetpermutation_(PetscSection s,IS *perm, int *__ierr)
{
*__ierr = PetscSectionGetPermutation(
	(PetscSection)PetscToPointer((s) ),perm);
}
PETSC_EXTERN void  petscsectionsetpermutation_(PetscSection s,IS perm, int *__ierr)
{
*__ierr = PetscSectionSetPermutation(
	(PetscSection)PetscToPointer((s) ),
	(IS)PetscToPointer((perm) ));
}
PETSC_EXTERN void  petscsectiongetpointmajor_(PetscSection s,PetscBool *pm, int *__ierr)
{
*__ierr = PetscSectionGetPointMajor(
	(PetscSection)PetscToPointer((s) ),pm);
}
PETSC_EXTERN void  petscsectionsetpointmajor_(PetscSection s,PetscBool *pm, int *__ierr)
{
*__ierr = PetscSectionSetPointMajor(
	(PetscSection)PetscToPointer((s) ),*pm);
}
PETSC_EXTERN void  petscsectiongetincludesconstraints_(PetscSection s,PetscBool *includesConstraints, int *__ierr)
{
*__ierr = PetscSectionGetIncludesConstraints(
	(PetscSection)PetscToPointer((s) ),includesConstraints);
}
PETSC_EXTERN void  petscsectionsetincludesconstraints_(PetscSection s,PetscBool *includesConstraints, int *__ierr)
{
*__ierr = PetscSectionSetIncludesConstraints(
	(PetscSection)PetscToPointer((s) ),*includesConstraints);
}
PETSC_EXTERN void  petscsectiongetdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionGetDof(
	(PetscSection)PetscToPointer((s) ),*point,numDof);
}
PETSC_EXTERN void  petscsectionsetdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionSetDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void  petscsectionadddof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionAddDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void  petscsectiongetfielddof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionGetFieldDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,numDof);
}
PETSC_EXTERN void  petscsectionsetfielddof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionSetFieldDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void  petscsectionaddfielddof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionAddFieldDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void  petscsectiongetconstraintdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionGetConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,numDof);
}
PETSC_EXTERN void  petscsectionsetconstraintdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionSetConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void  petscsectionaddconstraintdof_(PetscSection s,PetscInt *point,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionAddConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*numDof);
}
PETSC_EXTERN void  petscsectiongetfieldconstraintdof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionGetFieldConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,numDof);
}
PETSC_EXTERN void  petscsectionsetfieldconstraintdof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionSetFieldConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void  petscsectionaddfieldconstraintdof_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *numDof, int *__ierr)
{
*__ierr = PetscSectionAddFieldConstraintDof(
	(PetscSection)PetscToPointer((s) ),*point,*field,*numDof);
}
PETSC_EXTERN void  petscsectionsetupbc_(PetscSection s, int *__ierr)
{
*__ierr = PetscSectionSetUpBC(
	(PetscSection)PetscToPointer((s) ));
}
PETSC_EXTERN void  petscsectionsetup_(PetscSection s, int *__ierr)
{
*__ierr = PetscSectionSetUp(
	(PetscSection)PetscToPointer((s) ));
}
PETSC_EXTERN void  petscsectiongetmaxdof_(PetscSection s,PetscInt *maxDof, int *__ierr)
{
*__ierr = PetscSectionGetMaxDof(
	(PetscSection)PetscToPointer((s) ),maxDof);
}
PETSC_EXTERN void  petscsectiongetstoragesize_(PetscSection s,PetscInt *size, int *__ierr)
{
*__ierr = PetscSectionGetStorageSize(
	(PetscSection)PetscToPointer((s) ),size);
}
PETSC_EXTERN void  petscsectiongetconstrainedstoragesize_(PetscSection s,PetscInt *size, int *__ierr)
{
*__ierr = PetscSectionGetConstrainedStorageSize(
	(PetscSection)PetscToPointer((s) ),size);
}
PETSC_EXTERN void  petscsectioncreateglobalsection_(PetscSection s,PetscSF sf,PetscBool *includeConstraints,PetscBool *localOffsets,PetscSection *gsection, int *__ierr)
{
*__ierr = PetscSectionCreateGlobalSection(
	(PetscSection)PetscToPointer((s) ),
	(PetscSF)PetscToPointer((sf) ),*includeConstraints,*localOffsets,gsection);
}
PETSC_EXTERN void  petscsectioncreateglobalsectioncensored_(PetscSection s,PetscSF sf,PetscBool *includeConstraints,PetscInt *numExcludes, PetscInt excludes[],PetscSection *gsection, int *__ierr)
{
*__ierr = PetscSectionCreateGlobalSectionCensored(
	(PetscSection)PetscToPointer((s) ),
	(PetscSF)PetscToPointer((sf) ),*includeConstraints,*numExcludes,excludes,gsection);
}
PETSC_EXTERN void  petscsectiongetpointlayout_(MPI_Fint * comm,PetscSection s,PetscLayout *layout, int *__ierr)
{
*__ierr = PetscSectionGetPointLayout(
	MPI_Comm_f2c(*(comm)),
	(PetscSection)PetscToPointer((s) ),layout);
}
PETSC_EXTERN void  petscsectiongetvaluelayout_(MPI_Fint * comm,PetscSection s,PetscLayout *layout, int *__ierr)
{
*__ierr = PetscSectionGetValueLayout(
	MPI_Comm_f2c(*(comm)),
	(PetscSection)PetscToPointer((s) ),layout);
}
PETSC_EXTERN void  petscsectiongetoffset_(PetscSection s,PetscInt *point,PetscInt *offset, int *__ierr)
{
*__ierr = PetscSectionGetOffset(
	(PetscSection)PetscToPointer((s) ),*point,offset);
}
PETSC_EXTERN void  petscsectionsetoffset_(PetscSection s,PetscInt *point,PetscInt *offset, int *__ierr)
{
*__ierr = PetscSectionSetOffset(
	(PetscSection)PetscToPointer((s) ),*point,*offset);
}
PETSC_EXTERN void  petscsectiongetfieldoffset_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *offset, int *__ierr)
{
*__ierr = PetscSectionGetFieldOffset(
	(PetscSection)PetscToPointer((s) ),*point,*field,offset);
}
PETSC_EXTERN void  petscsectionsetfieldoffset_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *offset, int *__ierr)
{
*__ierr = PetscSectionSetFieldOffset(
	(PetscSection)PetscToPointer((s) ),*point,*field,*offset);
}
PETSC_EXTERN void  petscsectiongetfieldpointoffset_(PetscSection s,PetscInt *point,PetscInt *field,PetscInt *offset, int *__ierr)
{
*__ierr = PetscSectionGetFieldPointOffset(
	(PetscSection)PetscToPointer((s) ),*point,*field,offset);
}
PETSC_EXTERN void  petscsectiongetoffsetrange_(PetscSection s,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = PetscSectionGetOffsetRange(
	(PetscSection)PetscToPointer((s) ),start,end);
}
PETSC_EXTERN void  petscsectioncreatesubsection_(PetscSection s,PetscInt *len, PetscInt fields[],PetscSection *subs, int *__ierr)
{
*__ierr = PetscSectionCreateSubsection(
	(PetscSection)PetscToPointer((s) ),*len,fields,subs);
}
PETSC_EXTERN void  petscsectioncreatesupersection_(PetscSection s[],PetscInt *len,PetscSection *supers, int *__ierr)
{
*__ierr = PetscSectionCreateSupersection(s,*len,supers);
}
PETSC_EXTERN void  petscsectioncreatesubmeshsection_(PetscSection s,IS subpointMap,PetscSection *subs, int *__ierr)
{
*__ierr = PetscSectionCreateSubmeshSection(
	(PetscSection)PetscToPointer((s) ),
	(IS)PetscToPointer((subpointMap) ),subs);
}
PETSC_EXTERN void  petscsectioncreatesubdomainsection_(PetscSection s,IS subpointMap,PetscSection *subs, int *__ierr)
{
*__ierr = PetscSectionCreateSubdomainSection(
	(PetscSection)PetscToPointer((s) ),
	(IS)PetscToPointer((subpointMap) ),subs);
}
PETSC_EXTERN void  petscsectionreset_(PetscSection s, int *__ierr)
{
*__ierr = PetscSectionReset(
	(PetscSection)PetscToPointer((s) ));
}
PETSC_EXTERN void  petscsectiondestroy_(PetscSection *s, int *__ierr)
{
*__ierr = PetscSectionDestroy(s);
}
PETSC_EXTERN void  petscsectionpermute_(PetscSection section,IS permutation,PetscSection *sectionNew, int *__ierr)
{
*__ierr = PetscSectionPermute(
	(PetscSection)PetscToPointer((section) ),
	(IS)PetscToPointer((permutation) ),sectionNew);
}
PETSC_EXTERN void  petscsectionsetclosureindex_(PetscSection section,PetscObject *obj,PetscSection clSection,IS clPoints, int *__ierr)
{
*__ierr = PetscSectionSetClosureIndex(
	(PetscSection)PetscToPointer((section) ),*obj,
	(PetscSection)PetscToPointer((clSection) ),
	(IS)PetscToPointer((clPoints) ));
}
PETSC_EXTERN void  petscsectiongetclosureindex_(PetscSection section,PetscObject *obj,PetscSection *clSection,IS *clPoints, int *__ierr)
{
*__ierr = PetscSectionGetClosureIndex(
	(PetscSection)PetscToPointer((section) ),*obj,clSection,clPoints);
}
PETSC_EXTERN void  petscsectionsetclosurepermutation_(PetscSection section,PetscObject *obj,PetscInt *depth,IS perm, int *__ierr)
{
*__ierr = PetscSectionSetClosurePermutation(
	(PetscSection)PetscToPointer((section) ),*obj,*depth,
	(IS)PetscToPointer((perm) ));
}
PETSC_EXTERN void  petscsectiongetclosurepermutation_(PetscSection section,PetscObject *obj,PetscInt *depth,PetscInt *clSize,IS *perm, int *__ierr)
{
*__ierr = PetscSectionGetClosurePermutation(
	(PetscSection)PetscToPointer((section) ),*obj,*depth,*clSize,perm);
}
PETSC_EXTERN void  petscsectiongetclosureinversepermutation_(PetscSection section,PetscObject *obj,PetscInt *depth,PetscInt *clSize,IS *perm, int *__ierr)
{
*__ierr = PetscSectionGetClosureInversePermutation(
	(PetscSection)PetscToPointer((section) ),*obj,*depth,*clSize,perm);
}
PETSC_EXTERN void  petscsectiongetfield_(PetscSection s,PetscInt *field,PetscSection *subs, int *__ierr)
{
*__ierr = PetscSectionGetField(
	(PetscSection)PetscToPointer((s) ),*field,subs);
}
PETSC_EXTERN void  petscsectionsymcreate_(MPI_Fint * comm,PetscSectionSym *sym, int *__ierr)
{
*__ierr = PetscSectionSymCreate(
	MPI_Comm_f2c(*(comm)),sym);
}
PETSC_EXTERN void  petscsectionsymdestroy_(PetscSectionSym *sym, int *__ierr)
{
*__ierr = PetscSectionSymDestroy(sym);
}
PETSC_EXTERN void  petscsectionsetsym_(PetscSection section,PetscSectionSym sym, int *__ierr)
{
*__ierr = PetscSectionSetSym(
	(PetscSection)PetscToPointer((section) ),
	(PetscSectionSym)PetscToPointer((sym) ));
}
PETSC_EXTERN void  petscsectiongetsym_(PetscSection section,PetscSectionSym *sym, int *__ierr)
{
*__ierr = PetscSectionGetSym(
	(PetscSection)PetscToPointer((section) ),sym);
}
PETSC_EXTERN void  petscsectionsetfieldsym_(PetscSection section,PetscInt *field,PetscSectionSym sym, int *__ierr)
{
*__ierr = PetscSectionSetFieldSym(
	(PetscSection)PetscToPointer((section) ),*field,
	(PetscSectionSym)PetscToPointer((sym) ));
}
PETSC_EXTERN void  petscsectiongetfieldsym_(PetscSection section,PetscInt *field,PetscSectionSym *sym, int *__ierr)
{
*__ierr = PetscSectionGetFieldSym(
	(PetscSection)PetscToPointer((section) ),*field,sym);
}
PETSC_EXTERN void  petscsectionsymcopy_(PetscSectionSym sym,PetscSectionSym nsym, int *__ierr)
{
*__ierr = PetscSectionSymCopy(
	(PetscSectionSym)PetscToPointer((sym) ),
	(PetscSectionSym)PetscToPointer((nsym) ));
}
PETSC_EXTERN void  petscsectionsymdistribute_(PetscSectionSym sym,PetscSF migrationSF,PetscSectionSym *dsym, int *__ierr)
{
*__ierr = PetscSectionSymDistribute(
	(PetscSectionSym)PetscToPointer((sym) ),
	(PetscSF)PetscToPointer((migrationSF) ),dsym);
}
PETSC_EXTERN void  petscsectiongetusefieldoffsets_(PetscSection s,PetscBool *flg, int *__ierr)
{
*__ierr = PetscSectionGetUseFieldOffsets(
	(PetscSection)PetscToPointer((s) ),flg);
}
PETSC_EXTERN void  petscsectionsetusefieldoffsets_(PetscSection s,PetscBool *flg, int *__ierr)
{
*__ierr = PetscSectionSetUseFieldOffsets(
	(PetscSection)PetscToPointer((s) ),*flg);
}
PETSC_EXTERN void  petscsectionextractdofsfromarray_(PetscSection origSection,MPI_Fint * dataType, void*origArray,IS points,PetscSection *newSection,void*newArray[], int *__ierr)
{
*__ierr = PetscSectionExtractDofsFromArray(
	(PetscSection)PetscToPointer((origSection) ),
	MPI_Type_f2c(*(dataType)),origArray,
	(IS)PetscToPointer((points) ),newSection,newArray);
}
#if defined(__cplusplus)
}
#endif
