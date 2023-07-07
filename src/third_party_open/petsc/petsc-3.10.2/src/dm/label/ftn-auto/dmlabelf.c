#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmlabel.c */
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

#include "petscdmlabel.h"
#include "petscis.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabeldestroy_ DMLABELDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabeldestroy_ dmlabeldestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelduplicate_ DMLABELDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelduplicate_ dmlabelduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelhasvalue_ DMLABELHASVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelhasvalue_ dmlabelhasvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelhaspoint_ DMLABELHASPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelhaspoint_ dmlabelhaspoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelstratumhaspoint_ DMLABELSTRATUMHASPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelstratumhaspoint_ dmlabelstratumhaspoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetdefaultvalue_ DMLABELGETDEFAULTVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetdefaultvalue_ dmlabelgetdefaultvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelsetdefaultvalue_ DMLABELSETDEFAULTVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelsetdefaultvalue_ dmlabelsetdefaultvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetvalue_ DMLABELGETVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetvalue_ dmlabelgetvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelsetvalue_ DMLABELSETVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelsetvalue_ dmlabelsetvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelclearvalue_ DMLABELCLEARVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelclearvalue_ dmlabelclearvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelinsertis_ DMLABELINSERTIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelinsertis_ dmlabelinsertis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetnumvalues_ DMLABELGETNUMVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetnumvalues_ dmlabelgetnumvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetvalueis_ DMLABELGETVALUEIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetvalueis_ dmlabelgetvalueis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelhasstratum_ DMLABELHASSTRATUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelhasstratum_ dmlabelhasstratum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetstratumsize_ DMLABELGETSTRATUMSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetstratumsize_ dmlabelgetstratumsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetstratumbounds_ DMLABELGETSTRATUMBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetstratumbounds_ dmlabelgetstratumbounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgetstratumis_ DMLABELGETSTRATUMIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgetstratumis_ dmlabelgetstratumis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelsetstratumis_ DMLABELSETSTRATUMIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelsetstratumis_ dmlabelsetstratumis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelclearstratum_ DMLABELCLEARSTRATUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelclearstratum_ dmlabelclearstratum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelfilter_ DMLABELFILTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelfilter_ dmlabelfilter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelpermute_ DMLABELPERMUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelpermute_ dmlabelpermute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabeldistribute_ DMLABELDISTRIBUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabeldistribute_ dmlabeldistribute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelgather_ DMLABELGATHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelgather_ dmlabelgather
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelconverttosection_ DMLABELCONVERTTOSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelconverttosection_ dmlabelconverttosection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectioncreateglobalsectionlabel_ PETSCSECTIONCREATEGLOBALSECTIONLABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectioncreateglobalsectionlabel_ petscsectioncreateglobalsectionlabel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsymlabelsetlabel_ PETSCSECTIONSYMLABELSETLABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsymlabelsetlabel_ petscsectionsymlabelsetlabel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionsymcreatelabel_ PETSCSECTIONSYMCREATELABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionsymcreatelabel_ petscsectionsymcreatelabel
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmlabeldestroy_(DMLabel *label, int *__ierr){
*__ierr = DMLabelDestroy(label);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelduplicate_(DMLabel label,DMLabel *labelnew, int *__ierr){
*__ierr = DMLabelDuplicate(
	(DMLabel)PetscToPointer((label) ),labelnew);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelhasvalue_(DMLabel label,PetscInt *value,PetscBool *contains, int *__ierr){
*__ierr = DMLabelHasValue(
	(DMLabel)PetscToPointer((label) ),*value,contains);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelhaspoint_(DMLabel label,PetscInt *point,PetscBool *contains, int *__ierr){
*__ierr = DMLabelHasPoint(
	(DMLabel)PetscToPointer((label) ),*point,contains);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelstratumhaspoint_(DMLabel label,PetscInt *value,PetscInt *point,PetscBool *contains, int *__ierr){
*__ierr = DMLabelStratumHasPoint(
	(DMLabel)PetscToPointer((label) ),*value,*point,contains);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetdefaultvalue_(DMLabel label,PetscInt *defaultValue, int *__ierr){
*__ierr = DMLabelGetDefaultValue(
	(DMLabel)PetscToPointer((label) ),defaultValue);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelsetdefaultvalue_(DMLabel label,PetscInt *defaultValue, int *__ierr){
*__ierr = DMLabelSetDefaultValue(
	(DMLabel)PetscToPointer((label) ),*defaultValue);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetvalue_(DMLabel label,PetscInt *point,PetscInt *value, int *__ierr){
*__ierr = DMLabelGetValue(
	(DMLabel)PetscToPointer((label) ),*point,value);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelsetvalue_(DMLabel label,PetscInt *point,PetscInt *value, int *__ierr){
*__ierr = DMLabelSetValue(
	(DMLabel)PetscToPointer((label) ),*point,*value);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelclearvalue_(DMLabel label,PetscInt *point,PetscInt *value, int *__ierr){
*__ierr = DMLabelClearValue(
	(DMLabel)PetscToPointer((label) ),*point,*value);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelinsertis_(DMLabel label,IS is,PetscInt *value, int *__ierr){
*__ierr = DMLabelInsertIS(
	(DMLabel)PetscToPointer((label) ),
	(IS)PetscToPointer((is) ),*value);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetnumvalues_(DMLabel label,PetscInt *numValues, int *__ierr){
*__ierr = DMLabelGetNumValues(
	(DMLabel)PetscToPointer((label) ),numValues);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetvalueis_(DMLabel label,IS *values, int *__ierr){
*__ierr = DMLabelGetValueIS(
	(DMLabel)PetscToPointer((label) ),values);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelhasstratum_(DMLabel label,PetscInt *value,PetscBool *exists, int *__ierr){
*__ierr = DMLabelHasStratum(
	(DMLabel)PetscToPointer((label) ),*value,exists);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetstratumsize_(DMLabel label,PetscInt *value,PetscInt *size, int *__ierr){
*__ierr = DMLabelGetStratumSize(
	(DMLabel)PetscToPointer((label) ),*value,size);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetstratumbounds_(DMLabel label,PetscInt *value,PetscInt *start,PetscInt *end, int *__ierr){
*__ierr = DMLabelGetStratumBounds(
	(DMLabel)PetscToPointer((label) ),*value,start,end);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgetstratumis_(DMLabel label,PetscInt *value,IS *points, int *__ierr){
*__ierr = DMLabelGetStratumIS(
	(DMLabel)PetscToPointer((label) ),*value,points);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelsetstratumis_(DMLabel label,PetscInt *value,IS is, int *__ierr){
*__ierr = DMLabelSetStratumIS(
	(DMLabel)PetscToPointer((label) ),*value,
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelclearstratum_(DMLabel label,PetscInt *value, int *__ierr){
*__ierr = DMLabelClearStratum(
	(DMLabel)PetscToPointer((label) ),*value);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelfilter_(DMLabel label,PetscInt *start,PetscInt *end, int *__ierr){
*__ierr = DMLabelFilter(
	(DMLabel)PetscToPointer((label) ),*start,*end);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelpermute_(DMLabel label,IS permutation,DMLabel *labelNew, int *__ierr){
*__ierr = DMLabelPermute(
	(DMLabel)PetscToPointer((label) ),
	(IS)PetscToPointer((permutation) ),labelNew);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabeldistribute_(DMLabel label,PetscSF sf,DMLabel *labelNew, int *__ierr){
*__ierr = DMLabelDistribute(
	(DMLabel)PetscToPointer((label) ),
	(PetscSF)PetscToPointer((sf) ),labelNew);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelgather_(DMLabel label,PetscSF sf,DMLabel *labelNew, int *__ierr){
*__ierr = DMLabelGather(
	(DMLabel)PetscToPointer((label) ),
	(PetscSF)PetscToPointer((sf) ),labelNew);
}
PETSC_EXTERN void PETSC_STDCALL  dmlabelconverttosection_(DMLabel label,PetscSection *section,IS *is, int *__ierr){
*__ierr = DMLabelConvertToSection(
	(DMLabel)PetscToPointer((label) ),section,is);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectioncreateglobalsectionlabel_(PetscSection s,PetscSF sf,PetscBool *includeConstraints,DMLabel label,PetscInt *labelValue,PetscSection *gsection, int *__ierr){
*__ierr = PetscSectionCreateGlobalSectionLabel(
	(PetscSection)PetscToPointer((s) ),
	(PetscSF)PetscToPointer((sf) ),*includeConstraints,
	(DMLabel)PetscToPointer((label) ),*labelValue,gsection);
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsymlabelsetlabel_(PetscSectionSym *sym,DMLabel label, int *__ierr){
*__ierr = PetscSectionSymLabelSetLabel(*sym,
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsectionsymcreatelabel_(MPI_Fint * comm,DMLabel label,PetscSectionSym *sym, int *__ierr){
*__ierr = PetscSectionSymCreateLabel(
	MPI_Comm_f2c(*(comm)),
	(DMLabel)PetscToPointer((label) ),
	(PetscSectionSym* )PetscToPointer((sym) ));
}
#if defined(__cplusplus)
}
#endif
