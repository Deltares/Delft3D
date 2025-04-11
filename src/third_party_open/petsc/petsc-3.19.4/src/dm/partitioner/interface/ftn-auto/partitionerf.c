#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* partitioner.c */
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

#include "petscpartitioner.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionerview_ PETSCPARTITIONERVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerview_ petscpartitionerview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionersetfromoptions_ PETSCPARTITIONERSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionersetfromoptions_ petscpartitionersetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionersetup_ PETSCPARTITIONERSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionersetup_ petscpartitionersetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionerreset_ PETSCPARTITIONERRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerreset_ petscpartitionerreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionerdestroy_ PETSCPARTITIONERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerdestroy_ petscpartitionerdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionerpartition_ PETSCPARTITIONERPARTITION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerpartition_ petscpartitionerpartition
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionercreate_ PETSCPARTITIONERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionercreate_ petscpartitionercreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscpartitionerview_(PetscPartitioner part,PetscViewer v, int *__ierr)
{
*__ierr = PetscPartitionerView(
	(PetscPartitioner)PetscToPointer((part) ),
	(PetscViewer)PetscToPointer((v) ));
}
PETSC_EXTERN void  petscpartitionersetfromoptions_(PetscPartitioner part, int *__ierr)
{
*__ierr = PetscPartitionerSetFromOptions(
	(PetscPartitioner)PetscToPointer((part) ));
}
PETSC_EXTERN void  petscpartitionersetup_(PetscPartitioner part, int *__ierr)
{
*__ierr = PetscPartitionerSetUp(
	(PetscPartitioner)PetscToPointer((part) ));
}
PETSC_EXTERN void  petscpartitionerreset_(PetscPartitioner part, int *__ierr)
{
*__ierr = PetscPartitionerReset(
	(PetscPartitioner)PetscToPointer((part) ));
}
PETSC_EXTERN void  petscpartitionerdestroy_(PetscPartitioner *part, int *__ierr)
{
*__ierr = PetscPartitionerDestroy(part);
}
PETSC_EXTERN void  petscpartitionerpartition_(PetscPartitioner part,PetscInt *nparts,PetscInt *numVertices,PetscInt start[],PetscInt adjacency[],PetscSection vertexSection,PetscSection targetSection,PetscSection partSection,IS *partition, int *__ierr)
{
*__ierr = PetscPartitionerPartition(
	(PetscPartitioner)PetscToPointer((part) ),*nparts,*numVertices,start,adjacency,
	(PetscSection)PetscToPointer((vertexSection) ),
	(PetscSection)PetscToPointer((targetSection) ),
	(PetscSection)PetscToPointer((partSection) ),partition);
}
PETSC_EXTERN void  petscpartitionercreate_(MPI_Fint * comm,PetscPartitioner *part, int *__ierr)
{
*__ierr = PetscPartitionerCreate(
	MPI_Comm_f2c(*(comm)),part);
}
#if defined(__cplusplus)
}
#endif
