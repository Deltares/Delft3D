#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matis.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisstorel2l_ MATISSTOREL2L
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisstorel2l_ matisstorel2l
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisfixlocalempty_ MATISFIXLOCALEMPTY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisfixlocalempty_ matisfixlocalempty
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matissetpreallocation_ MATISSETPREALLOCATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matissetpreallocation_ matissetpreallocation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisgetmpixaij_ MATISGETMPIXAIJ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisgetmpixaij_ matisgetmpixaij
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisgetlocalmat_ MATISGETLOCALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisgetlocalmat_ matisgetlocalmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisrestorelocalmat_ MATISRESTORELOCALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisrestorelocalmat_ matisrestorelocalmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matissetlocalmattype_ MATISSETLOCALMATTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matissetlocalmattype_ matissetlocalmattype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matissetlocalmat_ MATISSETLOCALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matissetlocalmat_ matissetlocalmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateis_ MATCREATEIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateis_ matcreateis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisgetlocaltoglobalmapping_ MATISGETLOCALTOGLOBALMAPPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisgetlocaltoglobalmapping_ matisgetlocaltoglobalmapping
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matisstorel2l_(Mat A,PetscBool *store, int *__ierr)
{
*__ierr = MatISStoreL2L(
	(Mat)PetscToPointer((A) ),*store);
}
PETSC_EXTERN void  matisfixlocalempty_(Mat A,PetscBool *fix, int *__ierr)
{
*__ierr = MatISFixLocalEmpty(
	(Mat)PetscToPointer((A) ),*fix);
}
PETSC_EXTERN void  matissetpreallocation_(Mat B,PetscInt *d_nz, PetscInt d_nnz[],PetscInt *o_nz, PetscInt o_nnz[], int *__ierr)
{
*__ierr = MatISSetPreallocation(
	(Mat)PetscToPointer((B) ),*d_nz,d_nnz,*o_nz,o_nnz);
}
PETSC_EXTERN void  matisgetmpixaij_(Mat mat,MatReuse *reuse,Mat *newmat, int *__ierr)
{
*__ierr = MatISGetMPIXAIJ(
	(Mat)PetscToPointer((mat) ),*reuse,newmat);
}
PETSC_EXTERN void  matisgetlocalmat_(Mat mat,Mat *local, int *__ierr)
{
*__ierr = MatISGetLocalMat(
	(Mat)PetscToPointer((mat) ),local);
}
PETSC_EXTERN void  matisrestorelocalmat_(Mat mat,Mat *local, int *__ierr)
{
*__ierr = MatISRestoreLocalMat(
	(Mat)PetscToPointer((mat) ),local);
}
PETSC_EXTERN void  matissetlocalmattype_(Mat mat,MatType *mtype, int *__ierr)
{
*__ierr = MatISSetLocalMatType(
	(Mat)PetscToPointer((mat) ),*mtype);
}
PETSC_EXTERN void  matissetlocalmat_(Mat mat,Mat local, int *__ierr)
{
*__ierr = MatISSetLocalMat(
	(Mat)PetscToPointer((mat) ),
	(Mat)PetscToPointer((local) ));
}
PETSC_EXTERN void  matcreateis_(MPI_Fint * comm,PetscInt *bs,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N,ISLocalToGlobalMapping rmap,ISLocalToGlobalMapping cmap,Mat *A, int *__ierr)
{
*__ierr = MatCreateIS(
	MPI_Comm_f2c(*(comm)),*bs,*m,*n,*M,*N,
	(ISLocalToGlobalMapping)PetscToPointer((rmap) ),
	(ISLocalToGlobalMapping)PetscToPointer((cmap) ),A);
}
PETSC_EXTERN void  matisgetlocaltoglobalmapping_(Mat A,ISLocalToGlobalMapping *rmapping,ISLocalToGlobalMapping *cmapping, int *__ierr)
{
*__ierr = MatISGetLocalToGlobalMapping(
	(Mat)PetscToPointer((A) ),rmapping,cmapping);
}
#if defined(__cplusplus)
}
#endif
