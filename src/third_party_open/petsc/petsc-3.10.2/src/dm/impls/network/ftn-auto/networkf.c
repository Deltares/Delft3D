#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* network.c */
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

#include "petscdmnetwork.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetplex_ DMNETWORKGETPLEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetplex_ dmnetworkgetplex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworksetsizes_ DMNETWORKSETSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworksetsizes_ dmnetworksetsizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworksetedgelist_ DMNETWORKSETEDGELIST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworksetedgelist_ dmnetworksetedgelist
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworklayoutsetup_ DMNETWORKLAYOUTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworklayoutsetup_ dmnetworklayoutsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetvertexrange_ DMNETWORKGETVERTEXRANGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetvertexrange_ dmnetworkgetvertexrange
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetedgerange_ DMNETWORKGETEDGERANGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetedgerange_ dmnetworkgetedgerange
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetglobaledgeindex_ DMNETWORKGETGLOBALEDGEINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetglobaledgeindex_ dmnetworkgetglobaledgeindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetglobalvertexindex_ DMNETWORKGETGLOBALVERTEXINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetglobalvertexindex_ dmnetworkgetglobalvertexindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetcomponent_ DMNETWORKGETCOMPONENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetcomponent_ dmnetworkgetcomponent
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkaddcomponent_ DMNETWORKADDCOMPONENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkaddcomponent_ dmnetworkaddcomponent
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetnumcomponents_ DMNETWORKGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetnumcomponents_ dmnetworkgetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetvariableoffset_ DMNETWORKGETVARIABLEOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetvariableoffset_ dmnetworkgetvariableoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetvariableglobaloffset_ DMNETWORKGETVARIABLEGLOBALOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetvariableglobaloffset_ dmnetworkgetvariableglobaloffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetedgeoffset_ DMNETWORKGETEDGEOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetedgeoffset_ dmnetworkgetedgeoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetvertexoffset_ DMNETWORKGETVERTEXOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetvertexoffset_ dmnetworkgetvertexoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkaddnumvariables_ DMNETWORKADDNUMVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkaddnumvariables_ dmnetworkaddnumvariables
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetnumvariables_ DMNETWORKGETNUMVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetnumvariables_ dmnetworkgetnumvariables
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworksetnumvariables_ DMNETWORKSETNUMVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworksetnumvariables_ dmnetworksetnumvariables
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkassemblegraphstructures_ DMNETWORKASSEMBLEGRAPHSTRUCTURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkassemblegraphstructures_ dmnetworkassemblegraphstructures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkdistribute_ DMNETWORKDISTRIBUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkdistribute_ dmnetworkdistribute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkisghostvertex_ DMNETWORKISGHOSTVERTEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkisghostvertex_ dmnetworkisghostvertex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkhasjacobian_ DMNETWORKHASJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkhasjacobian_ dmnetworkhasjacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkedgesetmatrix_ DMNETWORKEDGESETMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkedgesetmatrix_ dmnetworkedgesetmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkvertexsetmatrix_ DMNETWORKVERTEXSETMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkvertexsetmatrix_ dmnetworkvertexsetmatrix
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetplex_(DM netdm,DM *plexdm, int *__ierr){
*__ierr = DMNetworkGetPlex(
	(DM)PetscToPointer((netdm) ),plexdm);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworksetsizes_(DM dm,PetscInt *Nsubnet,PetscInt *NsubnetCouple,PetscInt nV[],PetscInt nE[],PetscInt NV[],PetscInt NE[], int *__ierr){
*__ierr = DMNetworkSetSizes(
	(DM)PetscToPointer((dm) ),*Nsubnet,*NsubnetCouple,nV,nE,NV,NE);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworksetedgelist_(DM dm,PetscInt *edgelist[],PetscInt *edgelistCouple[], int *__ierr){
*__ierr = DMNetworkSetEdgeList(
	(DM)PetscToPointer((dm) ),edgelist,edgelistCouple);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworklayoutsetup_(DM dm, int *__ierr){
*__ierr = DMNetworkLayoutSetUp(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetvertexrange_(DM dm,PetscInt *vStart,PetscInt *vEnd, int *__ierr){
*__ierr = DMNetworkGetVertexRange(
	(DM)PetscToPointer((dm) ),vStart,vEnd);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetedgerange_(DM dm,PetscInt *eStart,PetscInt *eEnd, int *__ierr){
*__ierr = DMNetworkGetEdgeRange(
	(DM)PetscToPointer((dm) ),eStart,eEnd);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetglobaledgeindex_(DM dm,PetscInt *p,PetscInt *index, int *__ierr){
*__ierr = DMNetworkGetGlobalEdgeIndex(
	(DM)PetscToPointer((dm) ),*p,index);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetglobalvertexindex_(DM dm,PetscInt *p,PetscInt *index, int *__ierr){
*__ierr = DMNetworkGetGlobalVertexIndex(
	(DM)PetscToPointer((dm) ),*p,index);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetcomponent_(DM dm,PetscInt *p,PetscInt *compnum,PetscInt *key,void**component, int *__ierr){
*__ierr = DMNetworkGetComponent(
	(DM)PetscToPointer((dm) ),*p,*compnum,key,component);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkaddcomponent_(DM dm,PetscInt *p,PetscInt *componentkey,void* compvalue, int *__ierr){
*__ierr = DMNetworkAddComponent(
	(DM)PetscToPointer((dm) ),*p,*componentkey,compvalue);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetnumcomponents_(DM dm,PetscInt *p,PetscInt *numcomponents, int *__ierr){
*__ierr = DMNetworkGetNumComponents(
	(DM)PetscToPointer((dm) ),*p,numcomponents);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetvariableoffset_(DM dm,PetscInt *p,PetscInt *offset, int *__ierr){
*__ierr = DMNetworkGetVariableOffset(
	(DM)PetscToPointer((dm) ),*p,offset);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetvariableglobaloffset_(DM dm,PetscInt *p,PetscInt *offsetg, int *__ierr){
*__ierr = DMNetworkGetVariableGlobalOffset(
	(DM)PetscToPointer((dm) ),*p,offsetg);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetedgeoffset_(DM dm,PetscInt *p,PetscInt *offset, int *__ierr){
*__ierr = DMNetworkGetEdgeOffset(
	(DM)PetscToPointer((dm) ),*p,offset);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetvertexoffset_(DM dm,PetscInt *p,PetscInt *offset, int *__ierr){
*__ierr = DMNetworkGetVertexOffset(
	(DM)PetscToPointer((dm) ),*p,offset);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkaddnumvariables_(DM dm,PetscInt *p,PetscInt *nvar, int *__ierr){
*__ierr = DMNetworkAddNumVariables(
	(DM)PetscToPointer((dm) ),*p,*nvar);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkgetnumvariables_(DM dm,PetscInt *p,PetscInt *nvar, int *__ierr){
*__ierr = DMNetworkGetNumVariables(
	(DM)PetscToPointer((dm) ),*p,nvar);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworksetnumvariables_(DM dm,PetscInt *p,PetscInt *nvar, int *__ierr){
*__ierr = DMNetworkSetNumVariables(
	(DM)PetscToPointer((dm) ),*p,*nvar);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkassemblegraphstructures_(DM dm, int *__ierr){
*__ierr = DMNetworkAssembleGraphStructures(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkdistribute_(DM *dm,PetscInt *overlap, int *__ierr){
*__ierr = DMNetworkDistribute(dm,*overlap);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkisghostvertex_(DM dm,PetscInt *p,PetscBool *isghost, int *__ierr){
*__ierr = DMNetworkIsGhostVertex(
	(DM)PetscToPointer((dm) ),*p,isghost);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkhasjacobian_(DM dm,PetscBool *eflg,PetscBool *vflg, int *__ierr){
*__ierr = DMNetworkHasJacobian(
	(DM)PetscToPointer((dm) ),*eflg,*vflg);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkedgesetmatrix_(DM dm,PetscInt *p,Mat J[], int *__ierr){
*__ierr = DMNetworkEdgeSetMatrix(
	(DM)PetscToPointer((dm) ),*p,J);
}
PETSC_EXTERN void PETSC_STDCALL  dmnetworkvertexsetmatrix_(DM dm,PetscInt *p,Mat J[], int *__ierr){
*__ierr = DMNetworkVertexSetMatrix(
	(DM)PetscToPointer((dm) ),*p,J);
}
#if defined(__cplusplus)
}
#endif
