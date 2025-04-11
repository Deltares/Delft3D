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
#define dmnetworkgetnumsubnetworks_ DMNETWORKGETNUMSUBNETWORKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetnumsubnetworks_ dmnetworkgetnumsubnetworks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworksetnumsubnetworks_ DMNETWORKSETNUMSUBNETWORKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworksetnumsubnetworks_ dmnetworksetnumsubnetworks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkaddsubnetwork_ DMNETWORKADDSUBNETWORK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkaddsubnetwork_ dmnetworkaddsubnetwork
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworklayoutsetup_ DMNETWORKLAYOUTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworklayoutsetup_ dmnetworklayoutsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkaddsharedvertices_ DMNETWORKADDSHAREDVERTICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkaddsharedvertices_ dmnetworkaddsharedvertices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetnumvertices_ DMNETWORKGETNUMVERTICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetnumvertices_ dmnetworkgetnumvertices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetnumedges_ DMNETWORKGETNUMEDGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetnumedges_ dmnetworkgetnumedges
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
#define dmnetworkgetnumcomponents_ DMNETWORKGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetnumcomponents_ dmnetworkgetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetlocalvecoffset_ DMNETWORKGETLOCALVECOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetlocalvecoffset_ dmnetworkgetlocalvecoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetglobalvecoffset_ DMNETWORKGETGLOBALVECOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetglobalvecoffset_ dmnetworkgetglobalvecoffset
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
#define dmnetworkaddcomponent_ DMNETWORKADDCOMPONENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkaddcomponent_ dmnetworkaddcomponent
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetcomponent_ DMNETWORKGETCOMPONENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetcomponent_ dmnetworkgetcomponent
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
#define dmnetworkissharedvertex_ DMNETWORKISSHAREDVERTEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkissharedvertex_ dmnetworkissharedvertex
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkgetvertexlocaltoglobalordering_ DMNETWORKGETVERTEXLOCALTOGLOBALORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkgetvertexlocaltoglobalordering_ dmnetworkgetvertexlocaltoglobalordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworksetvertexlocaltoglobalordering_ DMNETWORKSETVERTEXLOCALTOGLOBALORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworksetvertexlocaltoglobalordering_ dmnetworksetvertexlocaltoglobalordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkcreateis_ DMNETWORKCREATEIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkcreateis_ dmnetworkcreateis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkcreatelocalis_ DMNETWORKCREATELOCALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkcreatelocalis_ dmnetworkcreatelocalis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkfinalizecomponents_ DMNETWORKFINALIZECOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkfinalizecomponents_ dmnetworkfinalizecomponents
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmnetworkgetplex_(DM dm,DM *plexdm, int *__ierr)
{
*__ierr = DMNetworkGetPlex(
	(DM)PetscToPointer((dm) ),plexdm);
}
PETSC_EXTERN void  dmnetworkgetnumsubnetworks_(DM dm,PetscInt *nsubnet,PetscInt *Nsubnet, int *__ierr)
{
*__ierr = DMNetworkGetNumSubNetworks(
	(DM)PetscToPointer((dm) ),nsubnet,Nsubnet);
}
PETSC_EXTERN void  dmnetworksetnumsubnetworks_(DM dm,PetscInt *nsubnet,PetscInt *Nsubnet, int *__ierr)
{
*__ierr = DMNetworkSetNumSubNetworks(
	(DM)PetscToPointer((dm) ),*nsubnet,*Nsubnet);
}
PETSC_EXTERN void  dmnetworkaddsubnetwork_(DM dm, char *name,PetscInt *ne,PetscInt edgelist[],PetscInt *netnum, int *__ierr, int cl0)
{
*__ierr = DMNetworkAddSubnetwork(
	(DM)PetscToPointer((dm) ),name,*ne,edgelist,netnum);
}
PETSC_EXTERN void  dmnetworklayoutsetup_(DM dm, int *__ierr)
{
*__ierr = DMNetworkLayoutSetUp(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmnetworkaddsharedvertices_(DM dm,PetscInt *anetnum,PetscInt *bnetnum,PetscInt *nsvtx,PetscInt asvtx[],PetscInt bsvtx[], int *__ierr)
{
*__ierr = DMNetworkAddSharedVertices(
	(DM)PetscToPointer((dm) ),*anetnum,*bnetnum,*nsvtx,asvtx,bsvtx);
}
PETSC_EXTERN void  dmnetworkgetnumvertices_(DM dm,PetscInt *nVertices,PetscInt *NVertices, int *__ierr)
{
*__ierr = DMNetworkGetNumVertices(
	(DM)PetscToPointer((dm) ),nVertices,NVertices);
}
PETSC_EXTERN void  dmnetworkgetnumedges_(DM dm,PetscInt *nEdges,PetscInt *NEdges, int *__ierr)
{
*__ierr = DMNetworkGetNumEdges(
	(DM)PetscToPointer((dm) ),nEdges,NEdges);
}
PETSC_EXTERN void  dmnetworkgetvertexrange_(DM dm,PetscInt *vStart,PetscInt *vEnd, int *__ierr)
{
*__ierr = DMNetworkGetVertexRange(
	(DM)PetscToPointer((dm) ),vStart,vEnd);
}
PETSC_EXTERN void  dmnetworkgetedgerange_(DM dm,PetscInt *eStart,PetscInt *eEnd, int *__ierr)
{
*__ierr = DMNetworkGetEdgeRange(
	(DM)PetscToPointer((dm) ),eStart,eEnd);
}
PETSC_EXTERN void  dmnetworkgetglobaledgeindex_(DM dm,PetscInt *p,PetscInt *index, int *__ierr)
{
*__ierr = DMNetworkGetGlobalEdgeIndex(
	(DM)PetscToPointer((dm) ),*p,index);
}
PETSC_EXTERN void  dmnetworkgetglobalvertexindex_(DM dm,PetscInt *p,PetscInt *index, int *__ierr)
{
*__ierr = DMNetworkGetGlobalVertexIndex(
	(DM)PetscToPointer((dm) ),*p,index);
}
PETSC_EXTERN void  dmnetworkgetnumcomponents_(DM dm,PetscInt *p,PetscInt *numcomponents, int *__ierr)
{
*__ierr = DMNetworkGetNumComponents(
	(DM)PetscToPointer((dm) ),*p,numcomponents);
}
PETSC_EXTERN void  dmnetworkgetlocalvecoffset_(DM dm,PetscInt *p,PetscInt *compnum,PetscInt *offset, int *__ierr)
{
*__ierr = DMNetworkGetLocalVecOffset(
	(DM)PetscToPointer((dm) ),*p,*compnum,offset);
}
PETSC_EXTERN void  dmnetworkgetglobalvecoffset_(DM dm,PetscInt *p,PetscInt *compnum,PetscInt *offsetg, int *__ierr)
{
*__ierr = DMNetworkGetGlobalVecOffset(
	(DM)PetscToPointer((dm) ),*p,*compnum,offsetg);
}
PETSC_EXTERN void  dmnetworkgetedgeoffset_(DM dm,PetscInt *p,PetscInt *offset, int *__ierr)
{
*__ierr = DMNetworkGetEdgeOffset(
	(DM)PetscToPointer((dm) ),*p,offset);
}
PETSC_EXTERN void  dmnetworkgetvertexoffset_(DM dm,PetscInt *p,PetscInt *offset, int *__ierr)
{
*__ierr = DMNetworkGetVertexOffset(
	(DM)PetscToPointer((dm) ),*p,offset);
}
PETSC_EXTERN void  dmnetworkaddcomponent_(DM dm,PetscInt *p,PetscInt *componentkey,void*compvalue,PetscInt *nvar, int *__ierr)
{
*__ierr = DMNetworkAddComponent(
	(DM)PetscToPointer((dm) ),*p,*componentkey,compvalue,*nvar);
}
PETSC_EXTERN void  dmnetworkgetcomponent_(DM dm,PetscInt *p,PetscInt *compnum,PetscInt *compkey,void**component,PetscInt *nvar, int *__ierr)
{
*__ierr = DMNetworkGetComponent(
	(DM)PetscToPointer((dm) ),*p,*compnum,compkey,component,nvar);
}
PETSC_EXTERN void  dmnetworkassemblegraphstructures_(DM dm, int *__ierr)
{
*__ierr = DMNetworkAssembleGraphStructures(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmnetworkdistribute_(DM *dm,PetscInt *overlap, int *__ierr)
{
*__ierr = DMNetworkDistribute(dm,*overlap);
}
PETSC_EXTERN void  dmnetworkissharedvertex_(DM dm,PetscInt *p,PetscBool *flag, int *__ierr)
{
*__ierr = DMNetworkIsSharedVertex(
	(DM)PetscToPointer((dm) ),*p,flag);
}
PETSC_EXTERN void  dmnetworkisghostvertex_(DM dm,PetscInt *p,PetscBool *isghost, int *__ierr)
{
*__ierr = DMNetworkIsGhostVertex(
	(DM)PetscToPointer((dm) ),*p,isghost);
}
PETSC_EXTERN void  dmnetworkhasjacobian_(DM dm,PetscBool *eflg,PetscBool *vflg, int *__ierr)
{
*__ierr = DMNetworkHasJacobian(
	(DM)PetscToPointer((dm) ),*eflg,*vflg);
}
PETSC_EXTERN void  dmnetworkedgesetmatrix_(DM dm,PetscInt *p,Mat J[], int *__ierr)
{
*__ierr = DMNetworkEdgeSetMatrix(
	(DM)PetscToPointer((dm) ),*p,J);
}
PETSC_EXTERN void  dmnetworkvertexsetmatrix_(DM dm,PetscInt *p,Mat J[], int *__ierr)
{
*__ierr = DMNetworkVertexSetMatrix(
	(DM)PetscToPointer((dm) ),*p,J);
}
PETSC_EXTERN void  dmnetworkgetvertexlocaltoglobalordering_(DM dm,PetscInt *vloc,PetscInt *vg, int *__ierr)
{
*__ierr = DMNetworkGetVertexLocalToGlobalOrdering(
	(DM)PetscToPointer((dm) ),*vloc,vg);
}
PETSC_EXTERN void  dmnetworksetvertexlocaltoglobalordering_(DM dm, int *__ierr)
{
*__ierr = DMNetworkSetVertexLocalToGlobalOrdering(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmnetworkcreateis_(DM dm,PetscInt *numkeys,PetscInt keys[],PetscInt blocksize[],PetscInt nselectedvar[],PetscInt *selectedvar[],IS *is, int *__ierr)
{
*__ierr = DMNetworkCreateIS(
	(DM)PetscToPointer((dm) ),*numkeys,keys,blocksize,nselectedvar,selectedvar,is);
}
PETSC_EXTERN void  dmnetworkcreatelocalis_(DM dm,PetscInt *numkeys,PetscInt keys[],PetscInt blocksize[],PetscInt nselectedvar[],PetscInt *selectedvar[],IS *is, int *__ierr)
{
*__ierr = DMNetworkCreateLocalIS(
	(DM)PetscToPointer((dm) ),*numkeys,keys,blocksize,nselectedvar,selectedvar,is);
}
PETSC_EXTERN void  dmnetworkfinalizecomponents_(DM dm, int *__ierr)
{
*__ierr = DMNetworkFinalizeComponents(
	(DM)PetscToPointer((dm) ));
}
#if defined(__cplusplus)
}
#endif
