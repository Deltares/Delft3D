!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

!> Module containing the subroutines called in initializing <flow1d_implicit>
!> This module resides in <dflowfm_data> and hence has access to all other 
!> data. This is contrary to module <m_f1dimp> which resides in project 
!> <flow1d_implicit> and only has access to the variables in that project.
    
module m_initialize_flow1d_implicit

    contains
 
subroutine initialize_flow1d_implicit(iresult)

implicit none

!output
integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!----------------------------------------
!BEGIN CALC
!----------------------------------------

call inifm1dimp_ini(iresult) !INItialize arrays
call inifm1dimp_lob(iresult) !Loop On Branches
call inifm1dimp_faap(iresult)!Fill Arrays that need Additional Point
call inifm1dimp_fic(iresult) !Fill Initial Condition
call inifm1dimp_fbrp(iresult)!Fill Branch PRoperties 
call inifm1dimp_fbc(iresult) !Fill Boundary Conditions
call inifm1dimp_fnod(iresult)!Fill NODes
call inifm1dimp_chk(iresult) !CHecK

end subroutine initialize_flow1d_implicit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_ini
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
subroutine inifm1dimp_ini(iresult)

use m_f1dimp
use m_physcoef, only: ag, rhomean
use m_flowgeom, only: ndx, ndxi, wu, teta, lnx, tnode, lnx1D, ln, lnxi, nd, lnx1Db
use fm_external_forcings_data, only: nzbnd, nqbnd
use m_fm_erosed, only: nd_mor, ln_mor, ndx_mor, lnx_mor
use unstruc_channel_flow, only: network
use unstruc_messages
use m_sediment, only: stmpar, jased, stm_included
use m_fm_erosed, only: link1sign, link1sign2
use m_oned_functions, only: gridpoint2cross, t_gridp2cs

implicit none

!
!pointer
!
             
integer                                  , pointer :: ngrid   
integer                                  , pointer :: ngridm   
integer                                  , pointer :: nbran   
integer                                  , pointer :: maxlev
integer                                  , pointer :: nnode
integer                                  , pointer :: nhstat
integer                                  , pointer :: nqstat
integer                                  , pointer :: maxtab
integer                                  , pointer :: ntabm
integer                                  , pointer :: nbrnod
integer                                  , pointer :: table_length
integer                                  , pointer :: juer

integer, dimension(:)                    , pointer :: grd_ghost_link_closest
integer, dimension(:)                    , pointer :: grd_fmmv_fmsv

integer, dimension(:,:)                  , pointer :: node

!
!allocatable
!

type(t_gridp2cs), dimension(:), allocatable :: gridpoint2cross_o

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kbr, k1, k2, kl, kd
integer :: ndx_max, lnx_max
integer :: stat

character(len=512) :: msg

!----------------------------------------
!BEGIN POINT
!----------------------------------------

table_length           => f1dimppar%table_length
maxtab                 => f1dimppar%maxtab
nnode                  => f1dimppar%nnode
ntabm                  => f1dimppar%ntabm
nbran                  => f1dimppar%nbran
ngrid                  => f1dimppar%ngrid
nbrnod                 => f1dimppar%nbrnod
maxlev                 => f1dimppar%maxlev
ngridm                 => f1dimppar%ngridm
nhstat                 => f1dimppar%nhstat
nqstat                 => f1dimppar%nqstat
grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
grd_fmmv_fmsv          => f1dimppar%grd_fmmv_fmsv
juer                   => f1dimppar%juer
node                  => f1dimppar%node

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

!file for error
!FM1DIMP2DO. Ideally we would use the message handlinf of FM. This implies changing all calls in SRE and make sure the message handling error module is accessible. 
!Furthermore, closing of the file should be dealt with. I am not sure where to place it. 
open(newunit=juer, file="FM1DIMP.dia", status="replace", action="write", iostat=stat, iomsg=msg)

f1dimp_initialized=.true. !we use this for using <ndx> rather than <ndx_mor> (which is not defined yet) in <flow_sedmorinit>. See comment there. 

!parameters in <flwpar> (SRE variable)
f1dimppar%g=ag 
f1dimppar%rhow=rhomean 

!<SOFLOW> input
f1dimppar%steady=.true.

!dimensions
nbran=network%brs%count 

ngrid=0
ngridm=0
!nlink
do kbr=1,nbran
    ngrid=ngrid+network%BRS%BRANCH(kbr)%GRIDPOINTSCOUNT
    ngridm=max(ngridm,network%BRS%BRANCH(kbr)%GRIDPOINTSCOUNT)
    !nlink=nlink+network%BRS%BRANCH(k)%UPOINTSCOUNT
enddo

ndx_max=ndx+network%NDS%COUNT !maximum number of multivalued flownodes
lnx_max=lnx+network%NDS%maxnumberofconnections*network%NDS%COUNT !maximum number of links considering added ghost links

maxlev=0 
do k1=1,network%CSDEFINITIONS%COUNT 
    maxlev=max(maxlev,network%CSDEFINITIONS%CS(1)%LEVELSCOUNT)
enddo

nnode=network%nds%count 
nhstat=nzbnd 
nqstat=nqbnd 
maxtab=ndx - ndxi !<we have as many tables as open boundaries
!if (comparereal(nzbnd+nqbnd,ndx-ndxi,1d-10)/=0) then !FM1DIMP2DO: why does the compiler complain when using <comparereal>?
if ((nzbnd+nqbnd).ne.(ndx-ndxi)) then
    write (msgbuf, '(a)') 'Number of open boundaries is different than number of water level + discharge boundaries'
    call err_flush()
    iresult=1    
endif
table_length=2 !length of each table. All have only 2 times.
ntabm=maxtab*table_length*2 !last 2 is for <time> and <values> 
nbrnod=network%NDS%MAXNUMBEROFCONNECTIONS

if (allocated(f1dimppar%grd_sre_fm)) then
    deallocate(f1dimppar%grd_sre_fm)
endif
allocate(f1dimppar%grd_sre_fm(ngrid)) 

if (allocated(f1dimppar%grd_fm_sre)) then
    deallocate(f1dimppar%grd_fm_sre)
endif
allocate(f1dimppar%grd_fm_sre(ndx_max)) !we allocate more than we need. The maximum number of bifurcations and confluences is less than the number of nodes.
f1dimppar%grd_fm_sre=0

if (allocated(f1dimppar%grd_fm_sre2)) then
    deallocate(f1dimppar%grd_fm_sre2)
endif
allocate(f1dimppar%grd_fm_sre2(ndx_max)) !we allocate more than we need. The maximum number of bifurcations and confluences is less than the number of nodes.
f1dimppar%grd_fm_sre2=0

if (allocated(f1dimppar%grd_fmL_sre)) then
    deallocate(f1dimppar%grd_fmL_sre)
endif
allocate(f1dimppar%grd_fmL_sre(lnx1D,2)) 

if (allocated(f1dimppar%branch)) then
    deallocate(f1dimppar%branch)
endif
allocate(f1dimppar%branch(4,nbran)) 

if (allocated(f1dimppar%x)) then
    deallocate(f1dimppar%x)
endif
allocate(f1dimppar%x(ngrid))

if (allocated(f1dimppar%grd_sre_cs)) then
    deallocate(f1dimppar%grd_sre_cs)
endif
allocate(f1dimppar%grd_sre_cs(ngrid))

if (allocated(f1dimppar%hpack)) then
    deallocate(f1dimppar%hpack)
endif
allocate(f1dimppar%hpack(ngrid,3)) 

if (allocated(f1dimppar%qpack)) then
    deallocate(f1dimppar%qpack)
endif
allocate(f1dimppar%qpack(ngrid,3))

if (allocated(f1dimppar%grd_fmLb_sre)) then
    deallocate(f1dimppar%grd_fmLb_sre)
endif
allocate(f1dimppar%grd_fmLb_sre(lnx1Db-lnxi,2))

if (allocated(f1dimppar%waoft)) then
    deallocate(f1dimppar%waoft)
endif
allocate(f1dimppar%waoft(ngrid,18))

if (allocated(f1dimppar%bfrict)) then
    deallocate(f1dimppar%bfrict)
endif
allocate(f1dimppar%bfrict(3,nbran))

if (allocated(nd_mor)) then
    deallocate(nd_mor)
endif
allocate(nd_mor(ndx_max)) !more than we need
do kd=1,ndx
    nd_mor(kd)=nd(kd)
enddo
    
if (allocated(f1dimppar%grd_fmmv_fmsv)) then
    deallocate(f1dimppar%grd_fmmv_fmsv)
endif
allocate(f1dimppar%grd_fmmv_fmsv(ndx_max)) !more than we need
grd_fmmv_fmsv => f1dimppar%grd_fmmv_fmsv
!allocate every node with itself
do kd=1,ndx_max
    grd_fmmv_fmsv(kd)=kd
enddo

if (allocated(ln_mor)) then
    deallocate(ln_mor)
endif
allocate(ln_mor(2,lnx_max))
do kl=1,lnx
    do kd=1,2
        ln_mor(kd,kl)=ln(kd,kl)
    enddo
enddo

if (allocated(f1dimppar%grd_ghost_link_closest)) then
    deallocate(f1dimppar%grd_ghost_link_closest)
endif
allocate(f1dimppar%grd_ghost_link_closest(lnx_max)) !we allocate more than we need. The maximum number of bifurcations and confluences is less than the number of nodes. 
grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
do kl=1,lnx
    grd_ghost_link_closest(kl)=kl
enddo

!FM1DIMP2DO: I am now adapting the input for using the morphodynamic implementation of Pure 1D. However, 
!I amnot sure it is the best. This should be revisited with Bert :). 
if (jased > 0 .and. stm_included) then !passing if no morphpdynamics
    stmpar%morpar%mornum%pure1d=1
    call init_1dinfo() !<initialize_flow1d_implicit> is called before <init_1dinfo>. We have to call it here and it will not be called again because it will be allocated. 
endif 

allocate(link1sign(lnx_max))
link1sign=1

allocate(link1sign2(lnx_max))
link1sign2=0
!All internal links have direction 1
do kl=1,lnxi
    link1sign2(kl)=1
enddo

!copy to <gridpoint2cross_o>
if (allocated(gridpoint2cross_o)) then
    deallocate(gridpoint2cross_o)
endif
allocate(gridpoint2cross_o(ndx_max))
do kd=1,ndxi
    gridpoint2cross_o(kd)=gridpoint2cross(kd) 
    !a junction of only two branches has `num_cross_sections=2` but only one CS
    if ((gridpoint2cross_o(kd)%num_cross_sections==1).or.(gridpoint2cross_o(kd)%num_cross_sections>2)) then 
        do k2=1,gridpoint2cross_o(kd)%num_cross_sections
            if (gridpoint2cross_o(kd)%cross(k2)==-999) then
                iresult=1
            endif
        enddo
    else !`num_cross_sections=2`
        if (gridpoint2cross_o(kd)%cross(1)==-999) then !the only one is always in position 1
            iresult=1
        endif
    endif
enddo !kd
if (iresult==1) then
    write (msgbuf, '(a)') 'There is a node without cross-section.'
    call err_flush()
    return
endif

if (allocated(gridpoint2cross)) then
    deallocate(gridpoint2cross)
endif
allocate(gridpoint2cross(ndx_max))
!internal cross-sections are the same as they were (1 CS per flownode).
do kd=1,ndxi
    gridpoint2cross(kd)=gridpoint2cross_o(kd) 
enddo
!at ghost-boundary flownodes we set the number of CS to 0 to prevent looping on them (there is no CS)
do kd=ndxi+1,ndx
    gridpoint2cross(kd)%num_cross_sections=0 !This prevents it is looped in <fm_update_crosssections>
enddo

!
!gridpoints 
!

if (allocated(f1dimppar%bfricp)) then
    deallocate(f1dimppar%bfricp)
endif
allocate(f1dimppar%bfricp(6,ngrid)) !needs the part with FP1, FP2

if (allocated(f1dimppar%nlev)) then
    deallocate(f1dimppar%nlev)
endif
allocate(f1dimppar%nlev(ngrid)) 

if (allocated(f1dimppar%bedlevel)) then
    deallocate(f1dimppar%bedlevel)
endif
allocate(f1dimppar%bedlevel(ngrid))

!
!cross-section (gridpoint,level)
!

if (allocated(f1dimppar%wft)) then
    deallocate(f1dimppar%wft)
endif
allocate(f1dimppar%wft(ngrid,maxlev)) 

if (allocated(f1dimppar%aft)) then
    deallocate(f1dimppar%aft)
endif
allocate(f1dimppar%aft(ngrid,maxlev)) 

if (allocated(f1dimppar%wtt)) then
    deallocate(f1dimppar%wtt)
endif
allocate(f1dimppar%wtt(ngrid,maxlev)) 

if (allocated(f1dimppar%att)) then
    deallocate(f1dimppar%att)
endif
allocate(f1dimppar%att(ngrid,maxlev)) 

if (allocated(f1dimppar%of)) then
    deallocate(f1dimppar%of)
endif
allocate(f1dimppar%of(ngrid,maxlev)) 

if (allocated(f1dimppar%hlev)) then
    deallocate(f1dimppar%hlev)
endif
allocate(f1dimppar%hlev(ngrid,maxlev))

!
!table information 
!

if (allocated(f1dimppar%ntab)) then
    deallocate(f1dimppar%ntab)
endif     
allocate(f1dimppar%ntab(4,maxtab)) 

!
!BC 
!

if (allocated(f1dimppar%hbdpar)) then
    deallocate(f1dimppar%hbdpar)
endif
allocate(f1dimppar%hbdpar(3,nhstat)) 

if (allocated(f1dimppar%qbdpar)) then
    deallocate(f1dimppar%qbdpar)
endif
allocate(f1dimppar%qbdpar(3,nqstat)) 

if (allocated(f1dimppar%table)) then
    deallocate(f1dimppar%table)
endif
allocate(f1dimppar%table(ntabm)) 

!
!node 
!

if (allocated(f1dimppar%node)) then
    deallocate(f1dimppar%node)
endif 
allocate(f1dimppar%node(4,nnode))
node => f1dimppar%node
node = -999 !we use this value to check that it has not been filled.

if (allocated(f1dimppar%numnod)) then
    deallocate(f1dimppar%numnod)
endif 
allocate(f1dimppar%numnod(nnode))

if (allocated(f1dimppar%nodnod)) then
    deallocate(f1dimppar%nodnod)
endif 
allocate(f1dimppar%nodnod(nnode,nbrnod+1))

!
!debug
!

f1dimppar%fm1dimp_debug_k1=1

!
!deallocate locals
!

if (allocated(gridpoint2cross_o)) then
    deallocate(gridpoint2cross_o)
endif

end subroutine inifm1dimp_ini

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_lob
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_lob(iresult)

use m_f1dimp
use m_flowgeom, only: ndx, ndxi, lnx, lnx1D, ln, nd, tnode, lnxi, lnx1Db
use unstruc_channel_flow, only: network
use unstruc_messages
use m_fm_erosed, only: link1sign, link1sign2, ndx_mor, lnx_mor, lnxi_mor, ndxi_mor, ln_mor, nd_mor, ndkx_mor
use m_oned_functions, only: gridpoint2cross

implicit none


!----------------------------------------
!BEGIN POINT
!----------------------------------------

integer                                  , pointer :: nbran   

integer, dimension(:)                    , pointer :: grd_sre_fm
integer, dimension(:)                    , pointer :: grd_fm_sre
integer, dimension(:)                    , pointer :: grd_fm_sre2
integer, dimension(:)                    , pointer :: grd_sre_cs 
integer, dimension(:)                    , pointer :: grd_ghost_link_closest
integer, dimension(:)                    , pointer :: grd_fmmv_fmsv
integer, dimension(:)                    , pointer :: lin
integer, dimension(:)                    , pointer :: grd
    
integer, dimension(:,:)                  , pointer :: grd_fmL_sre
integer, dimension(:,:)                  , pointer :: grd_fmLb_sre
integer, dimension(:,:)                  , pointer :: branch

real, dimension(:)                       , pointer :: x

type(tnode)    , allocatable :: nd_o(:) !Copy of <nd> for reworking <nd>

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kbr, k1, kn, kl
integer :: c_lnx, c_ndx !counters
integer :: idx_sre, idx_fm !indices
integer :: n1, n2, pointscount, jpos
integer :: idx_i, idx_f, nl, L, L2, idx_l1, idx_l2, idx_n
integer :: j
integer :: nint, nout 

!move to function
integer :: idx_aux
integer :: min_1, min_2, k2

double precision, allocatable, dimension(:,:,:) :: msed_o

!----------------------------------------
!BEGIN POINT
!----------------------------------------

nbran                  => f1dimppar%nbran
grd_sre_cs             => f1dimppar%grd_sre_cs
grd_sre_fm             => f1dimppar%grd_sre_fm
grd_fm_sre             => f1dimppar%grd_fm_sre
grd_fm_sre2            => f1dimppar%grd_fm_sre2
grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
grd_fmmv_fmsv          => f1dimppar%grd_fmmv_fmsv
grd_fmL_sre            => f1dimppar%grd_fmL_sre
grd_fmLb_sre           => f1dimppar%grd_fmLb_sre
branch                 => f1dimppar%branch
x                      => f1dimppar%x

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

if (allocated(nd_o)) then
    deallocate(nd_o)
endif
allocate(nd_o(ndx))
nd_o=nd

idx_i=1
idx_sre=0
c_lnx=lnx
c_ndx=ndx
do kbr=1,nbran
    idx_f=idx_i+network%BRS%BRANCH(kbr)%GRIDPOINTSCOUNT-1!update index final
    
    grd_sre_fm(idx_i:idx_f)=network%BRS%BRANCH(kbr)%GRD
    x(idx_i:idx_f)=network%BRS%BRANCH(kbr)%GRIDPOINTSCHAINAGES !chainage

    nl=network%BRS%BRANCH(kbr)%UPOINTSCOUNT !only internal
    do kl=1,nl
        L=network%BRS%BRANCH(kbr)%LIN(kl)
        grd_fmL_sre(L,:)=(/ idx_i+kl-1, idx_i+kl /)
        
        !FM1DIMP2DO: Do we need this?
        !search for the GRD with <n1>? 
    	n1 = ln(1,L) 
        n2 = ln(2,L)	
        if (.not. ((grd_sre_fm(grd_fmL_sre(L,1)) .eq. n1) .or. (grd_sre_fm(grd_fmL_sre(L,1)) .eq. n2))) then
           write (msgbuf, '(a)') 'Links and nodes do not match.'
           call err_flush()
           iresult=1
        endif
        if (.not. ((grd_sre_fm(grd_fmL_sre(L,2)) .eq. n1) .or. (grd_sre_fm(grd_fmL_sre(L,2)) .eq. n2))) then
           write (msgbuf, '(a)') 'Links and nodes do not match.'
           call err_flush()
           iresult=1
        endif
    enddo !kl
    
    pointscount=network%BRS%BRANCH(kbr)%GRIDPOINTSCOUNT !FM1DIMP2DO: also make pointer?
    lin      => network%brs%branch(kbr)%lin
    grd      => network%brs%branch(kbr)%grd
    
    do kn=1,pointscount
        idx_sre=idx_sre+1
        idx_fm=grd(kn) 

        !cross-section
        if (kn==1 .or. kn==pointscount) then
           
           !FM1DIMP2DO: This part of the code is part of <set_cross_sections_to_gridpoints>, could be modularized.
           !search for index with the CS
           if (kn==1) then 
              L = lin(1)
           else
              L = lin(pointscount-1)
           endif
           do kl = 1,nd_o(idx_fm)%lnx
              if (L == iabs(nd_o(idx_fm)%ln(kl))) then
                 jpos = kl
              endif
           enddo !kl
               
           !add ghost link
           if (nd(idx_fm)%lnx>2) then !bifurcation
               
                !-------link
                c_lnx=c_lnx+1 !update link number to ghost link
                
                grd_ghost_link_closest(c_lnx)=abs(nd_o(idx_fm)%ln(jpos))
                
                !In <nd> we keep the junction node <idx_fm> connected to several branches via the ghost link, as <nd> is used for the nodal point relation.
                
                !FM1DIMP2DO: It seems all links point toward a junction in the standard scheme (based on nodal point relation). Is it true?
                if (kn==1) then 
                   link1sign2(c_lnx)=1 !link direction for morphodynamics
                   nd(idx_fm)%ln(jpos)=-c_lnx !set ghost link as the one connected to junction flownode
                else
                   link1sign2(c_lnx)=1
                   nd(idx_fm)%ln(jpos)=c_lnx !set ghost link as the one connected to junction flownode
                endif

                !-------node
                c_ndx=c_ndx+1 !update node number to multivalued node

                !save the flownode closest to the junction node in the branch under consideration
                !and
                !change the flownode connected to the first link connected to the junction flownode along the branch under consideration to the new flownode
                n1=ln(1,grd_ghost_link_closest(c_lnx)) !flownode 1 associated to new link
                n2=ln(2,grd_ghost_link_closest(c_lnx)) !flownode 2 associated to new link
                !either <n1> or <n2> is the junction node <idx_fm>. We take the other one. 
                if (idx_fm.eq.n1) then
                    grd_fmmv_fmsv(c_ndx)=n2
                    ln_mor(1,grd_ghost_link_closest(c_lnx))=c_ndx
                else
                    grd_fmmv_fmsv(c_ndx)=n1
                    ln_mor(2,grd_ghost_link_closest(c_lnx))=c_ndx
                endif
                
                nd_mor(c_ndx)%lnx=2 !in <nd_mor> only two links are connected to each node. For ghost nodes these are:
                allocate(nd_mor(c_ndx)%ln(2))
                if (kn==1) then 
                   nd_mor(c_ndx)%ln(1)=c_lnx !new ghost link
                   nd_mor(c_ndx)%ln(2)=-grd_ghost_link_closest(c_lnx) !existing link
                   ln_mor(1,c_lnx)=c_ndx
                   ln_mor(2,c_lnx)=grd_fmmv_fmsv(c_ndx)
                else
                   nd_mor(c_ndx)%ln(1)=grd_ghost_link_closest(c_lnx) !existing link
                   nd_mor(c_ndx)%ln(2)=-c_lnx !new ghost link
                   ln_mor(2,c_lnx)=c_ndx
                   ln_mor(1,c_lnx)=grd_fmmv_fmsv(c_ndx)
                endif

                grd_fm_sre(c_ndx)=idx_sre
                
                !node <idx_fm> (at the junction) does not play any role anymore in <nd_mor>. Still, 
                !we save here the index of one of the SRE points associated
                !to it for the sake of writing a value for output. 
                grd_fm_sre(idx_fm)=idx_sre
                
                !add CS at multivalued-ghost flownode
                gridpoint2cross(c_ndx)%num_cross_sections=1
                allocate(gridpoint2cross(c_ndx)%cross(gridpoint2cross(c_ndx)%num_cross_sections))
                gridpoint2cross(c_ndx)%cross(1)=gridpoint2cross(idx_fm)%cross(jpos)
                           
                !remove CS at junction flownode
                gridpoint2cross(idx_fm)%num_cross_sections=0 !This prevents it is looped in <fm_update_crosssections>
                !gridpoint2cross(idx_fm)%cross(jpos)=-999 !This prevents it is passed in <fm_update_crosssections> -> NO. -999 causes error when parsing the number of CS per node. 
                
           else !not a bifurcation (i.e., boundary)
                
                grd_fmmv_fmsv(idx_fm)=idx_fm !the closest value is itself
                
                !if <grd_fm_sre(idx_fm)> is not 0, it has already been filled. This implies
                !it is a flownode in a junction of just two branches. We have to save both 
                !sre indices for filling the initial condition.
                if (grd_fm_sre(idx_fm) .ne. 0) then
                    grd_fm_sre2(idx_fm)=idx_sre 
                else
                    grd_fm_sre(idx_fm)=idx_sre 
                endif
                                
                !relate ghost flownode also to <idx_sre>
                idx_l1=abs(nd_o(idx_fm)%ln(1))
                idx_l2=abs(nd_o(idx_fm)%ln(2))
                !there are only two links
                L=max(idx_l1,idx_l2) !the one which is external (the largest of the two) points to the ghost flownode
                L2=min(idx_l1,idx_l2) !the one which is internal (the smallest of the two) points to the internal cell
                n1=ln(1,L)
                n2=ln(2,L)
                idx_n=max(n1,n2) !the maximum flownode is the ghost one
                grd_fm_sre(idx_n)=idx_sre
                
                !link direction for morphodynamics
                link1sign2(L2)=1
                if (kn==1) then 
                   link1sign2(L)=1
                else
                   link1sign2(L)=-1
                endif
                
           endif !(nd(idx_fm)%lnx>2)
           
        else !internal point of a branch, not beginning or end. 
           jpos = 1
           
           grd_fmmv_fmsv(idx_fm)=idx_fm !the closest value is itself
           grd_fm_sre(idx_fm)=idx_sre 
                      
        endif  
        
        !FM1DIMP2DO: I wonder whether we need this or we can use the adapted <gridpoint2cross> in which there is a cross-section for 1:ndx_mor 
        grd_sre_cs(idx_sre)=gridpoint2cross(idx_fm)%cross(jpos) !cross-section index associated to the FM gridpoint per branch
                
        !if there is not a unique cross-section per gridpoint per branch, <ic=-999>. It is not needed to check
        !this here because it is already checked in <flow_sedmorinit>, which is called before <initialize_flow1d_implicit>
        
    enddo !kn
    
    !branch    
    branch(1,kbr)=network%BRS%BRANCH(kbr)%NODEINDEX(1)
    branch(2,kbr)=network%BRS%BRANCH(kbr)%NODEINDEX(2)
    branch(3,kbr)=idx_i
    branch(4,kbr)=idx_f
    
    !update index initial
    idx_i=idx_f+1
enddo !branch

!new dimensions
lnx_mor=c_lnx !store new number of links (considering ghost links)
lnxi_mor=lnx_mor !there are no ghosts in SRE
ndx_mor=c_ndx !store new number of flow nodes (considering multivaluedness)
ndxi_mor=ndx_mor !there are no ghosts in SRE
ndkx_mor=ndx_mor

!creart
idx_fm=0
do L=lnxi+1,lnx1Db !boundary links
    idx_fm=idx_fm+1
    n1 = ln(1,L) 
    n2 = ln(2,L)
    nint=min(n1,n2) !from the two cells that this link connects, the minimum is internal, and hence we have data
    nout=max(n1,n2) !from the two cells that this link connects, the maximum is extrernal, and it is the one in which we have to set the water level
    
    !FM1DIMP2DO: move to function or search for smarter way
    !grd_fmLb_sre(k,1)=findloc(grd_sre_fm,nint) !sre index with <nint> FM value !not working fine due to type of array I guess. 
    idx_aux=1
    min_1=abs(grd_sre_fm(1)-nint)
    do k2=2,size(grd_sre_fm)
        min_2=abs(grd_sre_fm(k2)-nint)
        if (min_2 < min_1) then
            min_1=min_2
            idx_aux=k2
        endif
    enddo
    
    grd_fmLb_sre(idx_fm,1)=idx_aux !SRE index of the boundary cell
    grd_fmLb_sre(idx_fm,2)=nout !FM index of the ghost cell centre associated to link <L>
    
    !mask grid
    !kcs_sre(idx_aux)=-1 !FM1DIMP2DO: I am not sure I need this or I better deal with directions in <fm_erosed> and here just set to 1 but the right dimensions.
enddo

!deallocate

if (allocated(nd_o)) then
    deallocate(nd_o)
endif

end subroutine inifm1dimp_lob

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_faap
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_faap(iresult)

use m_f1dimp
use m_flowgeom, only: ndx, bai_mor, ba, bl, dx, lnx, dxi, acl, wu, snu, csu, wu_mor, wcx1, wcx2, wcy1, wcy2, kcu, wcl, lnxi, griddim
use m_flow, only: s0, s1, u1, au, hu, qa, frcu_mor, frcu, z0urou, ifrcutp, taubxu, ucx_mor, ucy_mor, ustb
use m_sediment, only: stmpar, jased, stm_included, kcsmor
use m_fm_erosed, only: ndx_mor, lsedtot, lnx_mor, pmcrit, link1, ln_mor, hs_mor, ucxq_mor, ucyq_mor, uau
use m_turbulence, only: rhowat
use m_xbeach_data, only: ktb
use m_bedform, only: bfmpar

implicit none


!
!pointer
!
integer                                  , pointer :: nlyr

integer, dimension(:)                    , pointer :: grd_ghost_link_closest
integer, dimension(:)                    , pointer :: grd_fmmv_fmsv

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kl, kd, k1   

double precision, allocatable, dimension(:,:) :: bodsed_o
double precision, allocatable, dimension(:,:) :: thlyr_o
double precision, allocatable, dimension(:,:) :: sedshort_o
double precision, allocatable, dimension(:,:) :: svfrac_o
double precision, allocatable, dimension(:,:) :: preload_o

double precision, allocatable, dimension(:,:,:) :: msed_o

!----------------------------------------
!BEGIN POINT
!----------------------------------------

grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
grd_fmmv_fmsv          => f1dimppar%grd_fmmv_fmsv

!stmpar
if (jased > 0 .and. stm_included) then !passing if no morphpdynamics
nlyr                   => stmpar%morlyr%SETTINGS%NLYR
endif

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

!FM1DIMP2DO: If friction varies with time, <frcu_mor> is updated. The subroutine that
!does that must be modified to also adapt the friction in the ghost links.

!this cannot be in the allocation function because it needs <ln_mor>
allocate(link1(ndx_mor))
link1=0
do kl=1,lnx_mor
    k1=ln_mor(1,kl)
    !k2 = ln(2,kl)
    link1(k1)=kl
enddo

!nodes
    !allocate
call reallocate_fill(s0     ,grd_fmmv_fmsv,ndx,ndx_mor) 
call reallocate_fill(s1     ,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill(bai_mor,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill(ba     ,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill(rhowat ,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill(ktb    ,grd_fmmv_fmsv,ndx,ndx_mor) !FM1DIMP2DO: It could be better to allocate the wave part with <ndx_mor>. To limit the mess it is now done here. 
call reallocate_fill(bl     ,grd_fmmv_fmsv,ndx,ndx_mor)
!FM1DIMP2DO
!The value of <bl> here is not correct, as it copies the value from the closest node. 
!However, this is not a big issue, as this value is only used for checking whether flow
!depth is above or below threshold. The bed level value for computing flow comes from
!the cross-sections, which are treated independently. Nevertheless, we could here fill
!the right value from cross-sections. 

call reallocate_fill_pointer(bfmpar%rksr,grd_fmmv_fmsv,ndx,ndx_mor)

!FM1DIMP2DO: When not having a morhodynamic simulation, morpho variables are not initialized. The best
!would be to <return> in <reallocate_fill>

if (jased > 0 .and. stm_included) then !passing if no morphpdynamics

call reallocate_fill_pointer(stmpar%morlyr%settings%thtrlyr,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill_pointer(stmpar%morlyr%settings%thexlyr,grd_fmmv_fmsv,ndx,ndx_mor)

call reallocate_fill_pointer(ucxq_mor,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill_pointer(ucyq_mor,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill_pointer(hs_mor,grd_fmmv_fmsv,ndx,ndx_mor)

call reallocate_fill(ucx_mor     ,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill(ucy_mor     ,grd_fmmv_fmsv,ndx,ndx_mor)
   
!multidimensional nodes

    !copy pointers to temporary array

!Variables in <stmpar%morlyr%state> with the initial condition need to be reallocated.
!we cannot check if allocated because these arrays are not <allocatable>. Also, we 
!cannot work with a pointer and we have to allocate <stmpar%morlyr%state%val> rather 
!than creating <val> and point to it. This is because a pointer just points to the 
!starting address of the variable it points to and allocating does not change that. A
!different way of seeing it is to think that you should interpret the <alloc> call as
!```
!thlyr => new array(nlyr,ndx_mor)).
!```
!That's the way in which you would write it in C. Then it's immediately clear that 
!it's just changing the local pointer and not the associated target pointer.

if (allocated(bodsed_o)) then
    deallocate(bodsed_o)
endif
allocate(bodsed_o(lsedtot,ndx))
bodsed_o=stmpar%morlyr%state%bodsed

if (stmpar%morlyr%SETTINGS%IUNDERLYR==2) then
    
    if (allocated(msed_o)) then
        deallocate(msed_o)
    endif
    allocate(msed_o(lsedtot,nlyr,ndx))
    msed_o=stmpar%morlyr%state%msed

    if (allocated(thlyr_o)) then
        deallocate(thlyr_o)
    endif
    allocate(thlyr_o(nlyr,ndx))
    thlyr_o=stmpar%morlyr%state%thlyr

    if (allocated(sedshort_o)) then
        deallocate(sedshort_o)
    endif
    allocate(sedshort_o(lsedtot,ndx))
    sedshort_o=stmpar%morlyr%state%sedshort

    if (allocated(svfrac_o)) then
        deallocate(svfrac_o)
    endif
    allocate(svfrac_o(nlyr,ndx))
    svfrac_o=stmpar%morlyr%state%svfrac

    if (allocated(preload_o)) then
        deallocate(preload_o)
    endif
    allocate(preload_o(nlyr,ndx))
    preload_o=stmpar%morlyr%state%preload

endif !underlayer==2

endif !jased

!links
    !allocate
call reallocate_fill(u1      ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(au      ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(hu      ,grd_ghost_link_closest,lnx,lnx_mor)
call reallocate_fill(dx      ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(dxi     ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(wu      ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(frcu_mor,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(frcu    ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(qa      ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(acl     ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(snu     ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(csu     ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(wu_mor  ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(z0urou  ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(taubxu  ,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill(wcx1    ,grd_ghost_link_closest,lnx,lnx_mor)
call reallocate_fill(wcx2    ,grd_ghost_link_closest,lnx,lnx_mor)
call reallocate_fill(wcy1    ,grd_ghost_link_closest,lnx,lnx_mor)
call reallocate_fill(wcy2    ,grd_ghost_link_closest,lnx,lnx_mor)
call reallocate_fill(ustb    ,grd_ghost_link_closest,lnx,lnx_mor)

call reallocate_fill_int(ifrcutp,grd_ghost_link_closest,lnx,lnx_mor) 
call reallocate_fill_int(kcu,grd_ghost_link_closest,lnx,lnx_mor)

!multidimensional links

    !copy arrays to temporary array
if (allocated(wcl)) then
    deallocate(wcl)
endif
allocate(wcl(2,lnx_mor)) 
do kl=1,lnx_mor
    do kd=1,2
        wcl(kd,kl)=0.5d0 !each link corresponds to 50% of the flownode. 
    enddo
enddo
do kl=lnxi+1,lnx
    wcl(1,kl)=1.0d0 !boundary links are full
enddo

!morphodynamics initialization is done before fm1dimp initialization. Hence, we have to reallocate here using <ndx_mor> and <lnx_mor>
!It must be before the calls to <reallocate_~> because some variables (e.g., <ucxq_mor>) are set to the wrong size (i.e., <ndkx>) in <allocsedtra>
!We have to copy <bodsed> and other state variables before <flow_sedmorinit>.

if (jased > 0 .and. stm_included) then !passing if no morphpdynamics
    
stmpar%morlyr%settings%nmub=ndx_mor !size over which we will loop over morphodynamic variables
griddim%nmub=ndx_mor !size of the variables when reading morphodynamic input
call flow_sedmorinit()
griddim%nmub=ndx !restore to the original size because it is used for exporting data

stmpar%morpar%mornum%pure1d=1 !we have set it for reading <init_1dinfo> but we have to set it again because it is overwritten in <flow_sedmorinit>

!FM1DIMP2DO: We could do the same trick and call <lnx_mor> in <flow_waveinit>, but some variables have been moved to another module after JR merge. Hence, we reallocate in this routine. 
!call flow_waveinit()

!needs to be after <flow_sedmorinit>, where it is allocated. 
!FM1DIMP2DO: Ideally, these variables are allocated in <flow_sedmorinit>
call reallocate_fill_pointer(pmcrit                   ,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill_pointer(stmpar%morlyr%state%dpsed,grd_fmmv_fmsv,ndx,ndx_mor)
call reallocate_fill_int    (kcsmor                   ,grd_fmmv_fmsv,ndx,ndx_mor)

call reallocate_fill_pointer(uau,grd_ghost_link_closest,lnx,lnx_mor) 

call reallocate_fill_manual_2(stmpar%morlyr%state%bodsed  ,bodsed_o  ,grd_fmmv_fmsv,ndx,ndx_mor,lsedtot)

if (stmpar%morlyr%SETTINGS%IUNDERLYR==2) then
    
    call reallocate_fill_manual_2(stmpar%morlyr%state%sedshort,sedshort_o,grd_fmmv_fmsv,ndx,ndx_mor,lsedtot)
    
    call reallocate_fill_manual_2(stmpar%morlyr%state%thlyr   ,thlyr_o   ,grd_fmmv_fmsv,ndx,ndx_mor,nlyr)
    call reallocate_fill_manual_2(stmpar%morlyr%state%svfrac  ,svfrac_o  ,grd_fmmv_fmsv,ndx,ndx_mor,nlyr)
    call reallocate_fill_manual_2(stmpar%morlyr%state%preload ,preload_o ,grd_fmmv_fmsv,ndx,ndx_mor,nlyr)
    
    call reallocate_fill_manual_3(stmpar%morlyr%state%msed    ,msed_o    ,grd_fmmv_fmsv,ndx,ndx_mor,lsedtot,nlyr)

endif !underlayer=2

ucyq_mor=0d0 !set to 0 once rather than every time step. Somewhere in the code is changed. I have to set it every time step. 

!deallocate

if (allocated(bodsed_o)) then 
    deallocate(bodsed_o)
endif

if (allocated(msed_o)) then
    deallocate(msed_o)
endif
    
if (allocated(thlyr_o)) then
    deallocate(thlyr_o)
endif
    
if (allocated(sedshort_o)) then
    deallocate(sedshort_o)
endif
    
if (allocated(svfrac_o)) then
    deallocate(svfrac_o)
endif

if (allocated(preload_o)) then
    deallocate(preload_o)
endif
    
endif

end subroutine inifm1dimp_faap
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fic
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_fic(iresult)

use m_f1dimp
use m_flowgeom, only: ndx, ndxi, wu, nd
use unstruc_channel_flow, only: network
use m_flow, only: s1, au, u1
use m_fm_erosed, only: ndx_mor, nd_mor, ln_mor

implicit none

!
!pointer
!

integer, dimension(:)                    , pointer :: grd_fm_sre
integer, dimension(:)                    , pointer :: grd_fm_sre2

real, dimension(:,:)                     , pointer :: waoft

double precision, dimension(:,:)         , pointer :: hpack
double precision, dimension(:,:)         , pointer :: qpack

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kd, idx_fm, k1, idx_sre, idx_l1, idx_l2, k2, kn, kl, n1, n2, L, idx_n

real :: swaoft

double precision :: wu_int, au_int

!----------------------------------------
!BEGIN POINT
!----------------------------------------

grd_fm_sre             => f1dimppar%grd_fm_sre
grd_fm_sre2            => f1dimppar%grd_fm_sre2
hpack                  => f1dimppar%hpack
qpack                  => f1dimppar%qpack
waoft                  => f1dimppar%waoft

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

swaoft=size(waoft,dim=2)

!Data must be available already at ghost links and nodes

!do ksre=1,ngrid
do kd=1,ndx_mor
    
    idx_fm=kd
    
    !do not overwrite the values at the boundary
    !which are correctly written above (LOB) and would be 
    !filled incorrectly as the velocity at the ghost link
    !in the boundaries is 0.
    if (idx_fm>ndxi .and. idx_fm<ndx+1) then
        cycle
    endif
    
    if (nd_mor(idx_fm)%lnx.ne.2) then
        cycle
    endif
    
    !This is not the nicest, but it works. It is necessary to 
    !fill the initial condition correctly at junctions of only
    !two branches. We loop twice. The first time we use the regular 
    !map between fm and sre <grd_fm_sre>. The second time, if the
    !index in the auxiliary array <grd_fm_sre2> is not 0 it means
    !that this FM flownode is of a junction of two branhces. 
    !Hence, there are two gridpoints associated to it that must be 
    !filled.

    do k1=1,2
        select case (k1)
        case (1)
            idx_sre=grd_fm_sre(idx_fm)
        case (2)
            idx_sre=grd_fm_sre2(idx_fm)
            if (idx_sre.eq.0) then
                cycle
            endif
        end select

        !links connected to a given fm grid node
        idx_l1=abs(nd_mor(idx_fm)%ln(1))
        idx_l2=abs(nd_mor(idx_fm)%ln(2))
            
        !initial condition
        do k2=1,3 !< time step in SRE [before, intermediate, after]
            !water level
            hpack(idx_sre,k2)=s1(idx_fm)
            !discharge
            qpack(idx_sre,k2)=0.5*(au(idx_l1)*u1(idx_l1)+au(idx_l2)*u1(idx_l2))
        enddo !k2
        
        !waoft
        wu_int=0.5*(wu(idx_l1)+wu(idx_l2))
        au_int=0.5*(au(idx_l1)+au(idx_l2))
        
        !FM1DIMP2DO: needs to be separated between flow and total
        !check right order in <FLNORM> and not in documentation. 

        waoft(idx_sre,1)=real(wu_int) !wf = actual flow width 
        waoft(idx_sre,2)=real(wu_int) !wt = actual total width
        waoft(idx_sre,3)=real(au_int) !af = actual flow area
        waoft(idx_sre,4)=real(au_int) !at = actual total area n
        waoft(idx_sre,5)=real(au_int) !at = actual total area n+1
        waoft(idx_sre,6)=real(au_int/wu_int) !o = actual wetted perimeter
        do k2=7,swaoft
            waoft(idx_sre,k2)=0
        enddo !k2
    enddo !k1
enddo !ksre

!at junctions of more than 2 branches we set the same water level for
!all SRE nodes. 
do kn=1,network%nds%Count
   if (network%nds%node(kn)%numberofconnections < 3) cycle
      idx_fm=network%nds%node(kn)%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
      !we search for the SRE gridpoint associated to the FM ghost flownode. 
      !the FM ghostflownode is found in <ln_mor> which is found in <nd>
      do kl=1,nd(idx_fm)%lnx
          L=abs(nd(idx_fm)%ln(kl))
          n1=ln_mor(1,L)
          n2=ln_mor(2,L)
          idx_n=max(n1,n2) !maximum is ghost flownode
          idx_sre=grd_fm_sre(idx_n)
          do kd=1,3
            hpack(idx_sre,kd)=s1(idx_fm)
          enddo !kd
      enddo !kl
enddo !kn

end subroutine inifm1dimp_fic

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fbrp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_fbrp(iresult)

use m_f1dimp
use unstruc_channel_flow, only: network
use unstruc_messages

implicit none

!
!pointer
!

integer                                  , pointer :: nbran  
integer                                  , pointer :: ngrid 

integer, dimension(:)                    , pointer :: grd_sre_cs
integer, dimension(:)                    , pointer :: grd_sre_fm

integer, dimension(:,:)                  , pointer :: bfrict

real, dimension(:,:)                     , pointer :: bfricp

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kbr, ksre, k2, idx_fm, idx_sre, idx_crs

!----------------------------------------
!BEGIN POINT
!----------------------------------------

nbran                  => f1dimppar%nbran
ngrid                  => f1dimppar%ngrid
grd_sre_cs             => f1dimppar%grd_sre_cs
grd_sre_fm             => f1dimppar%grd_sre_fm
bfrict                 => f1dimppar%bfrict
bfricp                 => f1dimppar%bfricp

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

do kbr=1,nbran
    
    !bfrict
    do k2=1,3 !< main channel, floodplain 1, floodplain 2
        select case (network%RGS%ROUGH(k2)%FRICTIONTYPE) !< where is the information per branch? add when several branches!
            case (0)
                bfrict(k2,kbr)=1
            case default
                write (msgbuf, '(a)') 'Only constant Chezy friction is supported at the moment.'
                call err_flush()
                iresult=1
        end select
    enddo
!
!bfrict(1,i)=cfrchc (1) : Chézy constant
!bfrict(1,i)=cfrchq (2) : Chézy function of discharge
!bfrict(1,i)=cfrchh (3) : Chézy function of water level
!bfrict(1,i)=cfrman (4) : Manning constant
!bfrict(1,i)=cfrskn (5) : Strickler 1 constant ( k n )
!bfrict(1,i)=cfrsks (6) : Strickler 2 constant ( k s )
!bfrict(1,i)=cfrnik (7) : Nikuradze constant
!bfrict(1,i)=cfreng (8) : Engelund predicto

enddo !kbr
 
do ksre=1,ngrid
    
    idx_fm=grd_sre_fm(ksre) !index of the global grid point in fm for the global gridpoint <k> in SRE
    
    !bfrictp
    idx_crs=grd_sre_cs(ksre)
    
    bfricp(1,ksre)=network%CRS%CROSS(idx_crs)%FRICTIONVALUEPOS(1)
    bfricp(2,ksre)=network%CRS%CROSS(idx_crs)%FRICTIONVALUENEG(1)
    !deal properly with the values below when friction per section varies. 
    bfricp(3,ksre)=network%CRS%CROSS(idx_crs)%FRICTIONVALUEPOS(1)
    bfricp(4,ksre)=network%CRS%CROSS(idx_crs)%FRICTIONVALUENEG(1)
    bfricp(5,ksre)=network%CRS%CROSS(idx_crs)%FRICTIONVALUEPOS(1)
    bfricp(6,ksre)=network%CRS%CROSS(idx_crs)%FRICTIONVALUENEG(1)
!bfricp(6,ngrid)   I  Bed friction parameters:
!                     (1,i) = Parameter for positive flow direction
!                             in main section (depending on friction
!                             type):
!                             =     Chezy constant value
!                             =     Table pointer (Q or h table)
!                             =     Nikuradse parameter kn for Ni-
!                                   kuradse formula
!                             =     Manning parameter nm for Manning
!                                   formula
!                             =     Strickler coefficient ks for
!                                   Strickler formula
!                     (2,i) = Parameter for negative flow direction
!                             in main section (depending on friction
!                             type) Same definitions as bfricp(1,i).
!                     (3,i) = Parameter for positive flow direction
!                             in sub sec 1 (depending on friction
!                             type):
!                             =     Chezy constant value
!                             =     Nikuradse parameter kn for Niku-
!                                   radse formula
!                             =     Manning parameter nm for Manning
!                                   formula
!                             =     Strickler coefficient ks for
!                                   Strickler formula
!                     (4,i) = Parameter for negative flow direction
!                             in sub sec 1 (depending on friction
!                             type) Same definition as bfricp (3,i):
!                     (5,i) = Parameter for positive flow direction
!                             in sub sec 2 (depending on friction
!                             type) Same definition as bfricp (3,i).
!                     (6,i) = Parameter for negative flow direction
!                             in sub sec 2 (depending on friction
!                             type) Same definition as bfricp (3,i).
    
enddo !ksre
    
end subroutine inifm1dimp_fbrp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fbc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_fbc(iresult)

use m_f1dimp
use fm_external_forcings_data, only: kbndz, kbndu

implicit none


!
!pointer
!

integer                                  , pointer :: nhstat
integer                                  , pointer :: nqstat
integer                                  , pointer :: maxtab
integer                                  , pointer :: table_length

integer, dimension(:)                    , pointer :: grd_fm_sre

integer, dimension(:,:)                  , pointer :: hbdpar
integer, dimension(:,:)                  , pointer :: qbdpar
integer, dimension(:,:)                  , pointer :: ntab

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: k1
integer :: table_number

integer, allocatable, dimension(:)   :: kcol

!----------------------------------------
!BEGIN POINT
!----------------------------------------

table_length           => f1dimppar%table_length
maxtab                 => f1dimppar%maxtab
nhstat                 => f1dimppar%nhstat
grd_fm_sre             => f1dimppar%grd_fm_sre
nqstat                 => f1dimppar%nqstat
ntab                   => f1dimppar%ntab
qbdpar                 => f1dimppar%qbdpar
hbdpar                 => f1dimppar%hbdpar

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

!<table> will contain 4 elements per BC:
!   -X1 (time)
!   -X2 (time)
!   -Y1 (variable, e.g., water level)
!   -Y2 (variable, e.g., water level)
!the order in <table> is:
!   -H-boundaries 
!   -Q-boundaires

table_number=0 !counter position in which the BC is saved in the table

!h
do k1=1,nhstat
    table_number=table_number+1
    
    hbdpar(1,k1)=grd_fm_sre(kbndz(2,k1)) !< first s1 point on the inside of the domain
    hbdpar(2,k1)=1
    hbdpar(3,k1)=table_number
    
    !FM1DIMP2DO: make this a subroutine? called in every loop for every BC
    ntab(1,table_number)=table_length !length of table 
    ntab(2,table_number)=table_number*table_length-1 !start address X
    ntab(3,table_number)=maxtab*table_length+table_number*table_length-1 !start address Y
    ntab(4,table_number)=0 !access method (0=continuous interpolation)
end do

!q
do k1=1,nqstat
    table_number=table_number+1
    
    qbdpar(1,k1)=grd_fm_sre(kbndu(2,k1)) !< first s1 point on the inside of the domain
    qbdpar(2,k1)=1
    qbdpar(3,k1)=table_number !< table number after the ones of <hbdpar>
    
    ntab(1,table_number)=table_length !length of table 
    ntab(2,table_number)=table_number*table_length-1 !start address X
    ntab(3,table_number)=maxtab*table_length+table_number*table_length-1 !start address Y
    ntab(4,table_number)=0 !access method (0=continuous interpolation)
end do

     !hbdpar(3,nhstat)  Hydrodynamic conditions for H-stations:
     !    (1,i) = Location [grid point] for H-station.
     !    (2,i) = Type of condition
     !            cbftim (1) : h = f(t)
     !            cbfqoh (2) : h = h(Q)
     !            cbfour (3) : h = fourier
     !            cbtidl (4) : h = tidal components
     !    (3,i) = Table number for f(t), h(Q), fourier
     !            or tidal components table.

end subroutine inifm1dimp_fbc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fnod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_fnod(iresult)

use m_f1dimp
use unstruc_channel_flow, only: network
use unstruc_messages

implicit none

!
!pointer
!

integer                                  , pointer :: nbran   
integer                                  , pointer :: nnode
integer                                  , pointer :: nhstat
integer                                  , pointer :: nqstat

integer, dimension(:)                    , pointer :: numnod
integer, dimension(:)                    , pointer :: grd_fm_sre
integer, dimension(:,:)                  , pointer :: hbdpar
integer, dimension(:,:)                  , pointer :: qbdpar
integer, dimension(:,:)                  , pointer :: node
integer, dimension(:,:)                  , pointer :: nodnod

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kbr, knod, k2
integer :: idx_fr, idx_to

integer, allocatable, dimension(:)   :: kcol !saves the index to write in the row of <nonnod>

!----------------------------------------
!BEGIN POINT
!----------------------------------------

nnode                  => f1dimppar%nnode
nbran                  => f1dimppar%nbran
nhstat                 => f1dimppar%nhstat
nqstat                 => f1dimppar%nqstat
grd_fm_sre             => f1dimppar%grd_fm_sre
qbdpar                 => f1dimppar%qbdpar
hbdpar                 => f1dimppar%hbdpar
node                   => f1dimppar%node
numnod                 => f1dimppar%numnod
nodnod                 => f1dimppar%nodnod

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

!nodes
do knod=1,nnode
    !f1dimppar%node(1,k)= 
    !for some reason at this stage <network%NDS%NODE%NODETYPE> is only either 0 or 1
    !hence, if type 0 it means BC and I have to search which one is it
    !network%NDS%NODE%NODETYPE
    !! - -2    boundary node
    !! - -1    not set
    !! -  0    node with one reach connected
    !! -  1    connection node with more than one reach connected
    !! -  2    water level boundary
    !! -  3    Discharge boundary 
    !! -  4    Discharge boundary as tabulated function of water level
    !! -  5    Embedded node
    if (network%NDS%NODE(knod)%NODETYPE .eq. 0) then !BC noce
        
        do k2=1,nhstat !search in hbdpar
            if (hbdpar(1,k2) .eq. grd_fm_sre(network%NDS%NODE(knod)%GRIDNUMBER)) then
                node(1,knod)=2 !H boundary
                node(2,knod)=hbdpar(1,k2) !gridpoint
                node(3,knod)=k2
                node(4,knod)=1 !not sure what should it be
            endif
        enddo !nhstat
        
        if (node(1,knod) .eq. -999) then !it is not hbdpar, we search in qbdpar
        
            do k2=1,nqstat !search in hbdpar
                if (qbdpar(1,k2) .eq. grd_fm_sre(network%NDS%NODE(knod)%GRIDNUMBER)) then
                    node(1,knod)=3 !Q boundary
                    node(2,knod)=qbdpar(1,k2) !gridpoint
                    node(3,knod)=k2
                    node(4,knod)=1 !not sure what should it be
                endif
            enddo
        
        endif
        
        if (node(1,knod) .eq. 0) then !it is not hbdpar nor qbdpar => error
            write (msgbuf, '(a)') 'There is a node which is neither internal nor H nor Q boundary.'
            call err_flush()
            iresult=1
        endif
        
    elseif (network%NDS%NODE(knod)%NODETYPE .eq. 1) then !internal node
        node(1,knod)=1
        !node(2:end) is undefined if internal node
    else
        write (msgbuf, '(a)') 'The type of node is not what I expected.'
        call err_flush()
        iresult=1
    endif
    
    numnod(knod)=network%NDS%NODE(knod)%NUMBEROFCONNECTIONS+1
    
end do

!nodes

!<kcol> saves the index to write in the row of <nonnod>
if (allocated(kcol)) then
    deallocate(kcol)
endif 
allocate(kcol(nnode))
kcol=2 !first index is filled with its own node

!filling first index with its own node
do knod=1,nnode
    nodnod(knod,1)=knod
enddo

do kbr=1,nbran
    
    idx_fr=network%BRS%BRANCH(kbr)%NODEINDEX(1)
    idx_to=network%BRS%BRANCH(kbr)%NODEINDEX(2)
    
    nodnod(idx_fr,kcol(idx_fr))=idx_to
    kcol(idx_fr)=kcol(idx_fr)+1
    
    nodnod(idx_to,kcol(idx_to))=idx_fr
    kcol(idx_to)=kcol(idx_to)+1
    
enddo

if (allocated(kcol)) then
    deallocate(kcol)
endif 

end subroutine inifm1dimp_fnod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_chk
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inifm1dimp_chk(iresult)

use m_f1dimp
use m_fm_erosed, only: lsedtot, ndx_mor, frac
use m_sediment, only: jased, stm_included
use unstruc_messages

implicit none

!
!pointer
!

integer                                  , pointer :: ngrid 

integer, dimension(:)                    , pointer :: grd_sre_cs

!
!output
!

integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!local
!

integer :: kd, ksed

integer, dimension(1) :: idx_findloc

!----------------------------------------
!BEGIN POINT
!----------------------------------------

ngrid                  => f1dimppar%ngrid
grd_sre_cs             => f1dimppar%grd_sre_cs

!----------------------------------------
!BEGIN CALC
!----------------------------------------

iresult=0 !no error

!all cross-sections must be mapped to an SRE gridpoint
do kd=1,ngrid
    idx_findloc=findloc(grd_sre_cs,kd)
    if (idx_findloc(1).eq.0) then
        write (msgbuf, '(a)') 'Not all SRE nodes are related to a cross-section.'
        call err_flush()
        iresult=1
    endif
enddo

if (jased > 0 .and. stm_included) then !passing if no morphpdynamics
    
!FM1DIMP2DO: for some strange reason <frac> seems to not be always fine. I do not know why. 
!if repeating the run in debug mode, the problem is not there. 
      
do kd=1,ndx_mor
    do ksed=1,lsedtot
        if (frac(kd,ksed)>1.00001) then
            write (msgbuf, '(a)') 'Something is wrong with <frac>.'
            call err_flush()
            iresult=1
        endif
    enddo !ksed
enddo!kd

endif !jased

end subroutine inifm1dimp_chk

end module m_initialize_flow1d_implicit