!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!> Module containing the subroutines called in initializing <flow1d_implicit>
!! This module resides in <dflowfm_data> and hence has access to all other
!! data. This is contrary to module <m_f1dimp> which resides in project
!! <flow1d_implicit> and only has access to the variables in that project.
module m_initialize_flow1d_implicit
   use m_flow_sedmorinit, only: flow_sedmorinit
   use m_init_1dinfo, only: init_1dinfo

contains

   !> Subroutine to initialize the 1D flow model.
   !! This subroutine serves as the main entry point for initializing the 1D flow model.
   !! It sequentially calls other subroutines to perform specific initialization tasks,
   !! such as allocating arrays, setting up grid points, initializing boundary conditions,
   !! and validating the setup. Each step ensures that the model is properly configured
   !! before simulation begins.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine initialize_flow1d_implicit(iresult)

      implicit none

!output
      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      call inifm1dimp_ini(iresult) !INItialize arrays
      call inifm1dimp_lob(iresult) !Loop On Branches
      call inifm1dimp_faap(iresult) !Fill Arrays that need Additional Point
      call inifm1dimp_fic(iresult) !Fill Initial Condition
      call inifm1dimp_fbrp(iresult) !Fill Branch PRoperties
      call inifm1dimp_fbc(iresult) !Fill Boundary Conditions
      call inifm1dimp_fstruc(iresult) !Fill STRUCtures
      call inifm1dimp_fnod(iresult) !Fill NODes
      call inifm1dimp_chk(iresult) !CHecK

   end subroutine initialize_flow1d_implicit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_ini
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to initialize arrays for the 1D flow model.
   !! This subroutine handles the allocation and initialization of various arrays
   !! required for the 1D flow model. It sets up pointers, allocates memory for
   !! arrays, and initializes parameters such as boundary conditions, grid points,
   !! and morphodynamic variables. It also performs error handling and ensures
   !! that all necessary data structures are properly initialized before further
   !! computations.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_ini(iresult)

      use m_f1dimp
      use m_physcoef, only: ag, rhomean
      use m_flowgeom, only: ndx, ndxi, lnx, tnode, lnx1D, ln, lnxi, nd, lnx1Db
      use fm_external_forcings_data, only: nzbnd, nqbnd
      use m_fm_erosed, only: nd_mor, ln_mor
      use unstruc_channel_flow, only: network
      use messagehandling, only: msgbuf, err_flush
      use m_sediment, only: stmpar, jased, stm_included
      use m_fm_erosed, only: link1sign, link1sign2
      use m_oned_functions, only: gridpoint2cross, t_gridp2cs
      use m_alloc, only: realloc

      implicit none

!
!pointer
!

      integer, pointer :: ngrid
      integer, pointer :: ngridm
      integer, pointer :: nbran
      integer, pointer :: maxlev
      integer, pointer :: nnode
      integer, pointer :: nhstat
      integer, pointer :: nqstat
      integer, pointer :: maxtab
      integer, pointer :: ntabm
      integer, pointer :: nbrnod
      integer, pointer :: table_length
      integer, pointer :: juer
      integer, pointer :: number_bc_tables
      integer, pointer :: nstru
      integer, pointer :: dmstrpar
      integer, pointer :: table_length_stru

      integer, dimension(:), pointer :: grd_ghost_link_closest
      integer, dimension(:), pointer :: grd_fmmv_fmsv

      integer, dimension(:, :), pointer :: node

!
!allocatable
!

      type(t_gridp2cs), dimension(:), allocatable :: gridpoint2cross_o

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

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

      table_length => f1dimppar%table_length
      maxtab => f1dimppar%maxtab
      number_bc_tables => f1dimppar%number_bc_tables
      nnode => f1dimppar%nnode
      ntabm => f1dimppar%ntabm
      nbran => f1dimppar%nbran
      ngrid => f1dimppar%ngrid
      nbrnod => f1dimppar%nbrnod
      nstru => f1dimppar%nstru
      dmstrpar => f1dimppar%dmstrpar
      maxlev => f1dimppar%maxlev
      ngridm => f1dimppar%ngridm
      nhstat => f1dimppar%nhstat
      nqstat => f1dimppar%nqstat
      grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
      grd_fmmv_fmsv => f1dimppar%grd_fmmv_fmsv
      juer => f1dimppar%juer
      node => f1dimppar%node
      table_length_stru => f1dimppar%table_length_stru

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

!file for error
!FM1DIMP2DO. Ideally we would use the message handlinf of FM. This implies changing all calls in SRE and make sure the message handling error module is accessible.
!Furthermore, closing of the file should be dealt with. I am not sure where to place it.
      open (newunit=juer, file="FM1DIMP.dia", status="replace", action="write", iostat=stat, iomsg=msg)

      f1dimp_initialized = .true. !we use this for using <ndx> rather than <ndx_mor> (which is not defined yet) in <flow_sedmorinit>. See comment there.

!parameters in <flwpar> (SRE variable)
      f1dimppar%g = ag
      f1dimppar%rhow = rhomean

!<SOFLOW> input
      f1dimppar%steady = .true.

!dimensions
      nbran = network%brs%count

      ngrid = 0
      ngridm = 0
      nstru = network%sts%count
      nbrnod = network%nds%maxnumberofconnections
      dmstrpar = 21 !size of structure parameters
!nlink
      do kbr = 1, nbran
         ngrid = ngrid + network%brs%branch(kbr)%gridpointscount
         ngridm = max(ngridm, network%brs%branch(kbr)%gridpointscount)
         !nlink=nlink+network%brs%branch(k)%upointscount
      end do

      ndx_max = ndx + network%nds%count !maximum number of multivalued flownodes
      lnx_max = lnx + network%nds%maxnumberofconnections * network%nds%count !maximum number of links considering added ghost links

      maxlev = 0
      do k1 = 1, network%csdefinitions%count
         maxlev = max(maxlev, network%csdefinitions%cs(1)%levelscount)
      end do

      nnode = network%nds%count
      nhstat = nzbnd
      nqstat = nqbnd
      number_bc_tables = nhstat + nqstat
!if (comparereal(nzbnd+nqbnd,ndx-ndxi,1d-10)/=0) then !FM1DIMP2DO: why does the compiler complain when using <comparereal>?
      if ((nzbnd + nqbnd) /= (ndx - ndxi)) then
         write (msgbuf, '(a)') 'Number of open boundaries is different than number of water level + discharge boundaries'
         call err_flush()
         iresult = 1
      end if

!table
      !BC
      table_length = 2 !length of each boundary conditions table. All have only 2 times.
      maxtab = ndx - ndxi !<we have as many tables as open boundaries
      ntabm = maxtab * table_length * 2 !last 2 is for <time> and <values>

      !structures
      table_length_stru = 8
      maxtab = maxtab + 1
      ntabm = ntabm + table_length_stru * 2

!allocate
      call realloc(f1dimppar%grd_sre_fm, ngrid)
      call realloc(f1dimppar%grd_fm_sre, ndx_max) !we allocate more than we need. The maximum number of bifurcations and confluences is less than the number of nodes.
      f1dimppar%grd_fm_sre = 0
      call realloc(f1dimppar%grd_fm_sre2, ndx_max) !we allocate more than we need. The maximum number of bifurcations and confluences is less than the number of nodes.
      f1dimppar%grd_fm_sre2 = 0
      call realloc(f1dimppar%grd_fmL_sre, lnx1D, 2)
      call realloc(f1dimppar%branch, 4, nbran)
      call realloc(f1dimppar%x, ngrid)
      call realloc(f1dimppar%grid, ngrid)
      call realloc(f1dimppar%grd_sre_cs, ngrid)
      call realloc(f1dimppar%hpack, ngrid, 3)
      call realloc(f1dimppar%qpack, ngrid, 3)
      call realloc(f1dimppar%grd_fmLb_sre, lnx1Db - lnxi, 2)
      call realloc(f1dimppar%waoft, ngrid, 18)
      call realloc(f1dimppar%bfrict, 3, nbran)
      call realloc(f1dimppar%sectv, ngrid, 8)
      !`nd_mor` is a derived type. We cannot use `realloc` here.
      if (allocated(nd_mor)) then
         deallocate (nd_mor)
      end if
      allocate (nd_mor(ndx_max)) !more than we need
      do kd = 1, ndx
         nd_mor(kd) = nd(kd)
      end do
      call realloc(f1dimppar%grd_fmmv_fmsv, ndx_max) !more than we need
      grd_fmmv_fmsv => f1dimppar%grd_fmmv_fmsv
      !allocate every node with itself
      do kd = 1, ndx_max
         grd_fmmv_fmsv(kd) = kd
      end do
      call realloc(f1dimppar%strtyp, 10, nstru)
      call realloc(f1dimppar%strpar, dmstrpar, nstru)
      call realloc(ln_mor, 2, lnx_max)
      do kl = 1, lnx
         do kd = 1, 2
            ln_mor(kd, kl) = ln(kd, kl)
         end do
      end do
      call realloc(f1dimppar%grd_ghost_link_closest, lnx_max) !we allocate more than we need. The maximum number of bifurcations and confluences is less than the number of nodes.
      grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
      do kl = 1, lnx
         grd_ghost_link_closest(kl) = kl
      end do

!FM1DIMP2DO: I am now adapting the input for using the morphodynamic implementation of Pure 1D. However,
!I amnot sure it is the best. This should be revisited with Bert :).
      if (jased > 0 .and. stm_included) then !passing if no morphpdynamics
         stmpar%morpar%mornum%pure1d = .true.
         call init_1dinfo() !<initialize_flow1d_implicit> is called before <init_1dinfo>. We have to call it here and it will not be called again because it will be allocated.
      end if

      allocate (link1sign(lnx_max))
      link1sign = 1

      allocate (link1sign2(lnx_max))
      link1sign2 = 0
!All internal links have direction 1
      do kl = 1, lnxi
         link1sign2(kl) = 1
      end do

!copy to <gridpoint2cross_o>
      !`gridpoint2cross_o` is a derived type. No `realloc` possible.
      if (allocated(gridpoint2cross_o)) then
         deallocate (gridpoint2cross_o)
      end if
      allocate (gridpoint2cross_o(ndx_max))
      do kd = 1, ndxi
         gridpoint2cross_o(kd) = gridpoint2cross(kd)
         !a junction of only two branches has `num_cross_sections=2` but only one CS
         if ((gridpoint2cross_o(kd)%num_cross_sections == 1) .or. (gridpoint2cross_o(kd)%num_cross_sections > 2)) then
            do k2 = 1, gridpoint2cross_o(kd)%num_cross_sections
               if (gridpoint2cross_o(kd)%cross(k2) == -999) then
                  iresult = 1
               end if
            end do
         else !`num_cross_sections=2`
            if (gridpoint2cross_o(kd)%cross(1) == -999) then !the only one is always in position 1
               iresult = 1
            end if
         end if
      end do !kd
      if (iresult == 1) then
         write (msgbuf, '(a)') 'There is a node without cross-section.'
         call err_flush()
         return
      end if

      !`gridpoint2cross` is a derived type. No `realloc` possible.
      if (allocated(gridpoint2cross)) then
         deallocate (gridpoint2cross)
      end if
      allocate (gridpoint2cross(ndx_max))
!internal cross-sections are the same as they were (1 CS per flownode).
      do kd = 1, ndxi
         gridpoint2cross(kd) = gridpoint2cross_o(kd)
      end do
!at ghost-boundary flownodes we set the number of CS to 0 to prevent looping on them (there is no CS)
      do kd = ndxi + 1, ndx
         gridpoint2cross(kd)%num_cross_sections = 0 !This prevents it is looped in <fm_update_crosssections>
      end do

!
!gridpoints
!

      call realloc(f1dimppar%bfricp, 6, ngrid)
      call realloc(f1dimppar%nlev, ngrid)
      call realloc(f1dimppar%bedlevel, ngrid)
      call realloc(f1dimppar%sectc, ngrid, 3)
!
!cross-section (gridpoint,level)
!

      call realloc(f1dimppar%wft, ngrid, maxlev)
      call realloc(f1dimppar%aft, ngrid, maxlev)
      call realloc(f1dimppar%wtt, ngrid, maxlev)
      call realloc(f1dimppar%att, ngrid, maxlev)
      call realloc(f1dimppar%of, ngrid, maxlev)
      call realloc(f1dimppar%hlev, ngrid, maxlev)

!
!table information
!

      call realloc(f1dimppar%ntab, 4, maxtab)

!
!BC
!

      call realloc(f1dimppar%hbdpar, 3, nhstat)
      call realloc(f1dimppar%qbdpar, 3, nqstat)
      call realloc(f1dimppar%table, ntabm)

!
!node
!

      call realloc(f1dimppar%node, 4, nnode)
      node => f1dimppar%node
      node = -999 !we use this value to check that it has not been filled.
      call realloc(f1dimppar%numnod, nnode)
      call realloc(f1dimppar%nodnod, nnode, nbrnod + 1)

!
!network
!
      f1dimppar%network => network

!
!debug
!

      f1dimppar%fm1dimp_debug_k1 = 1

!
!deallocate locals
!

      if (allocated(gridpoint2cross_o)) then
         deallocate (gridpoint2cross_o)
      end if

   end subroutine inifm1dimp_ini

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_lob
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to loop over branches and initialize grid points and links for the 1D flow model.
   !! This subroutine processes each branch in the 1D flow model, assigning grid points, links, and
   !! cross-sections to the respective branches. It handles bifurcations, boundaries, and ghost links
   !! to ensure proper connectivity and initialization of the model. The subroutine also updates
   !! the dimensions of links and nodes to account for ghost links and multivalued nodes.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_lob(iresult)

      use m_f1dimp
      use m_flowgeom, only: ndx, lnx, ln, nd, tnode, lnxi, lnx1Db
      use unstruc_channel_flow, only: network
      use messagehandling, only: msgbuf, err_flush
      use m_fm_erosed, only: link1sign2, ndx_mor, lnx_mor, lnxi_mor, ndxi_mor, ln_mor, nd_mor, ndkx_mor
      use m_oned_functions, only: gridpoint2cross

      implicit none

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      integer, pointer :: nbran

      integer, dimension(:), pointer :: grd_sre_fm
      integer, dimension(:), pointer :: grd_fm_sre
      integer, dimension(:), pointer :: grd_fm_sre2
      integer, dimension(:), pointer :: grd_sre_cs
      integer, dimension(:), pointer :: grd_ghost_link_closest
      integer, dimension(:), pointer :: grd_fmmv_fmsv
      integer, dimension(:), pointer :: lin
      integer, dimension(:), pointer :: grd

      integer, dimension(:, :), pointer :: grd_fmL_sre
      integer, dimension(:, :), pointer :: grd_fmLb_sre
      integer, dimension(:, :), pointer :: branch

      real, dimension(:), pointer :: x

      type(tnode), allocatable :: nd_o(:) !Copy of <nd> for reworking <nd>

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: kbr, kn, kl
      integer :: c_lnx, c_ndx !counters
      integer :: idx_sre, idx_fm !indices
      integer :: n1, n2, pointscount, jpos
      integer :: idx_i, idx_f, nl, L, L2, idx_l1, idx_l2, idx_n
      integer :: nint, nout

!move to function
      integer :: idx_aux
      integer :: min_1, min_2, k2

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      nbran => f1dimppar%nbran
      grd_sre_cs => f1dimppar%grd_sre_cs
      grd_sre_fm => f1dimppar%grd_sre_fm
      grd_fm_sre => f1dimppar%grd_fm_sre
      grd_fm_sre2 => f1dimppar%grd_fm_sre2
      grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
      grd_fmmv_fmsv => f1dimppar%grd_fmmv_fmsv
      grd_fmL_sre => f1dimppar%grd_fmL_sre
      grd_fmLb_sre => f1dimppar%grd_fmLb_sre
      branch => f1dimppar%branch
      x => f1dimppar%x

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

      if (allocated(nd_o)) then
         deallocate (nd_o)
      end if
      allocate (nd_o(ndx))
      nd_o = nd

      idx_i = 1
      idx_sre = 0
      c_lnx = lnx
      c_ndx = ndx
      do kbr = 1, nbran
         idx_f = idx_i + network%brs%branch(kbr)%gridpointscount - 1 !update index final

         grd_sre_fm(idx_i:idx_f) = network%brs%branch(kbr)%grd
         x(idx_i:idx_f) = network%brs%branch(kbr)%gridpointschainages !chainage

         nl = network%brs%branch(kbr)%upointscount !only internal
         do kl = 1, nl
            L = network%brs%branch(kbr)%LIN(kl)
            grd_fmL_sre(L, :) = [idx_i + kl - 1, idx_i + kl]

            !FM1DIMP2DO: Do we need this?
            !search for the GRD with <n1>?
            n1 = ln(1, L)
            n2 = ln(2, L)
            if (.not. ((grd_sre_fm(grd_fmL_sre(L, 1)) == n1) .or. (grd_sre_fm(grd_fmL_sre(L, 1)) == n2))) then
               write (msgbuf, '(a)') 'Links and nodes do not match.'
               call err_flush()
               iresult = 1
            end if
            if (.not. ((grd_sre_fm(grd_fmL_sre(L, 2)) == n1) .or. (grd_sre_fm(grd_fmL_sre(L, 2)) == n2))) then
               write (msgbuf, '(a)') 'Links and nodes do not match.'
               call err_flush()
               iresult = 1
            end if
         end do !kl

         pointscount = network%brs%branch(kbr)%gridpointscount !FM1DIMP2DO: also make pointer?
         lin => network%brs%branch(kbr)%lin
         grd => network%brs%branch(kbr)%grd

         do kn = 1, pointscount
            idx_sre = idx_sre + 1
            idx_fm = grd(kn)

            !cross-section
            if (kn == 1 .or. kn == pointscount) then

               !FM1DIMP2DO: This part of the code is part of <set_cross_sections_to_gridpoints>, could be modularized.
               !search for index with the CS
               if (kn == 1) then
                  L = lin(1)
               else
                  L = lin(pointscount - 1)
               end if
               do kl = 1, nd_o(idx_fm)%lnx
                  if (L == abs(nd_o(idx_fm)%ln(kl))) then
                     jpos = kl
                  end if
               end do !kl

               !add ghost link
               if (nd(idx_fm)%lnx > 2) then !bifurcation

                  !-------link
                  c_lnx = c_lnx + 1 !update link number to ghost link

                  grd_ghost_link_closest(c_lnx) = abs(nd_o(idx_fm)%ln(jpos))

                  !In <nd> we keep the junction node <idx_fm> connected to several branches via the ghost link, as <nd> is used for the nodal point relation.

                  !FM1DIMP2DO: It seems all links point toward a junction in the standard scheme (based on nodal point relation). Is it true?
                  if (kn == 1) then
                     link1sign2(c_lnx) = 1 !link direction for morphodynamics
                     nd(idx_fm)%ln(jpos) = -c_lnx !set ghost link as the one connected to junction flownode
                  else
                     link1sign2(c_lnx) = 1
                     nd(idx_fm)%ln(jpos) = c_lnx !set ghost link as the one connected to junction flownode
                  end if

                  !-------node
                  c_ndx = c_ndx + 1 !update node number to multivalued node

                  !save the flownode closest to the junction node in the branch under consideration
                  !and
                  !change the flownode connected to the first link connected to the junction flownode along the branch under consideration to the new flownode
                  n1 = ln(1, grd_ghost_link_closest(c_lnx)) !flownode 1 associated to new link
                  n2 = ln(2, grd_ghost_link_closest(c_lnx)) !flownode 2 associated to new link
                  !either <n1> or <n2> is the junction node <idx_fm>. We take the other one.
                  if (idx_fm == n1) then
                     grd_fmmv_fmsv(c_ndx) = n2
                     ln_mor(1, grd_ghost_link_closest(c_lnx)) = c_ndx
                  else
                     grd_fmmv_fmsv(c_ndx) = n1
                     ln_mor(2, grd_ghost_link_closest(c_lnx)) = c_ndx
                  end if

                  nd_mor(c_ndx)%lnx = 2 !in <nd_mor> only two links are connected to each node. For ghost nodes these are:
                  allocate (nd_mor(c_ndx)%ln(2))
                  if (kn == 1) then
                     nd_mor(c_ndx)%ln(1) = c_lnx !new ghost link
                     nd_mor(c_ndx)%ln(2) = -grd_ghost_link_closest(c_lnx) !existing link
                     ln_mor(1, c_lnx) = c_ndx
                     ln_mor(2, c_lnx) = grd_fmmv_fmsv(c_ndx)
                  else
                     nd_mor(c_ndx)%ln(1) = grd_ghost_link_closest(c_lnx) !existing link
                     nd_mor(c_ndx)%ln(2) = -c_lnx !new ghost link
                     ln_mor(2, c_lnx) = c_ndx
                     ln_mor(1, c_lnx) = grd_fmmv_fmsv(c_ndx)
                  end if

                  grd_fm_sre(c_ndx) = idx_sre

                  !node <idx_fm> (at the junction) does not play any role anymore in <nd_mor>. Still,
                  !we save here the index of one of the SRE points associated
                  !to it for the sake of writing a value for output.
                  grd_fm_sre(idx_fm) = idx_sre

                  !add CS at multivalued-ghost flownode
                  gridpoint2cross(c_ndx)%num_cross_sections = 1
                  allocate (gridpoint2cross(c_ndx)%cross(gridpoint2cross(c_ndx)%num_cross_sections))
                  gridpoint2cross(c_ndx)%cross(1) = gridpoint2cross(idx_fm)%cross(jpos)

                  !remove CS at junction flownode
                  gridpoint2cross(idx_fm)%num_cross_sections = 0 !This prevents it is looped in <fm_update_crosssections>
                  !gridpoint2cross(idx_fm)%cross(jpos)=-999 !This prevents it is passed in <fm_update_crosssections> -> NO. -999 causes error when parsing the number of CS per node.

               else !not a bifurcation (i.e., boundary)

                  grd_fmmv_fmsv(idx_fm) = idx_fm !the closest value is itself

                  !if <grd_fm_sre(idx_fm)> is not 0, it has already been filled. This implies
                  !it is a flownode in a junction of just two branches. We have to save both
                  !sre indices for filling the initial condition.
                  if (grd_fm_sre(idx_fm) /= 0) then
                     grd_fm_sre2(idx_fm) = idx_sre
                  else
                     grd_fm_sre(idx_fm) = idx_sre
                  end if

                  !relate ghost flownode also to <idx_sre>
                  idx_l1 = abs(nd_o(idx_fm)%ln(1))
                  idx_l2 = abs(nd_o(idx_fm)%ln(2))
                  !there are only two links
                  L = max(idx_l1, idx_l2) !the one which is external (the largest of the two) points to the ghost flownode
                  L2 = min(idx_l1, idx_l2) !the one which is internal (the smallest of the two) points to the internal cell
                  n1 = ln(1, L)
                  n2 = ln(2, L)
                  idx_n = max(n1, n2) !the maximum flownode is the ghost one
                  grd_fm_sre(idx_n) = idx_sre

                  !link direction for morphodynamics
                  link1sign2(L2) = 1
                  if (kn == 1) then
                     link1sign2(L) = 1
                  else
                     link1sign2(L) = -1
                  end if

               end if !(nd(idx_fm)%lnx>2)

            else !internal point of a branch, not beginning or end.
               jpos = 1

               grd_fmmv_fmsv(idx_fm) = idx_fm !the closest value is itself
               grd_fm_sre(idx_fm) = idx_sre

            end if

            !FM1DIMP2DO: I wonder whether we need this or we can use the adapted <gridpoint2cross> in which there is a cross-section for 1:ndx_mor
            grd_sre_cs(idx_sre) = gridpoint2cross(idx_fm)%cross(jpos) !cross-section index associated to the FM gridpoint per branch

            !if there is not a unique cross-section per gridpoint per branch, <ic=-999>. It is not needed to check
            !this here because it is already checked in <flow_sedmorinit>, which is called before <initialize_flow1d_implicit>

         end do !kn

         !branch
         branch(1, kbr) = network%brs%branch(kbr)%nodeindex(1)
         branch(2, kbr) = network%brs%branch(kbr)%nodeindex(2)
         branch(3, kbr) = idx_i
         branch(4, kbr) = idx_f

         !update index initial
         idx_i = idx_f + 1
      end do !branch

!new dimensions
      lnx_mor = c_lnx !store new number of links (considering ghost links)
      lnxi_mor = lnx_mor !there are no ghosts in SRE
      ndx_mor = c_ndx !store new number of flow nodes (considering multivaluedness)
      ndxi_mor = ndx_mor !there are no ghosts in SRE
      ndkx_mor = ndx_mor

!creart
      idx_fm = 0
      do L = lnxi + 1, lnx1Db !boundary links
         idx_fm = idx_fm + 1
         n1 = ln(1, L)
         n2 = ln(2, L)
         nint = min(n1, n2) !from the two cells that this link connects, the minimum is internal, and hence we have data
         nout = max(n1, n2) !from the two cells that this link connects, the maximum is extrernal, and it is the one in which we have to set the water level

         !FM1DIMP2DO: move to function or search for smarter way
         !grd_fmLb_sre(k,1)=findloc(grd_sre_fm,nint) !sre index with <nint> FM value !not working fine due to type of array I guess.
         idx_aux = 1
         min_1 = abs(grd_sre_fm(1) - nint)
         do k2 = 2, size(grd_sre_fm)
            min_2 = abs(grd_sre_fm(k2) - nint)
            if (min_2 < min_1) then
               min_1 = min_2
               idx_aux = k2
            end if
         end do

         grd_fmLb_sre(idx_fm, 1) = idx_aux !SRE index of the boundary cell
         grd_fmLb_sre(idx_fm, 2) = nout !FM index of the ghost cell centre associated to link <L>

         !mask grid
         !kcs_sre(idx_aux)=-1 !FM1DIMP2DO: I am not sure I need this or I better deal with directions in <fm_erosed> and here just set to 1 but the right dimensions.
      end do

!deallocate

      if (allocated(nd_o)) then
         deallocate (nd_o)
      end if

   end subroutine inifm1dimp_lob

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_faap
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to fill arrays that need additional points in the 1D flow model.
   !! This subroutine handles the allocation and initialization of various arrays
   !! required for the 1D flow model, including those related to nodes, links,
   !! and morphodynamic variables. It ensures that the arrays are properly
   !! resized and filled with appropriate values, taking into account ghost links
   !! and nodes for bifurcations and boundaries.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_faap(iresult)
      use precision, only: dp

      use m_f1dimp
      use m_flowgeom, only: ndx, bai_mor, ba, bl, dx, lnx, dxi, acl, wu, snu, csu, wu_mor, wcx1, wcx2, wcy1, wcy2, kcu, wcl, lnxi, griddim
      use m_flow, only: s0, s1, u1, v, au, hu, qa, frcu_mor, frcu, z0urou, ifrcutp, taubxu, ucx_mor, ucy_mor, ustb, z0ucur
      use m_sediment, only: stmpar, jased, stm_included, kcsmor
      use m_fm_erosed, only: ndx_mor, lsedtot, lnx_mor, pmcrit, link1, ln_mor, hs_mor, ucxq_mor, ucyq_mor, uau
      use m_turbulence, only: rhowat
      use m_xbeach_data, only: ktb
      use m_bedform, only: bfmpar

      implicit none

!
!pointer
!
      integer, pointer :: nlyr

      integer, dimension(:), pointer :: grd_ghost_link_closest
      integer, dimension(:), pointer :: grd_fmmv_fmsv

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: kl, kd, k1

      real(kind=dp), allocatable, dimension(:, :) :: bodsed_o
      real(kind=dp), allocatable, dimension(:, :) :: thlyr_o
      real(kind=dp), allocatable, dimension(:, :) :: sedshort_o
      real(kind=dp), allocatable, dimension(:, :) :: svfrac_o
      real(kind=dp), allocatable, dimension(:, :) :: preload_o

      real(kind=dp), allocatable, dimension(:, :, :) :: msed_o

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      grd_ghost_link_closest => f1dimppar%grd_ghost_link_closest
      grd_fmmv_fmsv => f1dimppar%grd_fmmv_fmsv

!stmpar
      if (jased > 0 .and. stm_included) then !passing if no morphpdynamics
         nlyr => stmpar%morlyr%settings%nlyr
      end if

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

!FM1DIMP2DO: If friction varies with time, <frcu_mor> is updated. The subroutine that
!does that must be modified to also adapt the friction in the ghost links.

!this cannot be in the allocation function because it needs <ln_mor>
      allocate (link1(ndx_mor))
      link1 = 0
      do kl = 1, lnx_mor
         k1 = ln_mor(1, kl)
         !k2 = ln(2,kl)
         link1(k1) = kl
      end do

!nodes
      !allocate
      call reallocate_fill(s0, grd_fmmv_fmsv, ndx, ndx_mor)
      call reallocate_fill(s1, grd_fmmv_fmsv, ndx, ndx_mor)
      call reallocate_fill(bai_mor, grd_fmmv_fmsv, ndx, ndx_mor)
      call reallocate_fill(ba, grd_fmmv_fmsv, ndx, ndx_mor)
      call reallocate_fill(rhowat, grd_fmmv_fmsv, ndx, ndx_mor)
      call reallocate_fill(ktb, grd_fmmv_fmsv, ndx, ndx_mor) !FM1DIMP2DO: It could be better to allocate the wave part with <ndx_mor>. To limit the mess it is now done here.
      call reallocate_fill(bl, grd_fmmv_fmsv, ndx, ndx_mor)
!FM1DIMP2DO
!The value of <bl> here is not correct, as it copies the value from the closest node.
!However, this is not a big issue, as this value is only used for checking whether flow
!depth is above or below threshold. The bed level value for computing flow comes from
!the cross-sections, which are treated independently. Nevertheless, we could here fill
!the right value from cross-sections.

      call reallocate_fill_pointer(bfmpar%rksr, grd_fmmv_fmsv, ndx, ndx_mor)

!FM1DIMP2DO: When not having a morhodynamic simulation, morpho variables are not initialized. The best
!would be to <return> in <reallocate_fill>

      if (jased > 0 .and. stm_included) then !passing if no morphpdynamics

         call reallocate_fill_pointer(stmpar%morlyr%settings%thtrlyr, grd_fmmv_fmsv, ndx, ndx_mor)
         call reallocate_fill_pointer(stmpar%morlyr%settings%thexlyr, grd_fmmv_fmsv, ndx, ndx_mor)

         call reallocate_fill_pointer(ucxq_mor, grd_fmmv_fmsv, ndx, ndx_mor)
         call reallocate_fill_pointer(ucyq_mor, grd_fmmv_fmsv, ndx, ndx_mor)
         call reallocate_fill_pointer(hs_mor, grd_fmmv_fmsv, ndx, ndx_mor)

         call reallocate_fill(ucx_mor, grd_fmmv_fmsv, ndx, ndx_mor)
         call reallocate_fill(ucy_mor, grd_fmmv_fmsv, ndx, ndx_mor)

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
            deallocate (bodsed_o)
         end if
         allocate (bodsed_o(lsedtot, ndx))
         bodsed_o = stmpar%morlyr%state%bodsed

         if (stmpar%morlyr%settings%iunderlyr == 2) then

            if (allocated(msed_o)) then
               deallocate (msed_o)
            end if
            allocate (msed_o(lsedtot, nlyr, ndx))
            msed_o = stmpar%morlyr%state%msed

            if (allocated(thlyr_o)) then
               deallocate (thlyr_o)
            end if
            allocate (thlyr_o(nlyr, ndx))
            thlyr_o = stmpar%morlyr%state%thlyr

            if (allocated(sedshort_o)) then
               deallocate (sedshort_o)
            end if
            allocate (sedshort_o(lsedtot, ndx))
            sedshort_o = stmpar%morlyr%state%sedshort

            if (allocated(svfrac_o)) then
               deallocate (svfrac_o)
            end if
            allocate (svfrac_o(nlyr, ndx))
            svfrac_o = stmpar%morlyr%state%svfrac

            if (allocated(preload_o)) then
               deallocate (preload_o)
            end if
            allocate (preload_o(nlyr, ndx))
            preload_o = stmpar%morlyr%state%preload

         end if !underlayer==2

      end if !jased

!links
      !allocate
      call reallocate_fill(u1, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(v, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(au, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(hu, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(dx, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(dxi, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(wu, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(frcu_mor, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(frcu, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(qa, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(acl, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(snu, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(csu, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(wu_mor, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(z0urou, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(z0ucur, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(taubxu, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(wcx1, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(wcx2, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(wcy1, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(wcy2, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill(ustb, grd_ghost_link_closest, lnx, lnx_mor)

      call reallocate_fill_int(ifrcutp, grd_ghost_link_closest, lnx, lnx_mor)
      call reallocate_fill_int(kcu, grd_ghost_link_closest, lnx, lnx_mor)

!multidimensional links

      !copy arrays to temporary array
      if (allocated(wcl)) then
         deallocate (wcl)
      end if
      allocate (wcl(2, lnx_mor))
      do kl = 1, lnx_mor
         do kd = 1, 2
            wcl(kd, kl) = 0.5_dp !each link corresponds to 50% of the flownode.
         end do
      end do
      do kl = lnxi + 1, lnx
         wcl(1, kl) = 1.0_dp !boundary links are full
      end do

!morphodynamics initialization is done before fm1dimp initialization. Hence, we have to reallocate here using <ndx_mor> and <lnx_mor>
!It must be before the calls to <reallocate_~> because some variables (e.g., <ucxq_mor>) are set to the wrong size (i.e., <ndkx>) in <allocsedtra>
!We have to copy <bodsed> and other state variables before <flow_sedmorinit>.

      if (jased > 0 .and. stm_included) then !passing if no morphpdynamics

         stmpar%morlyr%settings%nmub = ndx_mor !size over which we will loop over morphodynamic variables
         griddim%nmub = ndx_mor !size of the variables when reading morphodynamic input
         call flow_sedmorinit()
         griddim%nmub = ndx !restore to the original size because it is used for exporting data

         stmpar%morpar%mornum%pure1d = .true. !we have set it for reading <init_1dinfo> but we have to set it again because it is overwritten in <flow_sedmorinit>

!FM1DIMP2DO: We could do the same trick and call <lnx_mor> in <flow_waveinit>, but some variables have been moved to another module after JR merge. Hence, we reallocate in this routine.
!call flow_waveinit()

!needs to be after <flow_sedmorinit>, where it is allocated.
!FM1DIMP2DO: Ideally, these variables are allocated in <flow_sedmorinit>
         call reallocate_fill_pointer(pmcrit, grd_fmmv_fmsv, ndx, ndx_mor)
         call reallocate_fill_pointer(stmpar%morlyr%state%dpsed, grd_fmmv_fmsv, ndx, ndx_mor)
         call reallocate_fill_int(kcsmor, grd_fmmv_fmsv, ndx, ndx_mor)

         call reallocate_fill_pointer(uau, grd_ghost_link_closest, lnx, lnx_mor)

         call reallocate_fill_manual_2(stmpar%morlyr%state%bodsed, bodsed_o, grd_fmmv_fmsv, ndx, ndx_mor, lsedtot)

         if (stmpar%morlyr%settings%iunderlyr == 2) then

            call reallocate_fill_manual_2(stmpar%morlyr%state%sedshort, sedshort_o, grd_fmmv_fmsv, ndx, ndx_mor, lsedtot)

            call reallocate_fill_manual_2(stmpar%morlyr%state%thlyr, thlyr_o, grd_fmmv_fmsv, ndx, ndx_mor, nlyr)
            call reallocate_fill_manual_2(stmpar%morlyr%state%svfrac, svfrac_o, grd_fmmv_fmsv, ndx, ndx_mor, nlyr)
            call reallocate_fill_manual_2(stmpar%morlyr%state%preload, preload_o, grd_fmmv_fmsv, ndx, ndx_mor, nlyr)

            call reallocate_fill_manual_3(stmpar%morlyr%state%msed, msed_o, grd_fmmv_fmsv, ndx, ndx_mor, lsedtot, nlyr)

         end if !underlayer=2

         ucyq_mor = 0_dp !set to 0 once rather than every time step. Somewhere in the code is changed. I have to set it every time step.

!deallocate

         if (allocated(bodsed_o)) then
            deallocate (bodsed_o)
         end if

         if (allocated(msed_o)) then
            deallocate (msed_o)
         end if

         if (allocated(thlyr_o)) then
            deallocate (thlyr_o)
         end if

         if (allocated(sedshort_o)) then
            deallocate (sedshort_o)
         end if

         if (allocated(svfrac_o)) then
            deallocate (svfrac_o)
         end if

         if (allocated(preload_o)) then
            deallocate (preload_o)
         end if

      end if

   end subroutine inifm1dimp_faap

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fic
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to fill initial conditions for the 1D flow model.
   !! This subroutine initializes the water levels and discharges at each grid point
   !! in the 1D flow model. It ensures that the initial conditions are properly set
   !! for both internal nodes and boundary nodes, including handling special cases
   !! such as junctions of two branches and junctions with more than two branches.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_fic(iresult)
      use precision, only: dp

      use m_f1dimp
      use m_flowgeom, only: ndx, ndxi, wu, nd
      use unstruc_channel_flow, only: network
      use m_flow, only: s1, au, u1
      use m_fm_erosed, only: ndx_mor, nd_mor, ln_mor

      implicit none

!
!pointer
!

      integer, dimension(:), pointer :: grd_fm_sre
      integer, dimension(:), pointer :: grd_fm_sre2

      real, dimension(:, :), pointer :: waoft

      real(kind=dp), dimension(:, :), pointer :: hpack
      real(kind=dp), dimension(:, :), pointer :: qpack

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: kd, idx_fm, k1, idx_sre, idx_l1, idx_l2, k2, kn, kl, n1, n2, L, idx_n

      integer :: swaoft

      real(kind=dp) :: wu_int, au_int

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      grd_fm_sre => f1dimppar%grd_fm_sre
      grd_fm_sre2 => f1dimppar%grd_fm_sre2
      hpack => f1dimppar%hpack
      qpack => f1dimppar%qpack
      waoft => f1dimppar%waoft

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

      swaoft = size(waoft, dim=2)

!Data must be available already at ghost links and nodes

!do ksre=1,ngrid
      do kd = 1, ndx_mor

         idx_fm = kd

         !do not overwrite the values at the boundary
         !which are correctly written above (LOB) and would be
         !filled incorrectly as the velocity at the ghost link
         !in the boundaries is 0.
         if (idx_fm > ndxi .and. idx_fm < ndx + 1) then
            cycle
         end if

         if (nd_mor(idx_fm)%lnx /= 2) then
            cycle
         end if

         !This is not the nicest, but it works. It is necessary to
         !fill the initial condition correctly at junctions of only
         !two branches. We loop twice. The first time we use the regular
         !map between fm and sre <grd_fm_sre>. The second time, if the
         !index in the auxiliary array <grd_fm_sre2> is not 0 it means
         !that this FM flownode is of a junction of two branhces.
         !Hence, there are two gridpoints associated to it that must be
         !filled.

         do k1 = 1, 2
            select case (k1)
            case (1)
               idx_sre = grd_fm_sre(idx_fm)
            case (2)
               idx_sre = grd_fm_sre2(idx_fm)
               if (idx_sre == 0) then
                  cycle
               end if
            end select

            !links connected to a given fm grid node
            idx_l1 = abs(nd_mor(idx_fm)%ln(1))
            idx_l2 = abs(nd_mor(idx_fm)%ln(2))

            !initial condition
            do k2 = 1, 3 !< time step in SRE [before, intermediate, after]
               !water level
               hpack(idx_sre, k2) = s1(idx_fm)
               !discharge
               qpack(idx_sre, k2) = 0.5 * (au(idx_l1) * u1(idx_l1) + au(idx_l2) * u1(idx_l2))
            end do !k2

            !waoft
            wu_int = 0.5 * (wu(idx_l1) + wu(idx_l2))
            au_int = 0.5 * (au(idx_l1) + au(idx_l2))

            !FM1DIMP2DO: needs to be separated between flow and total
            !check right order in <FLNORM> and not in documentation.

            waoft(idx_sre, 1) = real(wu_int) !wf = actual flow width
            waoft(idx_sre, 2) = real(wu_int) !wt = actual total width
            waoft(idx_sre, 3) = real(au_int) !af = actual flow area
            waoft(idx_sre, 4) = real(au_int) !at = actual total area n
            waoft(idx_sre, 5) = real(au_int) !at = actual total area n+1
            waoft(idx_sre, 6) = real(au_int / wu_int) !o = actual wetted perimeter
            do k2 = 7, swaoft
               waoft(idx_sre, k2) = 0
            end do !k2
         end do !k1
      end do !ksre

!at junctions of more than 2 branches we set the same water level for
!all SRE nodes.
      do kn = 1, network%nds%count
         if (network%nds%node(kn)%numberofconnections < 3) then
            cycle
         end if
         idx_fm = network%nds%node(kn)%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
         !we search for the SRE gridpoint associated to the FM ghost flownode.
         !the FM ghostflownode is found in <ln_mor> which is found in <nd>
         do kl = 1, nd(idx_fm)%lnx
            L = abs(nd(idx_fm)%ln(kl))
            n1 = ln_mor(1, L)
            n2 = ln_mor(2, L)
            idx_n = max(n1, n2) !maximum is ghost flownode
            idx_sre = grd_fm_sre(idx_n)
            do kd = 1, 3
               hpack(idx_sre, kd) = s1(idx_fm)
            end do !kd
         end do !kl
      end do !kn

   end subroutine inifm1dimp_fic

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fbrp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to fill branch properties for the 1D flow model.
   !! This subroutine initializes the branch properties for the 1D flow model by
   !! assigning friction parameters to each branch and grid point. It ensures that
   !! the friction values are properly set for both positive and negative flow
   !! directions in the main channel and floodplains.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_fbrp(iresult)

      use m_f1dimp
      use unstruc_channel_flow, only: network
      use messagehandling, only: msgbuf, err_flush

      implicit none

!
!pointer
!

      integer, pointer :: nbran
      integer, pointer :: ngrid

      integer, dimension(:), pointer :: grd_sre_cs
      integer, dimension(:), pointer :: grd_sre_fm

      integer, dimension(:, :), pointer :: bfrict

      real, dimension(:, :), pointer :: bfricp
      real, dimension(:, :), pointer :: sectc
      real, dimension(:, :), pointer :: sectv

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: ksre, k2, idx_fm, idx_crs, idx_h
      integer :: last_friction_type !< to check that friction type is the same in all sections

      !Defined in <sobcon.i>, but not accessible here.
      !
      !cfrchc (1) : Chezy constant
      !cfrchq (2) : Chezy function of discharge
      !cfrchh (3) : Chezy function of water level
      !cfrman (4) : Manning constant
      !cfrskn (5) : Strickler 1 constant ( k n )
      !cfrsks (6) : Strickler 2 constant ( k s )
      !cfrnik (7) : Nikuradze constant
      !cfreng (8) : Engelund predictor

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      nbran => f1dimppar%nbran
      ngrid => f1dimppar%ngrid
      grd_sre_cs => f1dimppar%grd_sre_cs
      grd_sre_fm => f1dimppar%grd_sre_fm
      bfrict => f1dimppar%bfrict
      bfricp => f1dimppar%bfricp
      sectc => f1dimppar%sectc
      sectv => f1dimppar%sectv

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

      last_friction_type = network%rgs%rough(1)%frictiontype !initially the overall value. It has already been checked it is the same for all section.

      do ksre = 1, ngrid

         idx_fm = grd_sre_fm(ksre) !index of the global grid point in fm for the global gridpoint <k> in SRE

         !bfrictp
         idx_crs = grd_sre_cs(ksre)

         !In FM it is possible that the friction type is different in the main channel than in the
         !floodplains. This is not possible in SOBEK-RE. Here we check that all are equal.
         !
         !Is this really true? Also when calling `getFrictionValue` rather than `FLCHZT`?
         do k2 = 1, network%crs%cross(idx_crs)%frictionsectionscount
            if (last_friction_type /= network%crs%cross(idx_crs)%frictiontypepos(k2)) then
               write (msgbuf, '(a)') 'The same type of friction must be applied to all sections (i.e., main channel, floodplain 1, floodplain 2, ...) at all cross-sections.'
               call err_flush()
               iresult = 1
            end if
            last_friction_type = network%crs%cross(idx_crs)%frictiontypepos(k2)
         end do

         !`sectc(:,1)` = `subsec`
         ! subsec(ngrid)     I  Defines the number of sub sections for every
         !                      cross section:
         !                      c1sec (0) : Main section only (0 sub sections)
         !                      c2sec (1) : 1 sub section
         !                      c3sec (2) : 2 sub sections
         !                      (For a circle cross section   : 0 ;
         !                       For a sedredge cross section : 1 )
         sectc(ksre, 1) = network%crs%cross(idx_crs)%frictionsectionscount - 1

         if (network%crs%cross(idx_crs)%frictionsectionscount > 1) then
         !!!
         !!!SUBSECTION 1
         !!!
            idx_h = network%crs%cross(idx_crs)%tabdef%plainslocation(1)

            !`sectv(1,2)` = `secth0`
            ! secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
            !                         grid point.
            sectv(ksre, 2) = network%crs%cross(idx_crs)%tabdef%height(idx_h)
            !`sectv(1,4)` = `afh0`
            ! afh0(ngrid)       I  Flow area Af at water level h=h0 for every
            !                      grid point.
            sectv(ksre, 4) = network%crs%cross(idx_crs)%tabdef%flowarea(idx_h)
            ! `sectv(1,6)` = `oh0`
            ! oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
            !                      every grid point.
            sectv(ksre, 6) = network%crs%cross(idx_crs)%tabdef%wetperimeter(idx_h)

            !
            !`sectc(:,2)` = `wfh0`
            ! wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
            !                      grid point.
            sectc(ksre, 2) = network%crs%cross(idx_crs)%tabdef%flowwidth(idx_h)

         end if

         if (network%crs%cross(idx_crs)%frictionsectionscount > 2) then
         !!!
         !!!SUBSECTION 2
         !!!
            idx_h = network%crs%cross(idx_crs)%tabdef%plainslocation(2)

            !`sectv(1,3)` = `secth1`
            ! secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
            !                         point.
            sectv(ksre, 3) = network%crs%cross(idx_crs)%tabdef%height(idx_h)
            !`sectv(1,5)` = `afh1`
            ! afh1(ngrid)       I  Flow area Af at water level h=h1 for every
            !                      grid point.
            sectv(ksre, 5) = network%crs%cross(idx_crs)%tabdef%flowarea(idx_h)
            ! `sectv(1,7)` = `oh1`
            ! oh1(ngrid)        I  Wetted perimeter Ot at water level h=h1 for
            !                      every grid point.
            sectv(ksre, 7) = network%crs%cross(idx_crs)%tabdef%wetperimeter(idx_h)

            !
            !`sectc(:,3)` = `wfh1`
            ! wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
            !                      grid point.
            sectc(ksre, 3) = network%crs%cross(idx_crs)%tabdef%flowwidth(idx_h)

         end if

         !`bfricp` is not used anymore. We directly use `network` inside Sobek-RE.
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

      end do !ksre

   end subroutine inifm1dimp_fbrp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_fbc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to initialize boundary conditions for the 1D flow model.
   !! This subroutine sets up the boundary conditions for the 1D flow model by
   !! mapping boundary nodes to their respective grid points and assigning
   !! boundary condition parameters. It handles both water level (H) and
   !! discharge (Q) boundaries, ensuring that the boundary condition tables
   !! are properly initialized and linked to the corresponding grid points.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_fbc(iresult)

      use m_f1dimp
      use fm_external_forcings_data, only: kbndz, kbndu

      implicit none

!
!pointer
!

      integer, pointer :: nhstat
      integer, pointer :: nqstat
      integer, pointer :: table_length
      integer, pointer :: number_bc_tables

      integer, dimension(:), pointer :: grd_fm_sre

      integer, dimension(:, :), pointer :: hbdpar
      integer, dimension(:, :), pointer :: qbdpar
      integer, dimension(:, :), pointer :: ntab

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: k1
      integer :: table_number !< counter position in which the BC is saved in the table

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      table_length => f1dimppar%table_length
      nhstat => f1dimppar%nhstat
      grd_fm_sre => f1dimppar%grd_fm_sre
      nqstat => f1dimppar%nqstat
      ntab => f1dimppar%ntab
      qbdpar => f1dimppar%qbdpar
      hbdpar => f1dimppar%hbdpar
      number_bc_tables => f1dimppar%number_bc_tables

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

!<table> will contain 4 elements per BC:
!   -X1 (time)
!   -X2 (time)
!   -Y1 (variable, e.g., water level)
!   -Y2 (variable, e.g., water level)
!the order in <table> is:
!   -H-boundaries
!   -Q-boundaires

      table_number = 0

!h
      do k1 = 1, nhstat
         table_number = table_number + 1

         hbdpar(1, k1) = grd_fm_sre(kbndz(2, k1)) !< first s1 point on the inside of the domain
         hbdpar(2, k1) = 1
         hbdpar(3, k1) = table_number

         !FM1DIMP2DO: make this a subroutine? called in every loop for every BC
         ntab(1, table_number) = table_length !length of table
         ntab(2, table_number) = table_number * table_length - 1 !start address X
         ntab(3, table_number) = number_bc_tables * table_length + table_number * table_length - 1 !start address Y
         ntab(4, table_number) = 0 !access method (0=continuous interpolation)
      end do

!q
      do k1 = 1, nqstat
         table_number = table_number + 1

         qbdpar(1, k1) = grd_fm_sre(kbndu(2, k1)) !< first s1 point on the inside of the domain
         qbdpar(2, k1) = 1
         qbdpar(3, k1) = table_number !< table number after the ones of <hbdpar>

         ntab(1, table_number) = table_length !length of table
         ntab(2, table_number) = table_number * table_length - 1 !start address X
         ntab(3, table_number) = number_bc_tables * table_length + table_number * table_length - 1 !start address Y
         ntab(4, table_number) = 0 !access method (0=continuous interpolation)
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

   !> Subroutine to initialize nodes for the 1D flow model.
   !! This subroutine sets up the node properties for the 1D flow model by
   !! determining the type of each node (internal, boundary, or connection),
   !! mapping nodes to their respective grid points, and establishing
   !! connectivity between nodes. It ensures that boundary nodes are properly
   !! linked to their respective boundary conditions and that internal nodes
   !! are correctly connected to other nodes via branches.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_fnod(iresult)

      use m_f1dimp
      use unstruc_channel_flow, only: network
      use messagehandling, only: msgbuf, err_flush

      implicit none

!
!pointer
!

      integer, pointer :: nbran
      integer, pointer :: nnode
      integer, pointer :: nhstat
      integer, pointer :: nqstat

      integer, dimension(:), pointer :: numnod
      integer, dimension(:), pointer :: grd_fm_sre
      integer, dimension(:, :), pointer :: hbdpar
      integer, dimension(:, :), pointer :: qbdpar
      integer, dimension(:, :), pointer :: node
      integer, dimension(:, :), pointer :: nodnod

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: kbr, knod, k2
      integer :: idx_fr, idx_to

      integer, allocatable, dimension(:) :: kcol !saves the index to write in the row of <nonnod>

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      nnode => f1dimppar%nnode
      nbran => f1dimppar%nbran
      nhstat => f1dimppar%nhstat
      nqstat => f1dimppar%nqstat
      grd_fm_sre => f1dimppar%grd_fm_sre
      qbdpar => f1dimppar%qbdpar
      hbdpar => f1dimppar%hbdpar
      node => f1dimppar%node
      numnod => f1dimppar%numnod
      nodnod => f1dimppar%nodnod

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

!nodes
      do knod = 1, nnode
         !f1dimppar%node(1,k)=
         !for some reason at this stage <network%nds%node%nodetype> is only either 0 or 1
         !hence, if type 0 it means BC and I have to search which one is it
         !network%nds%node%nodetype
    !! - -2    boundary node
    !! - -1    not set
    !! -  0    node with one reach connected
    !! -  1    connection node with more than one reach connected
    !! -  2    water level boundary
    !! -  3    Discharge boundary
    !! -  4    Discharge boundary as tabulated function of water level
    !! -  5    Embedded node
         if (network%nds%node(knod)%nodetype == 0) then !BC noce

            do k2 = 1, nhstat !search in hbdpar
               if (hbdpar(1, k2) == grd_fm_sre(network%nds%node(knod)%gridnumber)) then
                  node(1, knod) = 2 !H boundary
                  node(2, knod) = hbdpar(1, k2) !gridpoint
                  node(3, knod) = k2
                  node(4, knod) = 1 !not sure what should it be
               end if
            end do !nhstat

            if (node(1, knod) == -999) then !it is not hbdpar, we search in qbdpar

               do k2 = 1, nqstat !search in hbdpar
                  if (qbdpar(1, k2) == grd_fm_sre(network%nds%node(knod)%gridnumber)) then
                     node(1, knod) = 3 !Q boundary
                     node(2, knod) = qbdpar(1, k2) !gridpoint
                     node(3, knod) = k2
                     node(4, knod) = 1 !not sure what should it be
                  end if
               end do

            end if

            if (node(1, knod) == 0) then !it is not hbdpar nor qbdpar => error
               write (msgbuf, '(a)') 'There is a node which is neither internal nor H nor Q boundary.'
               call err_flush()
               iresult = 1
            end if

         elseif (network%nds%node(knod)%nodetype == 1) then !internal node
            node(1, knod) = 1
            !node(2:end) is undefined if internal node
         else
            write (msgbuf, '(a)') 'The type of node is not what I expected.'
            call err_flush()
            iresult = 1
         end if

         numnod(knod) = network%nds%node(knod)%numberofconnections + 1

      end do

!nodes

!<kcol> saves the index to write in the row of <nonnod>
      if (allocated(kcol)) then
         deallocate (kcol)
      end if
      allocate (kcol(nnode))
      kcol = 2 !first index is filled with its own node

!filling first index with its own node
      do knod = 1, nnode
         nodnod(knod, 1) = knod
      end do

      do kbr = 1, nbran

         idx_fr = network%brs%branch(kbr)%nodeindex(1)
         idx_to = network%brs%branch(kbr)%nodeindex(2)

         nodnod(idx_fr, kcol(idx_fr)) = idx_to
         kcol(idx_fr) = kcol(idx_fr) + 1

         nodnod(idx_to, kcol(idx_to)) = idx_fr
         kcol(idx_to) = kcol(idx_to) + 1

      end do

      if (allocated(kcol)) then
         deallocate (kcol)
      end if

   end subroutine inifm1dimp_fnod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BEGIN inifm1dimp_chk
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !> Subroutine to perform checks on the initialization of the 1D flow model.
   !! This subroutine validates the initialization of the 1D flow model by ensuring that all
   !! cross-sections are mapped to SRE grid points and that sediment fractions are within
   !! acceptable limits. It also handles error reporting for any inconsistencies found during
   !! the validation process.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_chk(iresult)

      use m_f1dimp
      use m_fm_erosed, only: lsedtot, ndx_mor, frac
      use m_sediment, only: jased, stm_included
      use messagehandling, only: msgbuf, err_flush

      implicit none

!
!pointer
!

      integer, pointer :: ngrid

      integer, dimension(:), pointer :: grd_sre_cs

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!

      integer :: kd, ksed

      integer, dimension(1) :: idx_findloc

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      ngrid => f1dimppar%ngrid
      grd_sre_cs => f1dimppar%grd_sre_cs

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      iresult = 0 !no error

!all cross-sections must be mapped to an SRE gridpoint
      do kd = 1, ngrid
         idx_findloc = findloc(grd_sre_cs, kd)
         if (idx_findloc(1) == 0) then
            write (msgbuf, '(a)') 'Not all SRE nodes are related to a cross-section.'
            call err_flush()
            iresult = 1
         end if
      end do

      if (jased > 0 .and. stm_included) then !passing if no morphpdynamics

!FM1DIMP2DO: for some strange reason <frac> seems to not be always fine. I do not know why.
!if repeating the run in debug mode, the problem is not there.

         do kd = 1, ndx_mor
            do ksed = 1, lsedtot
               if (frac(kd, ksed) > 1.00001) then
                  write (msgbuf, '(a)') 'Something is wrong with <frac>.'
                  call err_flush()
                  iresult = 1
               end if
            end do !ksed
         end do !kd

      end if !jased

   end subroutine inifm1dimp_chk

   !> Subroutine to initialize structures in the 1D flow model.
   !! This subroutine sets up the necessary parameters and configurations for hydraulic structures
   !! in the 1D flow model. It assigns grid points, initializes tables for structure properties,
   !! and validates the structure types. Only structures converted to General Structure in Delft3D FM
   !! are supported.
   !!
   !! @param iresult [out] Error status, DFM_NOERR==0 if successful.
   subroutine inifm1dimp_fstruc(iresult)

      use m_f1dimp
      use unstruc_channel_flow, only: network
      use messagehandling, only: msgbuf, err_flush
!
!pointer
!

      integer, dimension(:), pointer :: grid
      integer, dimension(:, :), pointer :: ntab
      real, dimension(:), pointer :: table
      integer, pointer :: table_length
      integer, pointer :: number_bc_tables
      integer, pointer :: table_length_stru

!
!output
!

      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

!
!local
!
      integer :: table_number, idx_start_x, idx_start_y
      integer :: k
      integer :: flg_structure

!----------------------------------------
!BEGIN POINT
!----------------------------------------

      number_bc_tables => f1dimppar%number_bc_tables
      table_length => f1dimppar%table_length
      ntab => f1dimppar%ntab
      table => f1dimppar%table
      table_length_stru => f1dimppar%table_length_stru
      grid => f1dimppar%grid

!----------------------------------------
!BEGIN CALC
!----------------------------------------

      !All gridpoints as normal ones
      grid = 1

      !TABLES

      flg_structure = 2 !1=Simple weir, 2=General structure. In FM, all structures are treated as general structures.
      iresult = 0 !no error

      table_number = number_bc_tables + 1 !structure after the BC

      idx_start_x = 2 * (number_bc_tables * table_length) + 1
      idx_start_y = idx_start_x + table_length_stru

      ntab(1, table_number) = table_length_stru !length of table
      ntab(2, table_number) = idx_start_x !start address X
      ntab(3, table_number) = ntab(2, table_number) + table_length_stru !start address Y
      ntab(4, table_number) = 0 !access method (0=continuous interpolation)

      !Table for simple weir losses.
      !<SOBEK_flow_2012.pdf>, page 73, Figure 2 "drowned flow reduction curves".
      table(idx_start_x) = 1.00
      table(idx_start_y) = 0.00

      table(idx_start_x + 1) = 0.99_dp
      table(idx_start_y + 1) = 0.20_dp

      table(idx_start_x + 2) = 0.97_dp
      table(idx_start_y + 2) = 0.40_dp

      table(idx_start_x + 3) = 0.96_dp
      table(idx_start_y + 3) = 0.60_dp

      table(idx_start_x + 4) = 0.95_dp
      table(idx_start_y + 4) = 0.80_dp

      table(idx_start_x + 5) = 0.90_dp
      table(idx_start_y + 5) = 0.90_dp

      table(idx_start_x + 6) = 0.85_dp
      table(idx_start_y + 6) = 0.95_dp

      table(idx_start_x + 7) = 0.82_dp
      table(idx_start_y + 7) = 1.00_dp
      !FLABC
      !FLSTRU
      !FLSW
      !FLQHSW -> use of the table

      !STRUCTURE
      associate ( &
         nstru => f1dimppar%nstru, &
         strtyp => f1dimppar%strtyp, &
         grd_fmL_sre => f1dimppar%grd_fmL_sre, &
         strpar => f1dimppar%strpar &
         )
         do k = 1, nstru
            associate (struct_fm => network%sts%struct(k))

               if (struct_fm%type == 6) then

               else
                  write (msgbuf, '(a)') 'Only structures that are converted to General Structure in Delft3D FM are accepted.'
                  call err_flush()
                  iresult = 1
               end if

               strtyp(2, k) = 1

               strtyp(3, k) = grd_fmL_sre(struct_fm%linknumbers(1), 1)
               strtyp(4, k) = grd_fmL_sre(struct_fm%linknumbers(1), 2)

               !<SOBEK_flow_2012.pdf>, page 37.
               !Hydraulic structures are located al grid points. So when the user specifies the position of the
               !grid points, he should specify only one grid point per hydraulic structure. Internally, SOBEK
               !makes a copy of this grid point, since two equal grid points (one at each side of a hydrautic
               !structure) are used for the application of structure formulas.
               !
               !When I create a model using the SRE GUI, I only see one cross-section added 1 m after the structure.
               grid(strtyp(3, k)) = 2

               if (flg_structure == 1) then !Treat structure as simple weir
                  strtyp(1, k) = 1 !simple weir

                  !<SOBEK_flow_2012.pdf>, page 73, Table 1 "Crest shape and coefficients for simple weir structure (default values)".
                  strpar(1, k) = struct_fm%generalst%zs_actual
                  strpar(2, k) = struct_fm%generalst%ws_actual
                  strpar(3, k) = 1.0_dp
                  strpar(4, k) = 0.82_dp
                  strpar(5, k) = number_bc_tables + 1
                  strpar(6, k) = 1.0_dp
                  strpar(7, k) = 0.82_dp
                  strpar(8, k) = number_bc_tables + 1
               else !Treat structure as general structure
                  strtyp(1, k) = 5 !general structure type

                  strpar(1, k) = struct_fm%generalst%wu1 !(1,j) = Width left side of structure W1.
                  strpar(2, k) = struct_fm%generalst%zu1 !(2,j) = Bed level left side of structure Zb1.
                  strpar(3, k) = struct_fm%generalst%wu2 !(3,j) = Width structure left side Wsdl.
                  strpar(4, k) = struct_fm%generalst%zu2 !(4,j) = Bed left side of structure Zbsl.
                  strpar(5, k) = struct_fm%generalst%ws_actual !(5,j) = Width structure centre Ws.
                  strpar(6, k) = struct_fm%generalst%zs_actual !(6,j) = Bed level centre Zs.
                  strpar(7, k) = struct_fm%generalst%wd2 !(7,j) = Width structure right side Wsdr.
                  strpar(8, k) = struct_fm%generalst%zd2 !(8,j) = Bed right side of structure Zbsr.
                  strpar(9, k) = struct_fm%generalst%wd1 !(9,j) = Width right side of structure W2.
                  strpar(10, k) = struct_fm%generalst%zd1 !(10,j)= Bed level right side of structure Zb2.
                  strpar(11, k) = struct_fm%generalst%gateloweredgelevel_actual - struct_fm%generalst%zs_actual !(11,j)= Gate opening heigth dg.
!                              Positive flow:
                  strpar(12, k) = struct_fm%generalst%cgf_pos !(12,j)= Correction coefficient for free gate flow cgf.
                  strpar(13, k) = struct_fm%generalst%cgd_pos !(13,j)= Correction coefficient for drowned gate flow cgd.
                  strpar(14, k) = struct_fm%generalst%cwf_pos !(14,j)= Correction coefficient for free weir flow cwf.
                  strpar(15, k) = struct_fm%generalst%cwd_pos !(15,j)= Correction coefficient for drowned weir flow cwd.
                  strpar(16, k) = struct_fm%generalst%mugf_pos !(16,j)= Contraction coefficient for free gate flow MU-gf.
!                              Negative flow:
                  strpar(17, k) = struct_fm%generalst%cgf_pos !(17,j)= Correction coefficient for free gate flow cgf.
                  strpar(18, k) = struct_fm%generalst%cgd_pos !(18,j)= Correction coefficient for drowned gate flow cgd.
                  strpar(19, k) = struct_fm%generalst%cwf_pos !(19,j)= Correction coefficient for free weir flow cwf.
                  strpar(20, k) = struct_fm%generalst%cwd_pos !(20,j)= Correction coefficient for drowned weir flow cwd.
                  strpar(21, k) = struct_fm%generalst%mugf_pos !(21,j)= Contraction coefficient for free gate flow MU-gf.
               end if

            end associate
         end do
      end associate

   end subroutine inifm1dimp_fstruc

end module m_initialize_flow1d_implicit

