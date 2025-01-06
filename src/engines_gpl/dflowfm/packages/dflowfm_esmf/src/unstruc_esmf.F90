!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2015-2024.
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

!
!
#define ESMF_CHECK if (ESMF_LogFoundError(rcToCheck=rc, line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
module unstruc_esmf
   use m_inidat, only: inidat
   use esmf
   ! avoid namespace collision with network
   use unstruc_api, only: flowinit, flowstep, time_user, tstop_user, api_loadmodel
   use network_data
   use m_flow
   use m_flowgeom
   implicit none

   integer, parameter :: MAXSTRLEN = 1024
   public unstruc_register

contains
   subroutine unstruc_register(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, userRoutine=unstruc_init, rc=rc)
      ESMF_CHECK
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, userRoutine=unstruc_run, rc=rc)
      ESMF_CHECK
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, userRoutine=unstruc_final, rc=rc)
      ESMF_CHECK
   end subroutine unstruc_register

   subroutine unstruc_init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp ! TODO bug in esmf? Can't sepcify intent here....
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
      integer :: narg

      type(ESMF_Mesh) :: grid
      type(ESMF_DistGrid) :: distgrid ! For mpi
      type(ESMF_FieldBundle) :: fieldbundle

      character(len=MAXSTRLEN) :: filename

      if (command_argument_count() < 1) then
         ! we're not the main program, get the file from the attribute
         write (*, *) 'Getting mdu from gridcomp'
         call ESMF_AttributeGet(gridcomp, name='mdu', value=filename, rc=rc)
         ESMF_CHECK
      else
         call get_command_argument(1, filename)
      end if

      call start() ! required because of initprogram, which calls initsysenv
      call inidat()
      write (*, *) 'Initializing model', trim(filename)
      call api_loadmodel(filename)
      write (*, *) 'model initialized: ', numk, ' nodes'
      call flowinit()

      ! Initialize

      ! States
      call ESMF_StatePrint(importState, rc=rc)
      ESMF_CHECK
      call ESMF_StatePrint(exportState, rc=rc)
      ESMF_CHECK

      write (*, *) 'Making ESMF grid'
      ! Create the grid
      call make_unstruc_grid(grid, rc=rc)

      write (*, *) 'Making import Fields'

      call make_unstruc_fieldbundle(grid, stateintent=ESMF_STATEINTENT_IMPORT, fieldbundle=fieldbundle, rc=rc)
      call ESMF_StateAdd(importState, fieldbundleList=(/fieldbundle/), rc=rc)
      ESMF_CHECK

      write (*, *) 'Making export Fields'
      call make_unstruc_fieldbundle(grid, stateintent=ESMF_STATEINTENT_EXPORT, fieldbundle=fieldbundle, rc=rc)
      call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=rc)
      ESMF_CHECK

      call ESMF_StatePrint(importState, rc=rc)
      ESMF_CHECK
      call ESMF_StatePrint(exportState, rc=rc)
      ESMF_CHECK

      write (*, *) 'ESMF initialize complete'
   end subroutine unstruc_init

   subroutine unstruc_run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      type(ESMF_TimeInterval) :: timeinterval
      real(ESMF_KIND_R8) :: esmfstep, esmft, esmfnext

      integer :: jastop ! uggh

      call ESMF_StateWrite(importState, 'dflowfmimport.nc', rc)

      ! Synchronize the clocks

      ! ESMF clock
      !       ESMFT -----> ESMFNEXT
      !         .             .
      !          <-ESMFSTEP-->

      ! DFLOW clock
      !     time_user --> tstop_user

      ! Get the simulation time (since tref) from esmf
      call ESMF_ClockGet(clock, currSimTime=timeinterval, rc=rc)
      call ESMF_TimeIntervalGet(timeinterval, s_r8=esmft, rc=rc)

      ! Get the timestep from esmf, the coupled timestep
      call ESMF_ClockGet(clock, timestep=timeinterval, rc=rc)
      call ESMF_TimeIntervalGet(timeinterval, s_r8=esmfstep, rc=rc)

      ! Run until we reach esmfnext
      esmfnext = (esmft + esmfstep)
      ! set the DFLOWFM clock at the same stop time....
      ! TODO check, assuming seconds here...
      tstop_user = esmfnext
      do while (time_user < tstop_user .and. jastop == 0) ! time loop
         call flowstep(jastop)
      end do

      ! State export:
      call ESMF_StateWrite(exportState, 'dflowfmexport.nc', rc)
   end subroutine unstruc_run

   subroutine unstruc_final(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      call flowfinalize()
   end subroutine unstruc_final

   subroutine make_unstruc_grid(mesh, rc)
      type(ESMF_Mesh), intent(out) :: mesh
      integer, intent(out) :: rc

      ! Grid administration

      ! Use the same variable names as in the ESMF docs
      ! Dimensions and counting

      ! Dimension of the topology of the Mesh. (E.g. a mesh constructed
      ! of squares would have a parametric dimension of 2, whereas a
      ! Mesh constructed of cubes would have one of 3.)
      integer :: parametricDim = 2
      ! The number of coordinate dimensions needed to describe the
      ! locations of the nodes making up the Mesh. For a manifold, the
      ! spatial dimesion can be larger than the parametric dim (e.g. the
      ! 2D surface of a sphere in 3D space), but it can't be smaller.
      integer :: spatialDim = 2

      integer :: numNodes
      integer :: numQuadElems
      integer :: numTriElems
      integer :: numTotElems

      ! Variables An array containing the physical coordinates of the
      ! nodes to be created on this PET. This input consists of a 1D
      ! array the size of the number of nodes on this PET times the
      ! Mesh's spatial dimension (spatialDim). The coordinates in this
      ! array are ordered so that the coordinates for a node lie in
      ! sequence in memory. (e.g. for a Mesh with spatial dimension 2,
      ! the coordinates for node 1 are in nodeCoords(0) and
      ! nodeCoords(1), the coordinates for node 2 are in nodeCoords(2)
      ! and nodeCoords(3), etc.).
      real(ESMF_KIND_R8), allocatable :: nodeCoords(:)

      ! An array containing the global ids of the nodes to be created on
      ! this PET. This input consists of a 1D array the size of the
      ! number of nodes on this PET.
      integer, allocatable :: nodeIds(:)
      integer, allocatable :: nodeOwners(:)

      ! An array containing the global ids of the elements to be created
      ! on this PET. This input consists of a 1D array the size of the
      ! number of elements on this PET.
      integer, allocatable :: elementIds(:)

      ! An array containing the types of the elements to be created on
      ! this PET. The types used must be appropriate for the parametric
      ! dimension of the Mesh. Please see Section 29.2.1 for the list of
      ! options. This input consists of a 1D array the size of the
      ! number of elements on this PET.
      integer, allocatable :: elementTypes(:)

      ! An array containing the indexes of the sets of nodes to be
      ! connected together to form the elements to be created on this
      ! PET. The entries in this list are NOT node global ids, but
      ! rather each entry is a local index (1 based) into the list of
      ! nodes which were created on this PET by the previous
      ! ESMF_MeshAddNodes() call. In other words, an entry of 1
      ! indicates that this element contains the node described by
      ! nodeIds(1), nodeCoords(1), etc. passed into the
      ! ESMF_MeshAddNodes() call on this PET. It is also important to
      ! note that the order of the nodes in an element connectivity list
      ! matters. Please see Section 29.2.1 for diagrams illustrating the
      ! correct order of nodes in a element. This input consists of a 1D
      ! array with a total size equal to the sum of the number of nodes
      ! in each element on this PET. The number of nodes in each element
      ! is implied by its element type in elementTypes. The nodes for
      ! each element are in sequence in this array (e.g. the nodes for
      ! element 1 are elementConn(1), elementConn(2), etc.).
      integer, allocatable :: elementConn(:) ! 4*numQuadElems+3*numTriElems

      ! For coupling only these elements are supported.
      ! Cell types
      integer :: TRI = ESMF_MESHELEMTYPE_TRI
      integer :: QUAD = ESMF_MESHELEMTYPE_QUAD

      ! iters
      integer :: i, j, k

      write (*, *) 'Step 1'
      ! Create a Mesh as a 3 step process (dims, nodes, elements)
      mesh = ESMF_MeshCreate(parametricDim, spatialDim, rc)
      ESMF_CHECK

      ! Create the nodes...

      write (*, *) 'Making grid with ', numk, ' nodes.'
      numNodes = numk
      ! Fill the indices
      allocate (nodeIds(numNodes))
      forall (i=1:numNodes:1) nodeIds(i) = i

      ! nodeCoords=(/ xk(1), yk(1), xk(2), yk(2), ....
      allocate (nodeCoords(2 * numNodes))
      nodeCoords(1:(2 * numNodes):2) = xk(1:numNodes)
      nodeCoords(2:(2 * numNodes):2) = yk(1:numNodes)

      ! Set all nodes owned to pet0
      allocate (nodeOwners(numNodes))
      nodeOwners = 0

      ! write(*,*) 'Sizes: netcell', shape(netcell)
      ! write(*,*) 'Sizes: numnodes', numnodes
      ! write(*,*) 'Sizes: nump', nump, '*'
      ! write(*,*) 'Sizes: numk', numk, '*'
      ! write(*,*) 'Sizes: ndx', ndx
      ! write(*,*) 'Sizes: s1', size(s1), '*'
      ! write(*,*) 'Sizes: tnod', size(nd), '*'
      write (*, *) 'Step 2'
      call ESMF_MeshAddNodes(mesh, nodeIds, nodeCoords, nodeOwners, rc=rc)
      ESMF_CHECK

      ! Let's define the elements

      ! Following example of the reference manual
      numTotElems = nump
      numQuadElems = 0
      numTriElems = 0
      ! This is almost similar to the VTK data structure
      allocate (elementTypes(numTotElems))
      allocate (elementIds(numTotElems))

      do k = 1, numTotElems
         ! Use the netcells.

         select case (size(nd(k)%nod))
         case (3)
            elementTypes(k) = TRI
            numTriElems = numTriElems + 1
         case (4)
            elementTypes(k) = QUAD
            numQuadElems = numQuadElems + 1
         case default
            write (*, *) 'Assertion failed expecting elements of 3,4 nodes, got', nd(k)%nod, ' for ', k
         end select
      end do
      ! check...
      if (.not. (numTriElems + numQuadElems) == numTotElems) rc = 10
      ! A list of all nodes (without the count that vtk uses)
      allocate (elementConn(4 * numQuadElems + 3 * numTriElems))

      ! Just the counters. (1 based)
      forall (i=1:numTotElems:1) elementIds(i) = i

      ! Setup th econnections
      j = 1
      do k = 1, numTotElems
         ! Use the netcells. (TODO check)
         select case (size(nd(k)%nod))
         case (3)
            elementConn(j:(j + 3)) = nd(k)%nod(1:3)
            j = j + 3
         case (4)
            elementConn(j:(j + 4)) = nd(k)%nod(1:4)
            j = j + 4
         case default
            write (*, *) 'Assertion failed expecting elements of 3,4 nodes, got', nd(k)%nod, ' for ', k
         end select
      end do
      write (*, *) 'Step 3'
      ! Now we have everything, let's add them
      call ESMF_MeshAddElements(mesh, elementIds, elementTypes, elementConn, rc=rc)
      ESMF_CHECK

      ! Cleanup...
      deallocate (nodeIds)
      deallocate (nodeCoords)
      deallocate (nodeOwners)
      deallocate (elementIds)
      deallocate (elementTypes) !
      deallocate (elementConn)

      ! And we're done.
   end subroutine make_unstruc_grid

   ! ! Also make a helper function for fields...
   subroutine make_unstruc_fieldbundle(mesh, stateintent, fieldbundle, rc)

      ! Then we can define the fields
      type(ESMF_Mesh), intent(inout) :: mesh
      type(ESMF_StateIntent_Flag), intent(in) :: stateintent
      type(ESMF_FieldBundle), intent(out) :: fieldbundle
      integer, intent(out) :: rc

      type(ESMF_Field) :: field
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_MeshLoc) :: meshloc
      integer :: nelements

      real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr
      character(len=MAXSTRLEN) :: name

      if (stateintent == ESMF_STATEINTENT_EXPORT) then

         name = "fm export"
      elseif (stateintent == ESMF_STATEINTENT_IMPORT) then

         name = "fm import"
      end if
      fieldbundle = ESMF_FieldBundleCreate(name=name, rc=rc)
      ESMF_CHECK

      if (stateintent == ESMF_STATEINTENT_EXPORT) then

         ! Add all the fields here... Pointers should update automaticly.... (don't copy data)
         name = "s1"
         if (allocated(s1)) then
            farrayPtr => s1(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         else
            write (*, *) 's1 not allocated skipping'
         end if

         name = "ucx"
         if (allocated(ucx)) then
            farrayPtr => ucx(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         else
            write (*, *) 'ucx not allocated skipping'
         end if

         name = "ucy"
         if (allocated(ucy)) then
            farrayPtr => ucy(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         else
            write (*, *) 'ucy not allocated skipping'
         end if
         ! TODO extend with edge location

         name = "hs"
         if (allocated(hs)) then
            farrayPtr => hs(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         else
            write (*, *) 'hs not allocated skipping'
         end if
         ! TODO Wait for ESMF for best approach....
         ! ! test with edges...
         ! name="u1"
         ! farrayPtr => u1
         ! field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr,meshloc=ESMF_MESHLOC_ELEMENT, name=name, rc=rc)
         ! ESMF_CHECK
         ! call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/),rc=rc)
         ! ESMF_CHECK

      elseif (stateintent == ESMF_STATEINTENT_IMPORT) then

         ! Create import state with pointers to all variables

         if (allocated(twav)) then
            name = "twav"
            farrayPtr => twav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(Uorb)) then
            name = "Uorb"
            farrayPtr => Uorb(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(hwav)) then
            name = "hwav"
            farrayPtr => hwav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(tpswav)) then
            name = "tpswav"
            farrayPtr => tpswav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(rtpwav)) then
            name = "rtpwav"
            farrayPtr => rtpwav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(phiwav)) then
            name = "phiwav"
            farrayPtr => phiwav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(fxwav)) then
            name = "fxwav"
            farrayPtr => fxwav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(fywav)) then
            name = "fywav"
            farrayPtr => fywav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(uorbwav)) then
            name = "uorbwav"
            farrayPtr => uorbwav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if
         if (allocated(wlenwav)) then
            name = "wlenwav"
            farrayPtr => wlenwav(1:numk)
            field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
            ESMF_CHECK
            call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
            ESMF_CHECK
         end if

      end if

   end subroutine make_unstruc_fieldbundle
end module unstruc_esmf
