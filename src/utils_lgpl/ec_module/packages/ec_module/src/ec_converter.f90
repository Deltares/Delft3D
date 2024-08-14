!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.

!
!

!> This module contains all the methods for the datatype tEcConverter.
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
!! @author robert.leander@deltares.nl
module m_ec_converter
   use m_ec_typedefs
   use m_ec_message
   use mathconsts
   use m_ec_support
   use m_ec_alloc
   use m_ec_parameters
   use m_ec_spatial_extrapolation
   use time_class

   implicit none

   private

   public :: ecConverterCreate
   public :: ecConverterInitialize
   public :: ecConverterFree1dArray
   public :: ecConverterSetType
   public :: ecConverterSetOperand
   public :: ecConverterSetInputPointer
   public :: ecConverterUpdateWeightFactors
   public :: ecConverterPerformConversions
   public :: ecConverterSetElement
   public :: ecConverterSetInterpolation
   public :: ecConverterSetMask
   public :: ecConverterGetBbox
   contains

      ! =======================================================================

      !> Construct a new Converter with the specified id.
      !! Failure is indicated by returning a null pointer.
      function ecConverterCreate(converterId) result(converterPtr)
         type(tEcConverter), pointer            :: converterPtr !< the new Converter, intent(out)
         integer,                    intent(in) :: converterId  !< unique Converter id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(converterPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_converter::ecConverterCreate: Unable to allocate additional memory.")
            converterPtr => null()
            return
         end if
         ! initialization
         converterPtr%id = converterId
         converterPtr%ofType = convType_undefined
         converterPtr%operandType = operand_undefined
         converterPtr%interpolationType = interpolate_unknown
         converterPtr%targetIndex = ec_undef_int
      end function ecConverterCreate

      ! =======================================================================

      !> Free a tEcConverter, after which it can be deallocated.
      function ecConverterFree(converter) result (success)
         logical                           :: success   !< function status
         type(tEcConverter), intent(inout) :: converter !< tEcConverter to free
         !
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         ! The Converter owns the dynamically allocated memory, so deallocate.
         if (associated(converter%indexWeight)) then
            if (associated(converter%indexWeight%indices)) then
               deallocate(converter%indexWeight%indices, stat = istat)
               if (istat /= 0) success = .false.
            end if
            if (associated(converter%indexWeight%weightFactors)) then
               deallocate(converter%indexWeight%weightFactors, stat = istat)
               if (istat /= 0) success = .false.
            end if
            deallocate(converter%indexWeight, stat = istat)
            if (istat /= 0) success = .false.
         end if
      end function ecConverterFree

      ! =======================================================================

      !> Frees a 1D array of tEcConverterPtrs, after which the converterPtr is deallocated.
      function ecConverterFree1dArray(converterPtr, nConverters) result (success)
         logical                                      :: success      !< function status
         type(tEcConverterPtr), dimension(:), pointer :: converterPtr !< intent(inout)
         integer, intent(inout)                       :: nConverters  !< number of Converters
         !
         integer :: i      !< loop counter
         integer :: istat  !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(converterPtr)) then
            call setECMessage("WARNING: ec_converter::ecConverterFree1dArray: Dummy argument converterPtr is already disassociated.")
         else
            ! Free and deallocate all tEcConverterPtrs in the 1d array.
            do i=1, nConverters
               if (ecConverterFree(converterPtr(i)%ptr)) then
                  deallocate(converterPtr(i)%ptr, stat = istat)
                  if (istat /= 0) success = .false.
               else
                  success = .false.
               end if
            end do
            ! Finally deallocate the tEcConverterPtr(:) pointer.
            if (success) then
               deallocate(converterPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
         nConverters = 0
      end function ecConverterFree1dArray

      ! =======================================================================
      ! Set methods
      ! =======================================================================

      !> Helper function for initializing a Converter.
      function ecConverterInitialize(instancePtr, converterId, convtype, operand, method, srcmask) result(success)
         logical                    :: success      !< function status
         type(tEcInstance), pointer :: instancePtr  !<
         integer                    :: converterId  !<
         integer                    :: convtype     !<
         integer                    :: operand      !<
         integer                    :: method       !<
         type (tEcMask), optional   :: srcmask      !<
         !
         success              = ecConverterSetType(instancePtr, converterId, convtype)
         if (success) success = ecConverterSetOperand(instancePtr, converterId, operand)
         if (success) success = ecConverterSetInterpolation(instancePtr, converterId, method)
         if (present(srcmask)) then
            if (success) success = ecConverterSetMask(instancePtr, converterId, srcmask)
         end if
      end function ecConverterInitialize

      !> Attach a mask instance to the converter masking source points (used in case of arcinfo data)
      function ecConverterSetMask(instancePtr, converterId, srcmask) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         type(tEcMask),             intent(in) :: srcmask     !< new type of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         integer                     :: istat
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%srcmask = srcmask
            if (allocated(converterPtr%srcmask%msk)) deallocate (converterPtr%srcmask%msk, stat=istat)
            allocate(converterPtr%srcmask%msk(size(srcmask%msk)), stat=istat)
            converterPtr%srcmask%msk = srcmask%msk
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetMask: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetMask

      ! =======================================================================

      !> Change the type of the Converter corresponding to converterId.
      function ecConverterSetType(instancePtr, converterId, ofType) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         integer,                   intent(in) :: ofType      !< new type of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%ofType = ofType
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetType: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetType

      ! =======================================================================

      !> Change the operand of the Converter corresponding to converterId.
      function ecConverterSetOperand(instancePtr, converterId, operand) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         integer,                   intent(in) :: operand     !< new operand of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%operandType = operand
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetOperand: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetOperand

      ! =======================================================================

      !> Change the pointer to an input argument for the converter
      function ecConverterSetInputPointer(instancePtr, converterId, inputptr) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         real(hp), pointer                     :: inputptr    !< pointer to an input arg for the converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%inputptr => inputptr
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetInputPointer: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetInputPointer

      ! =======================================================================

      !> Change the interpolation type of the Converter corresponding to converterId.
      function ecConverterSetInterpolation(instancePtr, converterId, interpolationType) result(success)
         logical                               :: success           !< function status
         type(tEcInstance), pointer            :: instancePtr       !< intent(in)
         integer,                   intent(in) :: converterId       !< unique Converter id
         integer,                   intent(in) :: interpolationType !< new interpolation type of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%interpolationType = interpolationType
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetInterpolation: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetInterpolation

      ! =======================================================================

      !> Set the array index of the target Item's field to write to of the Converter corresponding to converterId.
      function ecConverterSetElement(instancePtr, converterId, indx) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         integer,                   intent(in) :: indx        !< array index in target item array where to put the converted data. NOTE: call site should already account for the vectormax/n_data in this index.
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%targetIndex = indx
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetElement: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetElement

      ! =======================================================================

      !> Update the weight factors of a Converter.
      function ecConverterUpdateWeightFactors(instancePtr, connection) result (success)
      use kdtree2Factory
      use m_ec_basic_interpolation
      use m_alloc
      implicit none
         logical                            :: success     !< function status
         type(tEcInstance), pointer         :: instancePtr !< intent(inout)
         type(tEcConnection), intent(inout) :: connection  !< access to Converter and Items
         !
         type(tEcIndexWeight), pointer :: weight           !< the new IndexWeight
         type(tEcElementSet),  pointer :: sourceElementSet !< source ElementSet
         type(tEcElementSet),  pointer :: targetElementSet !< target ElementSet
         real(hp) :: wx, wy
         integer  :: i,j !< loop counter
         integer  :: ii, jj, ierr
         integer  :: n_cols, n_rows, n_points
         integer  :: inside, mp, np, in, jn !< return values of findnm
         real(hp) :: wf(4) !< return value of findnm
         integer  :: fmask(4) !< return value of findnm
         real(hp) :: fsum
         type(tEcMask), pointer :: srcmask
         real(hp), dimension(:), pointer      ::  src_x, src_y
         real(hp)                             ::  tgt_x, tgt_y
         integer                              ::  nsx, nsy
         real(hp), allocatable, dimension(:)  ::  edge_poly_x
         real(hp), allocatable, dimension(:)  ::  edge_poly_y
         real(hp), dimension(:,:),  pointer   ::  sY_2D, sX_2D   !< 2D representation of linearly indexed array arr1D
         real(hp)                             ::  cx, cy, r2
         integer                              ::  nresult, iresult, idx, jsferic
         integer                              ::  iimin, iimax, jjmin, jjmax
         type(kdtree_instance)                ::  treeinst
         real(hp)                             ::  x0, x1, y0, y1, rd

         real(hp), dimension(4) :: xfindpoly, yfindpoly
         integer                :: imin, jmin, iii, jjj

         integer                              :: issparse
         integer, dimension(:),   allocatable :: ia
         integer, dimension(:),   allocatable :: ja
         integer, dimension(:,:), allocatable :: imask

         type(tEcField),          pointer     :: srcfld0, srcfld1

         logical hasKDTree

         issparse = 0

         !
         hasKDTree = .True.
!         hasKDTree = .False.
         success = .false.
         sourceElementSet => null()
         sourceElementSet => connection%sourceItemsPtr(1)%ptr%elementSetPtr
         targetElementSet => connection%targetItemsPtr(1)%ptr%elementSetPtr
         !
         ! Safety check while interpolate_spacetimeSaveWeightFactors is still liberally used.
         if (.not. (connection%converterPtr%ofType == convType_curvi .or. &
                    connection%converterPtr%ofType == convType_polytim .or. &
                    connection%converterPtr%ofType == convType_netcdf)) then
            success = .true.
            return
         end if
         ! Check whether there is anything to be done.
         if (connection%converterPtr%interpolationType == interpolate_spacetimeSaveWeightFactors .or. &
             connection%converterPtr%interpolationType == extrapolate_spacetimeSaveWeightFactors) then
            if (associated(connection%converterPtr%indexWeight)) then
               success = .true.
               return
            end if
         end if
         !
         ! Check whether the Converter's weights need to be allocated.
         if (.not. associated(connection%converterPtr%indexWeight)) then
            allocate(weight)
            connection%converterPtr%indexWeight => weight
         else
            weight => connection%converterPtr%indexWeight
         end if
         !
         if (sourceElementSet%ofType == elmSetType_samples) then
            n_points = targetElementSet%nCoordinates
            do i = 1, connection%nSourceItems
               connection%sourceItemsPtr(i)%ptr%sourceT0fieldPtr%bbox = (/1,1,sourceElementSet%n_cols,1/)
               connection%sourceItemsPtr(i)%ptr%sourceT1fieldPtr%bbox = (/1,1,sourceElementSet%n_cols,1/)
               if (associated(weight%indices)) deallocate(weight%indices)
               allocate(weight%indices(1, n_points))
               connection%converterPtr%indexWeight => weight
               weight%indices = ec_undef_int
               select case (instancePtr%coordsystem)
               case (EC_COORDS_SFERIC)
                  jsferic = 1
               case (EC_COORDS_CARTESIAN)
                  jsferic = 0
               end select
               call nearest_neighbour(n_points, targetElementSet%x, targetElementSet%y,  targetElementSet%mask, &
                    weight%indices(1,:), ec_undef_hp, &
                    sourceElementSet%x, sourceElementSet%y, sourceElementSet%n_cols, jsferic, 0)
            end do
            success = .true.
            return
         end if
         !
         ! Calculate and update the Converter's weights.
         srcmask => connection%converterPtr%srcmask
         select case(connection%converterPtr%ofType)
            case(convType_curvi, convType_netcdf)
               ! bilinear interpolation
               n_cols = sourceElementSet%n_cols
               n_rows = sourceElementSet%n_rows
               n_points = targetElementSet%nCoordinates
               if (associated(weight%indices)) deallocate(weight%indices)
               allocate(weight%indices(2, n_points))
               weight%indices = ec_undef_int
               if (associated(weight%weightFactors)) deallocate(weight%weightFactors)
               allocate(weight%weightFactors(4, n_points))
               allocate(weight%substndx(n_points))
               weight%weightFactors = ec_undef_hp
               weight%substndx = 0
               iimin=n_cols
               jjmin=n_rows
               iimax=0
               jjmax=0
               select case (sourceElementSet%ofType)
                  case (elmSetType_spheric_ortho, elmSetType_Cartesian_ortho)
                     src_x=>sourceElementSet%x
                     src_y=>sourceElementSet%y
                     nsx=n_cols
                     nsy=n_rows

                     if ( connection%converterPtr%ofType == convType_netcdf ) then
                        issparse = 1   ! sparse storage
                     end if

                     if ( issparse == 1 ) then
                        call realloc(imask,(/n_cols,n_rows/),fill=0)
                     end if

                     do i=1, n_points
                        tgt_x=targetElementSet%x(i)
                        tgt_y=targetElementSet%y(i)
                        np = 0
                        mp = 0
                        if ((tgt_x-src_x(1))*(src_x(nsx)-tgt_x)>=0) then            ! point is within in the range
                           do mp=1, nsx-1
                              if ((tgt_x-src_x(mp))*(src_x(mp+1)-tgt_x)>=0) exit    ! find m
                           end do
                           do np=1, nsy-1
                              if ((tgt_y-src_y(np))*(src_y(np+1)-tgt_y)>=0) exit    ! find n
                           end do
                        end if
                        if ((np<=0 .or. np>=nsy) .or. (mp<=0 .or. mp>=nsx)) then    ! if one coordinate out of bounds, both indices are zero
                           np=0
                           mp=0
                        end if

                        if (np>0 .and. mp>0) then
                           wx = (targetElementSet%x(i) - sourceElementSet%x(mp))/(sourceElementSet%x(mp+1) - sourceElementSet%x(mp))
                           wy = (targetElementSet%y(i) - sourceElementSet%y(np))/(sourceElementSet%y(np+1) - sourceElementSet%y(np))
                           weight%weightFactors(1,i) = (1.-wx)*(1-wy)                 ! 4        3
                           weight%weightFactors(2,i) = (   wx)*(1-wy)
                           weight%weightFactors(3,i) = (   wx)*(  wy)
                           weight%weightFactors(4,i) = (1.-wx)*(  wy)                 ! 1        2
                        endif

                        weight%indices(1,i) = np
                        weight%indices(2,i) = mp

                        if ( np>0 .and. mp>0 ) then
                           iimin=min(weight%indices(1,i),iimin)
                           jjmin=min(weight%indices(2,i),jjmin)
                           iimax=max(weight%indices(1,i)+1,iimax)
                           jjmax=max(weight%indices(2,i)+1,jjmax)

                           if ( issparse == 1 ) then
   !                          update mask array
                              imask(mp,  np)   = 1
                              imask(mp+1,np)   = 1
                              imask(mp,  np+1) = 1
                              imask(mp+1,np+1) = 1
                           end if
                        end if
                     end do
                  case (elmSetType_spheric, elmSetType_Cartesian)
                     if (hasKDTree) then            ! new style, using kdtree
                        ! adopt the global jsferic setting of the EC-module instance
                        !if (instancePtr%coordsystem==EC_COORDS_SFERIC) then
                        !    jsferic = 1
                        !else
                        !    jsferic = 0
                        !endif
                        jsferic = 0                             ! For now, EC-converter consistently regards all coordinates as cartesian
                        call build_kdtree(treeinst, n_points, targetElementSet%x, targetElementSet%y, ierr, jsferic, ec_undef_hp)
                        n_cols = sourceElementSet%n_cols
                        n_rows = sourceElementSet%n_rows
                        sy_2D(1:n_cols,1:n_rows) => sourceElementSet%y
                        sx_2D(1:n_cols,1:n_rows) => sourceElementSet%x

                        x0 = minval(targetElementSet%x)     ! improvised bounding box (does not work with targets around the 0-meridian)
                        x1 = maxval(targetElementSet%x)
                        y0 = minval(targetElementSet%y)
                        y1 = maxval(targetElementSet%y)
                        do jj=1,n_cols-1
                           do ii=1,n_rows-1
                              cx = (sx_2D(jj,ii)+sx_2D(jj+1,ii)+sx_2D(jj,ii+1)+sx_2D(jj+1,ii+1))/4.0
                              cy = (sy_2D(jj,ii)+sy_2D(jj+1,ii)+sy_2D(jj,ii+1)+sy_2D(jj+1,ii+1))/4.0
                              r2 = (max ( &
                                   dbdistance(sx_2D(jj,ii),sy_2D(jj,ii),sx_2D(jj+1,ii+1),sy_2D(jj+1,ii+1)),          &
                                   dbdistance(sx_2D(jj+1,ii),sy_2D(jj+1,ii),sx_2D(jj,ii+1),sy_2D(jj,ii+1))) / 2.)**2     ! longest diagonal divided by 2 (jasfer3D on if jsferic)
                              rd = sqrt(r2)
                              if (cx+rd>=x0 .and. cx-rd<=x1 .and. cy+rd>=y0 .and. cy-rd<=y1) then                       ! check if source point within the rectangle of interest
                                 call make_queryvector_kdtree(treeinst, cx, cy, jsferic)
                                 nresult = kdtree2_r_count(treeinst%tree, treeinst%qv, r2)
                                 call realloc_results_kdtree(treeinst, nresult)
                                 call kdtree2_n_nearest(treeinst%tree, treeinst%qv, nresult, treeinst%results)
                                 ! loop over find results with pinpok
                                 do iresult = 1, nresult
                                    idx = treeinst%results(iresult)%idx
                                    call pinpok(targetElementSet%x(idx), targetElementSet%y(idx), 5,                       &
                                       (/sx_2D(jj,ii), sx_2D(jj,ii+1), sx_2D(jj+1,ii+1), sx_2D(jj+1,ii),  sx_2D(jj,ii)/),  &
                                       (/sy_2D(jj,ii), sy_2D(jj,ii+1), sy_2D(jj+1,ii+1), sy_2D(jj+1,ii),  sy_2D(jj,ii)/),  &
                                        inside)
                                    if (inside == 1) then
                                        ! [jj,ii] and [jj+1,ii] and [jj,ii+1] and [jj+1,ii+1] are used
                                        ! Do stuff for a target point in a source cell
                                        weight%indices(1,idx) = ii
                                        weight%indices(2,idx) = jj
                                        iimin=min(ii,iimin)
                                        jjmin=min(jj,jjmin)
                                        iimax=max(ii+1,iimax)
                                        jjmax=max(jj+1,jjmax)
                                    end if
                                 end do
                              end if
                           end do
                        end do
                        call delete_kdtree2(treeinst)

                        ! RL: The sourceElementset x and y arrays span the full meteo grid and should now also be deallocated !
                        ! deallocate(sourceElementSet%x)
                        ! deallocate(sourceElementSet%y)

                        do i = 1, n_points
                           imin = weight%indices(1,i)
                           jmin = weight%indices(2,i)

                           if ( imin/=ec_undef_int .and. jmin/=ec_undef_int ) then
                              do iii=imin,imin+1
                                 do jjj=jmin,jmin+1
                                    idx = 1 + (jjj-jmin) + 2*(iii-imin)
   !                                 idx = 1 + (iii-imin) + 2*(jjj-jmin)
                                    xfindpoly(idx) = sx_2D(jjj,iii)
                                    yfindpoly(idx) = sy_2D(jjj,iii)
                                 end do
                              end do

                              call findnm(targetElementSet%x(i), targetElementSet%y(i), xfindpoly, yfindpoly , &
                                              2, 2, inside, mp, np, in, jn, wf)

                              weight%weightFactors(1,i) = wf(1)
                              weight%weightFactors(2,i) = wf(2)
                              weight%weightFactors(3,i) = wf(3)
                              weight%weightFactors(4,i) = wf(4)
                           end if
                        end do
                     else                           ! old style, not using kdtree
                        allocate(edge_poly_x(2*n_cols+2*n_rows-2), edge_poly_y(2*n_cols+2*n_rows-2), stat=ierr)
                        if (ierr /= 0) then
                            call setECMessage("ERROR: ec_converter::ecConverterUpdateWeightFactors: Unable to allocate additional memory.")
                            return
                        endif
                        j=1
                        do i=1,n_cols
                            edge_poly_x(j) = sourceElementSet%x(i)
                            edge_poly_y(j) = sourceElementSet%y(i)
                            j=j+1
                        enddo
                        do i=2,n_rows
                            edge_poly_x(j) = sourceElementSet%x(i*n_cols)
                            edge_poly_y(j) = sourceElementSet%y(i*n_cols)
                            j=j+1
                        enddo
                        do i=1,n_cols-1
                            edge_poly_x(j) = sourceElementSet%x(n_rows*n_cols-i)
                            edge_poly_y(j) = sourceElementSet%y(n_rows*n_cols-i)
                            j=j+1
                        enddo
                        do i=1,n_rows-1
                            edge_poly_x(j) = sourceElementSet%x((n_rows-1-i)*n_cols+1)
                            edge_poly_y(j) = sourceElementSet%y((n_rows-1-i)*n_cols+1)
                            j=j+1
                        enddo
                        edge_poly_x(j) = edge_poly_x(1)
                        edge_poly_y(j) = edge_poly_x(1)

                        do i=1, n_points
                           call pinpok(targetElementSet%x(i), targetElementSet%y(i), 2*n_cols+2*n_rows-2, edge_poly_x, edge_poly_y, inside)
                           if (inside == 1) then
                              call findnm(targetElementSet%x(i), targetElementSet%y(i), sourceElementSet%x, sourceElementSet%y, &
                                           n_cols, n_rows, inside, mp, np, in, jn, wf)
                              if (inside == 1) then
                                 if (allocated(srcmask%msk)) then
                                    fmask(1) = (srcmask%msk((np   -srcmask%nmin)*srcmask%mrange+mp   -srcmask%mmin+1))
                                    fmask(2) = (srcmask%msk((np   -srcmask%nmin)*srcmask%mrange+mp+1 -srcmask%mmin+1))
                                    fmask(3) = (srcmask%msk((np+1 -srcmask%nmin)*srcmask%mrange+mp+1 -srcmask%mmin+1))
                                    fmask(4) = (srcmask%msk((np+1 -srcmask%nmin)*srcmask%mrange+mp   -srcmask%mmin+1))
                                    fsum = sum((1.d0-fmask)*wf)            ! fmask = 1 for DISCARDED corners
                                    if (fsum>=1.0e-03) then
                                       wf = (wf*(1.d0-fmask))/fsum
                                    endif
                                 endif
                                 weight%weightFactors(1,i) = wf(1)
                                 weight%weightFactors(2,i) = wf(2)
                                 weight%weightFactors(3,i) = wf(3)
                                 weight%weightFactors(4,i) = wf(4)
                                 if ( mp>0 .and. np>0 ) then
                                    iimin=min(weight%indices(1,i),iimin)
                                    jjmin=min(weight%indices(2,i),jjmin)
                                    iimax=max(weight%indices(1,i)+1,iimax)
                                    jjmax=max(weight%indices(2,i)+1,jjmax)
                                 end if
                                 weight%indices(1,i) = mp
                                 weight%indices(2,i) = np
                              endif
                           endif
                        end do
                        deallocate(edge_poly_x)
                        deallocate(edge_poly_y)
                     endif                         ! old style, not using kdtree
                  case default
                     call setECMessage("Unknown element type set for interpolation weights in NetCDF file.")
                     return
                  end select

                  if ( issparse == 1 ) then
!                    make sparsity pattern, effectively disable bounding box and overwrite indices
                     call MaskToSparse(n_cols,n_rows,imask,ia,ja)

!                    deallocate mask
                     if ( allocated(imask) ) deallocate(imask)

!                    copy sparsity pattern to source fields and effectively disable bounding box
                     do i = 1, connection%nSourceItems
                        srcfld0 => connection%sourceItemsPtr(i)%ptr%sourceT0fieldPtr
                        srcfld1 => connection%sourceItemsPtr(i)%ptr%sourceT1fieldPtr

                        call SetSparsityPattern(srcfld0, n_cols, n_rows, ia, ja)
                        call SetSparsityPattern(srcfld1, n_cols, n_rows, ia, ja)
                     end do

!                    overwrite indices (mcol,nrow) with sparse indices
                     call ConvertToSparseIndices(n_points, weight%indices, n_rows, ia, ja)

!                    deallocate sparsity pattern
                     if ( allocated(ia) ) deallocate(ia)
                     if ( allocated(ja) ) deallocate(ja)
                  else

                      ! Shift indices (m and n), to run from (1,1) to (iimax-iimin+1,jjmax-jjmin+1), only for the netCDF type
                     if (connection%converterPtr%ofType == convType_netcdf) then
                        do i = 1, n_points
                           weight%indices(1,i) = weight%indices(1,i) - iimin + 1
                           weight%indices(2,i) = weight%indices(2,i) - jjmin + 1
                        end do
                     endif
                     ! Set bounding box for reading in the source T0 and T1 fields
                     do i = 1, connection%nSourceItems
                        connection%sourceItemsPtr(i)%ptr%sourceT0fieldPtr%bbox = (/jjmin,iimin,jjmax,iimax/)
                        connection%sourceItemsPtr(i)%ptr%sourceT1fieldPtr%bbox = (/jjmin,iimin,jjmax,iimax/)
                     end do
                     if (iimax == 0 .and. jjmax == 0) then
                         write(*,*) "WARNING: ec_converter: no overlapping points found for quantity:"
                         write(*,*) trim(connection%sourceItemsPtr(1)%ptr%quantityPtr%name)
                         write(*,*) "This is allowed in parallel runs"
                     endif
                  end if
                  ! Final step for gridded providers: when not masked, reset indices to undefined.
                  if (associated(targetElementSet%mask)) then
                     do i = 1, n_points
                        if (targetElementSet%mask(i) == 0) then
                           weight%indices(:,i) = ec_undef_int
                        end if
                     end do
                  end if

               success = .true.
            case(convType_polytim)
               sourceElementSet => connection%sourceItemsPtr(1)%ptr%elementSetPtr
               targetElementSet => connection%targetItemsPtr(1)%ptr%elementSetPtr
               n_points = targetElementSet%nCoordinates
               if (associated(weight%indices)) deallocate(weight%indices)
               allocate(weight%indices(2, n_points))
               weight%indices = ec_undef_int
               if (associated(weight%weightFactors)) deallocate(weight%weightFactors)
               allocate(weight%weightFactors(2, n_points))
               weight%weightFactors = ec_undef_hp
               if (     connection%converterPtr%interpolationType /= interpolate_passthrough &
                  .and. connection%converterPtr%interpolationType /= interpolate_time        &
                  .and. connection%converterPtr%interpolationType /= interpolate_time_extrapolation_ok ) then
                  do i=1, n_points
                     call polyindexweight(targetElementSet%x(i), &
                                          targetElementSet%y(i), &
                                          targetElementSet%xyen(1,i), &
                                          targetElementSet%xyen(2,i), &
                                          sourceElementSet%x, &
                                          sourceElementSet%y, &
                                          sourceElementSet%mask, &
                                          sourceElementSet%nCoordinates, &
                                          weight%indices(1,i), &       ! kL
                                          weight%weightFactors(1,i), & ! wL
                                          weight%indices(2,i), &       ! kR
                                          weight%weightFactors(2,i))   ! wR
                  end do
               end if
               success = .true.
            case default
               success = .true.
         end select
      end function ecConverterUpdateWeightFactors

      ! =======================================================================

      subroutine findnm(xl, yl, x, y, mc, nc, inside, mv, nv, in, jn, wf)
         real(hp),               intent(in)  :: xl, yl !< coordinates of point in target grid
         real(hp), dimension(:), intent(in)  :: x, y   !< coordinates of points in source grid
         integer,                intent(in)  :: mc, nc !< maximum and actual dimensions
         integer,                intent(out) :: inside, mv, nv, in, jn
         real(hp),               intent(out) :: wf(4)
         !
         real(hp) :: xx(4), yy(4), xk(3), yk(3)
         integer :: ishot, i, j, mz, nz, m1, m2, n1, n2, insidet, i1, i2, ier
         integer :: mvol = 0
         integer :: nvol = 0
         real(hp) :: dx, dy, r, rmin, xxc, yyc
         !
         if (mc == 0 .or. nc == 0) return
         ishot = 0
         rmin  = 99d+20
         !
5        continue
         mv = 0
         nv = 0
         if (mvol /= 0) then
            mz = mvol
            nz = nvol
         else
            ! Loop over all source ElementSet grid points.
            ! FM's rows,columns have been flattened into a 1D array.
            do i = 1,mc
               do j = 1,nc
                  dx = xl - x(i+(j-1)*mc)
                  dy = yl - y(i+(j-1)*mc)
                  r  = dx*dx + dy*dy
                  if (r < rmin) then
                     rmin = r
                     mz   = i
                     nz   = j
                  endif
               end do
            end do
         endif
         !
         m1     = max(1,mz-2)
         n1     = max(1,nz-2)
         m2     = min(mc-1 ,mz+1)
         n2     = min(nc-1 ,nz+1)
         inside = 0
         mvol   = 0
         nvol   = 0
         do i = m1,m2
            do j = n1-1,n2-1
               xx(1) = x(i+(j*mc))
               xx(2) = x(i+1+(j*mc))
               xx(3) = x(i+1+((j+1)*mc))
               xx(4) = x(i+((j+1)*mc))
               yy(1) = y(i+(j*mc))
               yy(2) = y(i+1+(j*mc))
               yy(3) = y(i+1+((j+1)*mc))
               yy(4) = y(i+((j+1)*mc))
               call pinpok(xl, yl, 4, xx, yy, inside)
               if (inside == 1) then
                  call bilin5( xx, yy, xl, yl, wf, ier)
                  mvol = i
                  nvol = j+1
                  mv   = i
                  nv   = j+1
                  ! determine quadrant
                  xxc  = ( xx(1) + xx(2) + xx(3) + xx(4) )/4
                  yyc  = ( yy(1) + yy(2) + yy(3) + yy(4) )/4
                  in   = 0
                  jn   = 0
                  do i1 = 1,4
                     i2    = mod(i1,4) + 1
                     xk(1) = xx(i1)
                     yk(1) = yy(i1)
                     xk(2) = xx(i2)
                     yk(2) = yy(i2)
                     xk(3) = xxc
                     yk(3) = yyc
                     call pinpok(xl, yl, 3, xk, yk, insidet)
                     if (insidet == 1) then
                        if ( i1 == 1) jn = -1
                        if ( i1 == 2) in =  1
                        if ( i1 == 3) jn =  1
                        if ( i1 == 4) in = -1
                        return
                     else if (i1 == 4) then
                        return
                     endif
                  end do
               endif
            end do
         end do
         !
         if (ishot == 1) return
         ishot = 1
         goto 5
      end subroutine findnm

      ! =======================================================================

      subroutine bilin5(xa, ya, x0, y0, w, ier)
         ! Author: H. Petit
         integer               , intent(out) :: ier
         real(hp)              , intent(in)  :: x0
         real(hp)              , intent(in)  :: y0
         real(hp), dimension(4), intent(out) :: w
         real(hp), dimension(4), intent(in)  :: xa
         real(hp), dimension(4), intent(in)  :: ya
         !
         ! Local variables
         !
         real(hp) :: a
         real(hp) :: a21
         real(hp) :: a22
         real(hp) :: a31
         real(hp) :: a32
         real(hp) :: a41
         real(hp) :: a42
         real(hp) :: b
         real(hp) :: c
         real(hp) :: det
         real(hp) :: discr
         real(hp) :: eta
         real(hp) :: x
         real(hp) :: x1
         real(hp) :: x2
         real(hp) :: x3
         real(hp) :: x3t
         real(hp) :: x4
         real(hp) :: xi
         real(hp) :: xt
         real(hp) :: y
         real(hp) :: y1
         real(hp) :: y2
         real(hp) :: y3
         real(hp) :: y3t
         real(hp) :: y4
         real(hp) :: yt
         !
         !! executable statements -------------------------------------------------------
         !
         ! read(12,*)x1,y1,f1
         x1 = xa(1)
         y1 = ya(1)
         ! read(12,*)x2,y2,f2
         x2 = xa(2)
         y2 = ya(2)
         ! read(12,*)x3,y3,f3
         x3 = xa(3)
         y3 = ya(3)
         ! read(12,*)x4,y4,f4
         x4 = xa(4)
         y4 = ya(4)
         x  = x0
         y  = y0
         !
         ! The bilinear interpolation problem is first transformed
         ! to the quadrangle with nodes (0,0),(1,0),(x3t,y3t),(0,1)
         ! and required location (xt,yt)
         !
         a21 = x2 - x1
         a22 = y2 - y1
         a31 = x3 - x1
         a32 = y3 - y1
         a41 = x4 - x1
         a42 = y4 - y1
         det = a21*a42 - a22*a41
         if (abs(det) < 1.0e-20_hp) then
            ier = 1
            goto 99999
         endif
         x3t = (  a42*a31      - a41*a32     ) / det
         y3t = ( -a22*a31      + a21*a32     ) / det
         xt  = (  a42*(x - x1) - a41*(y - y1)) / det
         yt  = ( -a22*(x - x1) + a21*(y - y1)) / det
         if ((x3t < 0.0_hp) .or. (y3t < 0.0_hp)) then
            ! write (*, *) 'distorted quadrangle'
            ier = 1
            goto 99999
         endif
         if (abs(x3t - 1.0_hp) < 1.0e-7_hp) then
            xi = xt
            if (abs(y3t - 1.0_hp) < 1.0e-7_hp) then
               eta = yt
            elseif (abs(1.0_hp + (y3t - 1.0_hp)*xt) < 1.0e-6_hp) then
               ! write (*, *) 'extrapolation over too large a distance'
               ier = 1
               goto 99999
            else
               eta = yt / (1.0_hp + (y3t - 1.0_hp)*xt)
            endif
         elseif (abs(y3t - 1.0_hp) < 1.0e-6_hp) then
            eta = yt
            if (abs(1.0_hp + (x3t - 1.0_hp)*yt) < 1.0e-6_hp) then
               ! write (*, *) 'extrapolation over too large a distance'
               ier = 1
               goto 99999
            else
               xi = xt / (1.0_hp + (x3t - 1.0_hp)*yt)
            endif
         else
            a     = y3t - 1.0_hp
            b     = 1.0_hp + (x3t - 1.0_hp)*yt - (y3t - 1.0_hp)*xt
            c     = -xt
            discr = b*b - 4.0_hp*a*c
            if (discr < 1.0e-6_hp) then
               ! write (*, *) 'extrapolation over too large a distance'
               ier = 1
               goto 99999
            endif
            xi  = (-b + sqrt(discr)) / (2.0_hp*a)
            eta = ((y3t - 1.0_hp)*(xi - xt) + (x3t - 1.0_hp)*yt) / (x3t - 1.0_hp)
         endif
         w(1) = (1.0_hp-xi) * (1.0_hp-eta)
         w(2) =         xi  * (1.0_hp-eta)
         w(3) =         xi  *         eta
         w(4) =        eta  * (1.0_hp-xi )
         return
99999    continue
      end subroutine bilin5

      ! =======================================================================

      subroutine pinpok(xl, yl, n, x, y, inside)
         ! Author: H. Kernkamp
         double precision              , intent(in)  :: xl, yl           ! point under consideration
         integer                       , intent(in)  :: n
         double precision, dimension(n), intent(in)  :: x, y             ! polygon(n)
         integer                      , intent(out)  :: inside

         double precision, parameter      :: dmiss_default  = -999.0_hp       ! Default missing value in meteo arrays
         integer  :: i, i1, i2, np, rechts
         double precision :: rl, rm, x1, x2, y1, y2

         if (n .le. 2) then
            inside = 1
         else
            np = 0
 5          continue
            np = np + 1
            if (np .le. n) then
               if ( x(np) .ne. dmiss_default) goto 5
            endif
            np = np - 1
            inside = 0
            rechts = 0
            i = 0
10          continue
            i1 = mod(i,np) + 1
            i2 = mod(i1,np) + 1
            x1 = x(i1)
            x2 = x(i2)
            y1 = y(i1)
            y2 = y(i2)
            if (xl .ge. min(x1,x2) .and. xl .le. max(x1,x2) ) then
               if (xl  ==  x1 .and. yl  ==  y1 .or. &                     ! tussen of op lijnstuk
                  (x1  ==  x2 .and. &                                     ! op punt 1
                   yl .ge. min(y1,y2) .and. yl .le. max(y1,y2) ) .or. &   ! op verticale lijn
                  (yl  ==  y1 .and. y1  ==  y2)  ) then                   ! op horizontale lijn
                  inside = 1
                  return
               else if (x1 .ne. x2) then                                  ! scheve lijn
                  rl = ( xl - x1 )  / ( x2 - x1 )
                  rm = ( y1 - yl )  + rl * ( y2 - y1 )
                  if (rm  ==  0) then                                     ! op scheve lijn
                     inside = 1
                     return
                  else if (rm .gt. 0d0) then                              ! onder scheve lijn
                     if (xl  ==  x1 .or. xl  ==  x2) then
                        if (x1 .gt. xl .or. x2 .gt. xl) then
                           rechts = rechts + 1
                        endif
                     endif
                     inside = 1 - inside
                  endif
               endif
            endif
            i = i + 1
            if (i .lt. np) goto 10
            if (mod(rechts,2) .ne. 0) inside = 1 - inside
         endif
      end subroutine pinpok

      ! =======================================================================

      !> Execute the Converters in the Connection sequentially.
      function ecConverterPerformConversions(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         type(c_time),        intent(in)    :: timesteps  !< convert to this number of timesteps since the kernel's reference date
         !
         success = .false.
         !
         if (.not.associated(connection%converterPtr)) then
            ! There is no converter needed here (it's optional), so return directly.
            success = .true.
            return
         endif
         select case(connection%converterPtr%ofType)
            case (convType_uniform)
               success = ecConverterUniform(connection, timesteps%mjd())
            case (convType_uniform_to_magnitude)
               success = ecConverterUniformToMagnitude(connection, timesteps%mjd())
            case (convType_unimagdir)
               success = ecConverterUnimagdir(connection, timesteps%mjd())
            case(convType_spiderweb)
               success = ecConverterSpiderweb(connection, timesteps%mjd())
            case(convType_fourier)
               success = ecConverterFourier(connection, timesteps)
            case(convType_arcinfo)
               success = ecConverterArcinfo(connection, timesteps%mjd())
            case(convType_curvi)
               success = ecConverterCurvi(connection, timesteps%mjd())
            case(convType_polytim)
               success = ecConverterPolytim(connection, timesteps%mjd())
            case(convType_netcdf)
               success = ecConverterNetcdf(connection, timesteps%mjd())
            case(convType_qhtable)
               success = ecConverterQhtable(connection)
            case(convType_samples)
               success = ecConverterSamples(connection, timesteps%mjd())
            case default
               call setECMessage("ERROR: ec_converter::ecConverterPerformConversions: Unknown Converter type requested.")
         end select
         if (success) then
             success = ecConverterUpdateScalar(connection)
         endif
      end function ecConverterPerformConversions

      ! =======================================================================

      !> Calculate weight factors from times.
      subroutine time_weight_factors(a0, a1, timesteps, t0, t1, extrapolated, timeint)
         !
         ! parameters
         real(hp), intent(inout) :: a0, a1
         real(hp), intent(in)    :: t1, t0, timesteps
         logical,  intent(out), optional  :: extrapolated
         integer,  intent(in),  optional  :: timeint
         !
         ! locals
         logical :: l_extrapolated
         integer :: l_timeint
         !
         ! body
         l_timeint = timeint_lin
         if (present(timeint)) then
            l_timeint = timeint
         endif
         select case (l_timeint)
         case (timeint_lin, timeint_lin_extrapol)
            l_extrapolated = .false.
            if (comparereal(timesteps,t0) == -1) then
               l_extrapolated = .true.
               a0 = 1.0_hp
               a1 = 0.0_hp
            endif
            if (comparereal(timesteps,t1) == 1) then
               l_extrapolated = .true.
               a0 = 0.0_hp
               a1 = 1.0_hp
            endif
            if (.not. l_extrapolated) then
               a0 = 1.0_hp
               a1 = 0.0_hp
               !
               if (comparereal(t0, t1) /= 0) then
                  a1 = real((timesteps-t0)/(t1-t0), hp)
                  a0 = a0 - a1
               endif
            endif
            if (present(extrapolated)) extrapolated = l_extrapolated
         case (timeint_bto)
            a0 = 0.0d0
            a1 = 1.0d0
         case (timeint_bfrom)
            a0 = 1.0d0
            a1 = 0.0d0
         case (timeint_rainfall)  ! constant rainfall intensity from time-integrated amount
            a0 = 0.0d0
            a1 = 1.d0/(t1-t0)
         case default
            ! invalid interpolation method
            return
         end select

      end subroutine time_weight_factors

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a uniform FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array, as well all as overwriting only one array element.
      !! Converts source(i) to target(i).
      function ecConverterUniform(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp)                            :: t0, t1        !< source item t0 and t1
         real(hp)                            :: a0, a1        !< weight for source t0 and t1 data
         integer                             :: n_data        !< number of values
         real(hp), dimension(:), pointer     :: valuesT0      !< values at time t0
         real(hp), dimension(:), pointer     :: valuesT1      !< values at time t1
         real(hp), dimension(:), allocatable :: valuesT       !< values at time t
         integer                             :: istat         !< allocation status
         integer                             :: i, j          !< loop counters
         type(tEcField),         pointer     :: targetField   !< Converter's result goes in here
         integer                             :: maxlay        !< maximum number of layers (3D)
         integer                             :: from, thru    !< contiguous range of indices in the target array
         integer                             :: jmin, jmax    !< from target position jmin through target position jmax is filled
         !
         integer, dimension(:), pointer :: targetMask
         success = .false.
         valuesT0 => null()
         valuesT1 => null()
         targetField => null()

         t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
         t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
         valuesT0 => connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr
         valuesT1 => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr
         n_data = connection%sourceItemsPtr(1)%ptr%quantityPtr%vectorMax
         if (associated(connection%targetItemsPtr(1)%ptr%ElementSetPtr%z)) then
            maxlay = size(connection%targetItemsPtr(1)%ptr%ElementSetPtr%z) / size(connection%targetItemsPtr(1)%ptr%ElementSetPtr%x)
         else
            maxlay = 1
         endif
         allocate(valuesT(maxlay*n_data), stat=istat)
         valuesT=ec_undef_hp
         if (connection%converterPtr%interpolationType == interpolate_passthrough)then
            !
            ! ===== block function (no interpolation in time) =====
            !
            if(timesteps.lt.t1) then
               ! use valuesT0 when timesteps is less than t1
               do i=1, size(valuesT0,dim=1)
                  valuesT(i) = valuesT0(i)
               end do
            else
               ! use valuesT1 when timesteps is equals to t1 (timesteps should never be higher than t1)
               do i=1, size(valuesT0,dim=1)
                  valuesT(i) = valuesT1(i)
               end do
            endif
            !
         else
            !
            ! ===== interpolation in time =====
            !
            if (.not.connection%sourceItemsPtr(1)%ptr%quantityptr%constant) then
               select case(connection%sourceItemsPtr(1)%ptr%quantityptr%timeint)
               case (timeint_lin, timeint_lin_extrapol, timeint_rainfall)
                  ! linear interpolation in time
                  call time_weight_factors(a0, a1, timesteps, t0, t1,  &
                                           timeint = connection%sourceItemsPtr(1)%ptr%quantityptr%timeint)
               case (timeint_bto)
                  a0 = 0.0d0
                  a1 = 1.0d0
               case (timeint_bfrom)
                  a0 = 1.0d0
                  a1 = 0.0d0
               end select
            !
               do i=1, size(valuesT0,dim=1)
                  ! "val0+(val1-val0)*a1" is more precise than "val0*a0+val1*a1" when val0 and val1 are huge
                  valuesT(i) = valuesT0(i) * (a1+a0)  + (valuesT1(i)-valuesT0(i)) * a1
               end do
            else
               do i=1, size(valuesT0,dim=1)
                  ! "val0+(val1-val0)*a1" is more precise than "val0*a0+val1*a1" when val0 and val1 are huge
                  valuesT(i) = valuesT0(i)
               end do
            endif
         endif

         select case(connection%converterPtr%interpolationType)
            case (interpolate_passthrough, interpolate_timespace)
               ! ===== operation =====
               ! Check target Item(s).
               do i=1, connection%nTargetItems
                  if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                     call setECMessage("ERROR: ec_converter::ecConverterUniform: Target ElementSet's number of coordinates not set.")
                     return
                  end if
               end do
               ! Select the operation.

               if (connection%converterPtr%targetIndex/=ec_undef_int) then
                  jmin = connection%converterPtr%targetIndex
                  jmax = connection%converterPtr%targetIndex
               else
                  jmin = 1
                  jmax = connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
               end if

               select case(connection%converterPtr%operandType)
                  case(operand_replace, operand_replace_if_value)
                     ! Write all values to one target Item or each value to its own target Item.
                     if (connection%nTargetItems == 1) then                               ! All in one target item
                        targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                        targetMask => connection%targetItemsPtr(1)%ptr%elementSetPtr%mask
                        do j=jmin, jmax
                           if (associated(targetmask)) then
                              if (targetmask(j) == 0) then
                                 cycle
                              end if
                           end if

                           from = (j-1)*(maxlay*n_data)+1
                           thru = (j  )*(maxlay*n_data)
                           if (allocated(connection%converterPtr%srcmask%msk)) then
                              targetField%arr1dPtr(from:thru) = valuesT * real(connection%converterPtr%srcmask%msk(from:thru),hp)
                           else
                              targetField%arr1dPtr(from:thru) = valuesT
                           endif
                        end do
                        targetField%timesteps = timesteps
                     else if (connection%nTargetItems == maxlay*n_data) then              ! Separate target items
                        do i=1, connection%nTargetItems
                           targetField => connection%targetItemsPtr(i)%ptr%targetFieldPtr
                           targetMask => connection%targetItemsPtr(i)%ptr%elementSetPtr%mask
                           do j=jmin, jmax
                              if (associated(targetmask)) then
                                 if (targetmask(j) == 0) then
                                    cycle
                                 end if
                              end if
                              targetField%arr1dPtr(j) = valuesT(i)
                           end do
                           targetField%timesteps = timesteps
                        end do
                     else
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Number of source Quantities does not match the number of target Items.")
                        return
                     end if
                  case(operand_replace_element) ! TODO: AvD/EB: why does operand_replace require a targetIndex, whereas operand_add does not?
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Converter's target Field array index not set.")
                        return
                     end if
                     targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                     j = connection%converterPtr%targetIndex
                     from = (j-1)*(maxlay*n_data)+1
                     thru = (j  )*(maxlay*n_data)
                     ! NOTE: No targetMask is checked here
                     targetField%arr1dPtr(from:thru) = valuesT
                     targetField%timesteps = timesteps
                  case(operand_add) ! TODO: AvD/EB: it seems that operand_add does not support targetIndex (offset). Should we not make this available?
                     ! Add all values to one target Item or each value to its own target Item.
                     if (connection%nTargetItems == 1) then                               ! All in one target item
                        targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                        targetMask => connection%targetItemsPtr(1)%ptr%elementSetPtr%mask
                        do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                           if (associated(targetmask)) then
                              if (targetmask(j) == 0) then
                                 cycle
                              end if
                           end if
                           from = (j-1)*(maxlay*n_data)+1
                           thru = (j  )*(maxlay*n_data)
                           targetField%arr1dPtr(from:thru) = targetField%arr1dPtr(from:thru) + valuesT
                        end do
                        targetField%timesteps = timesteps
                     else if (connection%nTargetItems == maxlay*n_data) then              ! Separate target items
                        do i=1, connection%nTargetItems
                           targetField => connection%targetItemsPtr(i)%ptr%targetFieldPtr
                           targetMask => connection%targetItemsPtr(i)%ptr%elementSetPtr%mask
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              if (associated(targetmask)) then
                                 if (targetmask(j) == 0) then
                                    cycle
                                 end if
                              end if
                              targetField%arr1dPtr(j) = targetField%arr1dPtr(j) + valuesT(i)
                           end do
                           targetField%timesteps = timesteps
                        end do
                     else
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Number of source Quantities does not match the number of target Items.")
                        return
                     end if
                  case(operand_add_element)
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Converter's target Field array index not set.")
                        return
                     end if
                     targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                     j = connection%converterPtr%targetIndex
                     from = (j-1)*(maxlay*n_data)+1
                     thru = (j  )*(maxlay*n_data)
                     ! NOTE: No targetMask is checked here
                     targetField%arr1dPtr(from:thru) = targetField%arr1dPtr(from:thru) + valuesT
                     targetField%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUniform: Unsupported operand type requested.")
                     return
               end select
            case (interpolate_time, interpolate_time_extrapolation_ok) ! performs implicit space conversion from 2D to 3D,
               ! ===== operation =====
               select case(connection%converterPtr%operandType)
                  case(operand_replace_element)
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Converter's target Field array index not set.")
                        return
                     end if
                     targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                     from = (connection%converterPtr%targetIndex-1)*(maxlay*n_data)+1
                     thru =  connection%converterPtr%targetIndex   *(maxlay*n_data)
                     ! NOTE: No target mask is checked here
                     targetField%arr1dPtr(from:thru) = valuesT

                     ! NOTE: AvD: call site now should account for n_data in the offset already, and I changed the above line to that as well.
                     !            But: can we expect from user/call site that it also accounts for maxlay? (at the moment we have no 3D target
                     !            element sets which *also* have n_data > 1, so may not be a problem yet)
                     ! targetField%arr1dPtr((connection%converterPtr%targetIndex-1)*maxlay*n_data + 1 : connection%converterPtr%targetIndex*maxlay*n_data) = valuesT
                     ! NOTE: RL: Currently this most definitely not the case: targetindex is not set correctly upon init.
                     targetField%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUniform: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterUniform: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
         deallocate(valuesT, stat=istat)
      end function ecConverterUniform






      ! =======================================================================

      !> Perform the configured conversion, if supported, for a uniform FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array, as well all as overwriting only one array element.
      !! Converts all sources into one target: the magnitude of the wind.
      function ecConverterUniformToMagnitude(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer                 :: j           !< loop counter
         real(hp)                :: t0          !< source item t0
         real(hp)                :: t1          !< source item t1
         real(hp)                :: a0          !< weight for source t0 data
         real(hp)                :: a1          !< weight for source t1 data
         real(hp)                :: windXT0     !< wind x-component value at time t0
         real(hp)                :: windXT1     !< wind x-component value at time t1
         real(hp)                :: windYT0     !< wind y-component value at time t0
         real(hp)                :: windYT1     !< wind y-component value at time t1
         real(hp)                :: windXT      !< wind x-component value at time t
         real(hp)                :: windYT      !< wind y-component value at time t
         real(hp)                :: magnitude   !< wind magnitude at time t
         type(tEcField), pointer :: targetField !< Converter's result goes in here
         !
         success = .false.
         targetField => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_time)
               ! linear interpolation in time
               t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
               t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
               call time_weight_factors(a0, a1, timesteps, t0, t1)
               !
               targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
               windXT0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)
               windXT1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(1)
               windYT0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(2)
               windYT1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(2)
               windXT = windXT0*a0 + windXT1*a1
               windYT = windYT0*a0 + windYT1*a1
               magnitude = sqrt(windXT*windXT + windYT*windYT)
               ! ===== operation =====
               select case(connection%converterPtr%operandType)
                  case(operand_replace, operand_replace_if_value)
                     if (connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Target ElementSet's number of coordinates not set.")
                        return
                     end if
                     do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        targetField%arr1dPtr(j) = magnitude
                     end do
                     targetField%timesteps = timesteps
                  case(operand_replace_element)
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Converter's target Field array index not set.")
                        return
                     end if
                     targetField%arr1dPtr(connection%converterPtr%targetIndex) = magnitude
                     targetField%timesteps = timesteps
                  case(operand_add)
                     if (connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Target ElementSet's number of coordinates not set.")
                        return
                     end if
                     do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        targetField%arr1dPtr(j) = targetField%arr1dPtr(j) + magnitude
                     end do
                     targetField%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterUniformToMagnitude

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a unimagdir FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array.
      !! Converts wind_magnitude and wind_direction into wind_u and wind_v.
      !! Assumes target(1) == wind_u and target(2) == wind_v.
      !! meteo1 : regdir, magdir2uv
      function ecConverterUnimagdir(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp)                        :: t0             !< source item t0
         real(hp)                        :: t1             !< source item t1
         real(hp)                        :: a0             !< weight for source t0 data
         real(hp)                        :: a1             !< weight for source t1 data
         real(hp)                        :: windspeed0     !< wind speed at timesteps t0
         real(hp)                        :: windspeed1     !< wind speed at timesteps t1
         real(hp)                        :: windspeed      !< time interpolated wind speed
         real(hp)                        :: winddirection0 !< wind direction at timesteps t0
         real(hp)                        :: winddirection1 !< wind direction at timesteps t1
         real(hp)                        :: winddirection  !< time interpolated wind direction
         real(hp), dimension(:), pointer :: u              !< calculated u value
         real(hp), dimension(:), pointer :: v              !< calculated v value
         integer                         :: i              !< loop counter
         real(hp)                        :: targetU        !< new u value to be written to all target grid points
         real(hp)                        :: targetV        !< new v value to be written to all target grid points
         !
         success = .false.
         u => null()
         v => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_timespace)
               ! === linear interpolation in time ===
               t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
               t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
               call time_weight_factors(a0, a1, timesteps, t0, t1)
               ! determine windspeed at t=timesteps
               windspeed0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)
               windspeed1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(1)
               windspeed = a0*windspeed0 + a1*windspeed1
               ! determine winddirection at t=timesteps
               winddirection0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(2)
               winddirection1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(2)
               winddirection = cyclic_interpolation(winddirection0, winddirection1, a0, a1)
               ! === space conversion using nautical convention ===
               winddirection = (270.0_hp - winddirection)*degrad
               u => connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr
               v => connection%targetItemsPtr(2)%ptr%targetFieldPtr%arr1dPtr
               targetU = windspeed * cos(winddirection)
               targetV = windspeed * sin(winddirection)
               select case(connection%converterPtr%operandType)
                  case(operand_replace, operand_replace_if_value)
                     do i=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        u(i) = targetU
                        v(i) = targetV
                     end do
                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     connection%targetItemsPtr(2)%ptr%targetFieldPtr%timesteps = timesteps
                  case(operand_add)
                     do i=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        u(i) = u(i) + targetU
                        v(i) = v(i) + targetV
                     end do
                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     connection%targetItemsPtr(2)%ptr%targetFieldPtr%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUnimagdir: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterUnimagdir: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterUnimagdir

      ! =======================================================================

      function ecConverterVerticalMean(zpos,val,zmin,zmax,ndxmin,ndxmax) result (integral)
         real(hp)                           :: integral
         real(hp), dimension(:), intent(in) :: zpos, val
         real(hp)              , intent(in) :: zmin, zmax
         integer               , intent(out):: ndxmin, ndxmax

         real(hp) :: wt, minz, maxz, dz
         integer  :: ndx
         ndxmin = size(zpos)
         ndxmax = 1
         do ndx=1,size(zpos)
            if (zpos(ndx)>=zmin .and. zpos(ndx)<=zmax) then
               ndxmin=min(ndxmin,ndx)
               ndxmax=max(ndxmax,ndx)
            end if
         enddo
         integral = sum(abs(zpos(ndxmin+1:ndxmax)-zpos(ndxmin:ndxmax-1))  &
                        *(val(ndxmin+1:ndxmax)+ val(ndxmin:ndxmax-1))*0.5)
         dz = min(abs(zpos(ndxmin)-zmin),abs(zpos(ndxmin)-zmax))
         if (ndxmin>1) then
            wt = dz/abs((zpos(ndxmin-1)-zpos(ndxmin)))*0.5
            integral = integral + ((1_hp-wt)*val(ndxmin)+wt*val(ndxmin-1))*dz
         else
            integral = integral + val(ndxmin)*dz
         end if
         dz = min(abs(zpos(ndxmax)-zmin),abs(zpos(ndxmax)-zmax))
         if (ndxmax<size(zpos)) then
            wt = dz/abs((zpos(ndxmax-1)-zpos(ndxmax)))*0.5
            integral = integral + ((1_hp-wt)*val(ndxmax)+wt*val(ndxmax-1))*dz
         else
            wt = 0_hp
            integral = integral + val(ndxmax)*dz
         end if
         integral = integral/(zmax-zmin)
      end function ecConverterVerticalMean
      ! =======================================================================

      !> Execute the Converters in the Connection sequentially.
      !! meteo1 : polyint
      function ecConverterPolytim(connection, timesteps) result (success)
      use m_ec_elementset, only:ecElementSetGetAbsZ
      use m_ec_message
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer  :: i, k, j          !< loop counters
         real(hp) :: wL, wR           !< left and right weights
         integer  :: kL, kR           !<
         integer  :: maxlay_tgt       !< size of ElementSet of the TARGET in third dimension (if relevant), a.k.a kmx
         integer  :: maxlay_src       !< size of ElementSet of the SOURCE in third dimension (if relevant)
         integer  :: maxlay_srcL      !< number of layers at the LEFT interpolation support point
         integer  :: maxlay_srcR      !< number of layers at the RIGHT interpolation support point
         integer  :: kbegin, kend, kbeginL, kendL, kbeginR, kendR, idxL1, idxR1, idxL2, idxR2 !<
         integer  :: ndxmin, ndxmax
         logical, save :: alreadyPrinted = .false.
         real(hp) :: wwL, wwR  !<
         real(hp), dimension(:), allocatable :: valL, valR, valL1, valL2, valR1, valR2, val !<
         real(hp), dimension(:), allocatable :: sigmaL, sigmaR, sigma, sigmaLL, sigmaRR
         logical,  dimension(:), allocatable :: vmaskL, vmaskR
         real(hp), dimension(:),     pointer :: zmin => null() !< vertical min
         real(hp), dimension(:),     pointer :: zmax => null() !< vertical max
         real(hp) :: missing

         integer  :: idx              !< helper variable
         integer  :: vectormax
         integer  :: from, thru       !< contiguous range of indices in the target array
         character(maxMessageLen) :: errormsg

         !
        success = .false.
         select case(connection%converterPtr%interpolationType)
            case (interpolate_passthrough)
               select case(connection%converterPtr%operandType)
                  case(operand_replace_element)
                     idx = connection%converterPtr%targetIndex
                     ! Highly specific: 1 source Item with 1 value.
                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(idx) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(idx)
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported operand type requested.")
                     return
                  end select
            case (interpolate_spacetimeSaveWeightFactors, interpolate_spacetime)
               vectormax = connection%sourceItemsPtr(1)%ptr%quantityPtr%vectorMax
               missing = connection%sourceItemsPtr(1)%ptr%quantityPtr%fillvalue
               allocate(valL1(vectormax))
               allocate(valL2(vectormax))
               allocate(valR1(vectormax))
               allocate(valR2(vectormax))
               allocate(val(vectormax))

               ! Determine maximum number of layers on the target side
               if (associated(connection%targetItemsPtr(1)%ptr%elementSetPtr%z)) then
                  maxlay_tgt = size(connection%targetItemsPtr(1)%ptr%elementSetPtr%z) /   &
                               size(connection%targetItemsPtr(1)%ptr%elementSetPtr%x)
               else
                  maxlay_tgt = 1
               end if

               ! Determine maximum number of layers on the source side
               if (associated(connection%sourceItemsPtr(1)%ptr%elementSetPtr%z)) then
                  maxlay_src = size(connection%sourceItemsPtr(1)%ptr%elementSetPtr%z) /   &
                               size(connection%sourceItemsPtr(1)%ptr%elementSetPtr%x)
               else
                  maxlay_src = 1
               end if

               if (allocated(valL)) deallocate(valL)
               allocate(valL(vectormax*maxlay_src))
               if (allocated(valR)) deallocate(valR)
               allocate(valR(vectormax*maxlay_src))

               if (associated(connection%sourceItemsPtr(1)%ptr%elementSetPtr%z) .and. &     ! source has sigma
                         associated(connection%targetItemsPtr(1)%ptr%elementSetPtr%z)) then    ! target has sigma
                  allocate(sigma(maxlay_tgt*connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates))
                  sigma = connection%targetItemsPtr(1)%ptr%elementSetPtr%z
                  allocate(sigmaL(maxlay_src))
                  allocate(sigmaR(maxlay_src))
                  allocate(sigmaLL(maxlay_src))
                  allocate(sigmaRR(maxlay_src))
                  allocate(vmaskL(maxlay_src))
                  allocate(vmaskR(maxlay_src))
               end if

               ! zmax and zmin are absolute top and bottom at target point coordinates
               zmax => connection%targetItemsPtr(1)%ptr%elementSetPtr%zmax
               zmin => connection%targetItemsPtr(1)%ptr%elementSetPtr%zmin
               !
               ! The polytim FileReader's source Item is actually a target Item, containing the time-interpolated wind_magnitude from the subFileReaders.
               do i=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                  kL = connection%converterPtr%indexWeight%indices(1,i)
                  kR = connection%converterPtr%indexWeight%indices(2,i)
                  wL = connection%converterPtr%indexWeight%weightFactors(1,i)
                  wR = connection%converterPtr%indexWeight%weightFactors(2,i)
                  select case(connection%converterPtr%operandType)
                     case(operand_replace_element, operand_replace, operand_replace_if_value, operand_add)
                        ! Are the subproviders 3D or 2D?
                        if (associated(connection%sourceItemsPtr(1)%ptr%elementSetPtr%z) .and. &     ! source has a vertical coordinate
                               associated(connection%targetItemsPtr(1)%ptr%elementSetPtr%z)) then    ! target has a vertical coordinate
                           ! deal with one-sided interpolation
                           if ( kL == 0 .and. kR /= 0 ) then
                              kL = kR
                              wL = 0.0_hp
                           end if
                           if ( kR == 0 .and. kL /= 0 ) then
                              kR = kL
                              wR = 0.0_hp
                           end if
                           if (kL > 0) then
                              if (kR > 0) then
                                 kbegin  = maxlay_tgt*(i-1)+1                        ! refers to target column
                                 kend    = maxlay_tgt*i

                                 kbeginL = maxlay_src*(kL-1)+1                       ! refers to source left column
                                 kendL   = maxlay_src*kL
                                 sigmaL  = connection%sourceItemsPtr(1)%ptr%ElementSetPtr%z(kbeginL:kendL)

                                 kbeginR = maxlay_src*(kR-1)+1                       ! refers to source right column
                                 kendR   = maxlay_src*kR
                                 sigmaR  = connection%sourceItemsPtr(1)%ptr%ElementSetPtr%z(kbeginR:kendR)

                                 ! Convert Z-coordinate to absolute z wrt datum
                                 ! For the time being, let's assume that both support points have the same
                                 ! zmin and zmax as the support points. This way interpolation from sigma->sigma
                                 ! and z->z gives the same result.
                                 ! Convert target elementset
                                 if (.not.ecElementSetGetAbsZ (connection%targetItemsPtr(1)%ptr%ElementSetPtr,   &
                                                                                                  kbegin,kend,   &
                                                                                        zmin(i),zmax(i),sigma(kbegin:kend)))  return
                                 ! Convert source elementset, first point
                                 if (.not.ecElementSetGetAbsZ (connection%sourceItemsPtr(1)%ptr%ElementSetPtr,   &
                                                                                                  kbeginR,kendR, &
                                                                                       zmin(i),zmax(i),sigmaR))  return
                                 ! Convert source elementset, second point
                                 if (.not.ecElementSetGetAbsZ (connection%sourceItemsPtr(1)%ptr%ElementSetPtr,   &
                                                                                                  kbeginL,kendL, &
                                                                                       zmin(i),zmax(i),sigmaL))  return
                                 ! Prepare sigmaR and valR
                                 maxlay_srcR = 0
                                 sigmaRR = ec_undef_hp
                                 vmaskR = .False.
                                 valR = ec_undef_hp
                                 do k=1,maxlay_src
                                    from = vectormax*maxlay_src*(kR-1)+vectormax*(k-1)+1
                                    thru = vectormax*maxlay_src*(kR-1)+vectormax*(k)
                                    ! check if all vector components are unequal missing for this layer
                                    if (all(connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr(from:thru)/=missing) .and. (sigmaR(k)>0.5*ec_undef_hp)) then
                                       maxlay_srcR = maxlay_srcR + 1
                                       valR((maxlay_srcR-1)*vectormax+1:maxlay_srcR*vectormax) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr(from:thru)
                                       sigmaRR(maxlay_srcR) = sigmaR(k)
                                    end if
                                 end do
                                 if (maxlay_srcR<1) then
                                    write(errormsg,'(a,i0,a,i5.5)') "ERROR: ec_converter::ecConverterPolytim: No valid sigma (layer) associated with point ", &
                                                                      kR," of polytim item ", connection%sourceItemsPtr(1)%ptr%id
                                    call setECMessage(errormsg)
                                    return
                                 end if

                                 ! Prepare sigmaL and valL
                                 maxlay_srcL = 0
                                 sigmaLL = ec_undef_hp
                                 vmaskL = .False.
                                 valL = ec_undef_hp
                                 do k=1,maxlay_src
                                    from = vectormax*maxlay_src*(kL-1)+vectormax*(k-1)+1
                                    thru = vectormax*maxlay_src*(kL-1)+vectormax*(k)
                                    ! check if all vector components are unequal missing for this layer
                                    if (all(connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr(from:thru)/=missing) .and. (sigmaL(k)>0.5*ec_undef_hp)) then
                                       maxlay_srcL = maxlay_srcL + 1
                                       valL((maxlay_srcL-1)*vectormax+1:maxlay_srcL*vectormax) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr(from:thru)
                                       sigmaLL(maxlay_srcL) = sigmaL(k)
                                    end if
                                 end do
                                 if (maxlay_srcL<1) then
                                    write(errormsg,'(a,i0,a,i5.5)') "ERROR: ec_converter::ecConverterPolytim: No valid sigma (layer) associated with point ", &
                                                                      kL," of polytim item ", connection%sourceItemsPtr(1)%ptr%id
                                    call setECMessage(errormsg)
                                    return
                                 end if


                                 if (connection%sourceItemsPtr(1)%ptr%quantityPtr%zInterpolationType == zinterpolate_mean) then
                                    valL1 = ecConverterVerticalMean(sigmaLL,valL,zmin(i),zmax(i),ndxmin,ndxmax)
                                    if (ndxmax-ndxmin<1) then
                                       write(errormsg,'(a,i0,a,i5.5)') "ERROR: ec_converter::ecConverterPolytim: No valid layer for averaging for point ", &
                                                                         kL," of polytim item ", connection%sourceItemsPtr(1)%ptr%id
                                       call setECMessage(errormsg)
                                       return
                                    end if
                                    valR1 = ecConverterVerticalMean(sigmaRR,valR,zmin(i),zmax(i),ndxmin,ndxmax)
                                    if (ndxmax-ndxmin<1) then
                                       write(errormsg,'(a,i0,a,i5.5)') "ERROR: ec_converter::ecConverterPolytim: No valid layer for averaging for point ", &
                                                                         kR," of polytim item ", connection%sourceItemsPtr(1)%ptr%id
                                       call setECMessage(errormsg)
                                       return
                                    end if
                                    val = wL*valL1  + wR*valR1
                                    do k=kbegin,kend         ! Set the average value for all vertical positions
                                       if ((connection%converterPtr%operandType == operand_replace) .or. &
                                           (connection%converterPtr%operandType == operand_replace_element) .or. &
                                           (connection%converterPtr%operandType == operand_replace_if_value)) then
                                          connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax) = val(1:vectormax)
                                       else if (connection%converterPtr%operandType == operand_add) then
                                          connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax)   &
                                                = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax) + val(1:vectormax)
                                       end if
                                    end do         ! target layers
                                 else
                                    do k=kbegin,kend
                                       ! RL: BUG!!! z(k) not initialised if the target side is not 3D !!! TO BE FIXED !!!!!!!!!!!!!!
                                       if ( sigma(k) < 0.5*ec_undef_hp ) cycle

                                       ! find vertical indices and weights for the LEFT point
                                       call findVerticalIndexWeight(sigma(k), sigmaLL, maxlay_srcL, kL, wwL, idxL1, idxL2)
                                       ! find vertical indices and weights for the RIGHT point
                                       call findVerticalIndexWeight(sigma(k), sigmaRR, maxlay_srcR, kR, wwR, idxR1, idxR2)

                                       ! idx are in terms of vector for a specific pli-point and layer
                                       valL1(1:vectormax) = valL((idxL1-1)*vectormax+1:(idxL1)*vectormax)
                                       valL2(1:vectormax) = valL((idxL2-1)*vectormax+1:(idxL2)*vectormax)
                                       valR1(1:vectormax) = valR((idxR1-1)*vectormax+1:(idxR1)*vectormax)
                                       valR2(1:vectormax) = valR((idxR2-1)*vectormax+1:(idxR2)*vectormax)
                                       !
                                       select case(connection%sourceItemsPtr(1)%ptr%quantityPtr%zInterpolationType)
                                          case(zinterpolate_unknown)
                                             if ( .not. alreadyPrinted) then
                                                call setECMessage("WARNING: ec_converter::ecConverterPolytim: Unknown vertical interpolation type given, will proceed with linear method.")
                                                alreadyPrinted = .true.
                                             endif
                                             val = wL*(wwL*valL1 + (1.0_hp-wwL)*valL2) + wR*(wwR*valR1 + (1.0_hp-wwR)*valR2)
                                          case(zinterpolate_linear)
                                             val = wL*(wwL*valL1 + (1.0_hp-wwL)*valL2) + wR*(wwR*valR1 + (1.0_hp-wwR)*valR2)
                                          case(zinterpolate_block)
                                             val = wL*valL1  + wR*valR1
                                          case(zinterpolate_log)
                                             val = wL*(valL1**wwL)*(valL2**(1.0_hp-wwL)) + wR*(valR1**wwR)*(valR2**(1.0_hp-wwR))
                                          case default
                                             call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported vertical interpolation type requested.")
                                             return
                                          end select
                                       !
                                       if ((connection%converterPtr%operandType == operand_replace) .or. &
                                           (connection%converterPtr%operandType == operand_replace_element) .or. &
                                           (connection%converterPtr%operandType == operand_replace_if_value)) then
                                          connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax) = val(1:vectormax)
                                       else if (connection%converterPtr%operandType == operand_add) then
                                          connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax)   &
                                                = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax) + val(1:vectormax)
                                       end if
                                       !
                                    end do         ! target layers
                                 end if            ! are we averaging the source in the vertical direction ?
                              end if               ! kR > 0: right support point exists
                           end if                  ! kL > 0: left support point exists
                        else                       ! no vertical coordinate assigned to this source item, i.e. 3D source
                           ! 2D subproviders
                           connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps   !!!!! ???????
                           ! Determine value
                           if (kL > 0) then
                              if (kR > 0) then
                                 val(1:vectormax) = wL*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kL-1)*vectormax+1:kL*vectormax) &
                                                  + wR*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kR-1)*vectormax+1:kR*vectormax)
                              else ! Just left point
                                 val(1:vectormax) = wL*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kL-1)*vectormax+1:kL*vectormax)
                              end if
                           else if (kR > 0) then ! Just right point
                                 val(1:vectormax) = wR*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kR-1)*vectormax+1:kR*vectormax)
                           end if
                              !
                           if( kL /= 0 .or. kR /= 0 ) then
                              ! Write value
                              do k = 1,maxlay_tgt
                                 from = (i-1)*maxlay_tgt*vectormax+(k-1)*vectormax+1
                                 thru = (i-1)*maxlay_tgt*vectormax+k*vectormax
                                 if ((connection%converterPtr%operandType == operand_replace) .or. &
                                     (connection%converterPtr%operandType == operand_replace_element) .or. &
                                     (connection%converterPtr%operandType == operand_replace_if_value)) then
                                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(from:thru) = val(1:vectormax)

                                 else if (connection%converterPtr%operandType == operand_add) then
                                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(from:thru) = &
                                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(from:thru) + val(1:vectormax)
                                 end if
                              enddo
                           end if            ! valid left or right point ?
                        end if               ! vertical coordinate for this source item, i.e. is it a 3D source  ?
                    case default
                       call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported operand type requested.")
                       return
                  end select
               end do

            case default
               call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported interpolation type requested.")
               return
         end select
         if (allocated(sigma)) deallocate(sigma)
         if (allocated(sigmaL)) deallocate(sigmaL)
         if (allocated(sigmaR)) deallocate(sigmaR)
         success = .true.
      end function ecConverterPolytim

      subroutine findVerticalIndexWeight(sigmak, sigma, maxlay_src, kLR, ww, idx1, idx2)
         real(kind=hp), intent(in) :: sigmak, sigma(:)
         integer, intent(in) :: maxlay_src, kLR
         real(kind=hp), intent(out) :: ww
         integer, intent(out) :: idx1, idx2

         integer :: kkl

         if ((sigmak - sigma(1)) * (sigmak - sigma(maxlay_src)) >= 0) then      ! beyond the range of source levels
            if (abs(sigmak - sigma(1)) < abs(sigmak - sigma(maxlay_src))) then  ! closer to sigma(1) (avoiding the assumption sigma(1) is the lowest)
               idx1 = 1
               idx2 = 1
               ww = 0.5d0
            else                                                                ! closer to sigma(maxlay_src)
               idx1 = maxlay_src
               idx2 = maxlay_src
               ww = 0.5d0
            endif
         else                                                                   ! within the range of source levels
            do idx1 = 1, maxlay_src-1                                           ! find vertical indices
               if ((sigmak - sigma(idx1)) * (sigmak - sigma(idx1+1)) <= 0) then ! between level idx1 and idx+1
                  idx2 = idx1 + 1
                  ww = (sigma(idx2)-sigmak) / (sigma(idx2)-sigma(idx1))
                  exit
               endif
            enddo
         endif
      end subroutine findVerticalIndexWeight
      ! =======================================================================

      !> Perform the configured conversion, if supported, for a curvi FileReader.
      !! Supports linear interpolation in time and interpolation is space.
      !! Supports overwriting and adding-to the entire target Field array.
      !! Converts data from source Item i to target Item i.
      !! unstruc : gettimespacevalue
      function ecConverterCurvi(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         type(tEcField),       pointer :: sourceT0Field    !< helper pointer
         type(tEcField),       pointer :: sourceT1Field    !< helper pointer
         real(hp)                      :: sourceMissing !< Source side missing value
         type(tEcField),       pointer :: targetField      !< helper pointer
         type(tEcIndexWeight), pointer :: indexWeight      !< helper pointer, saved index weights
         type(tEcElementSet),  pointer :: sourceElementSet !< source ElementSet
         real(hp), dimension(:), pointer   :: targetValues
         integer                       :: ii, jj
         integer                       :: nmiss
         real(hp), dimension(:,:),   pointer :: s2D_T0, s2D_T1   !< 2D representation of linearly indexed array arr1D
         integer :: n_cols, n_rows, n_points
         integer :: mp, np
         integer :: i, j
         real(hp) :: a0, a1
         real(hp) :: t0, t1
         real(hp), dimension(4) :: wf_i !< helper containing indexWeight%weightFactors(1:4,i)
         !
         success = .false.
         sourceT0Field    => null()
         sourceT1Field    => null()
         targetField      => null()
         indexWeight      => null()
         sourceElementSet => null()
         !
         if (connection%nSourceItems /= connection%nTargetItems) then
            call setECMessage("ERROR: ec_converter::ecConverterCurvi: The number of source and target Items differ and should have been identical.")
            return
         end if
         !
         select case(connection%converterPtr%interpolationType)
         case (interpolate_spacetimeSaveWeightFactors)
            indexWeight => connection%converterPtr%indexWeight
            sourceElementSet => connection%sourceItemsPtr(1)%ptr%elementSetPtr
            n_points = connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
            n_cols = sourceElementSet%n_cols
            n_rows = sourceElementSet%n_rows
            !
            sourceT0Field => connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr
            sourceT1Field => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr
            t0 = sourceT0Field%timesteps
            t1 = sourceT1Field%timesteps
            call time_weight_factors(a0, a1, timesteps, t0, t1)
            !
            ! TODO: Take care of the allocation of targetValues(n_point)
            do j=1, connection%nSourceItems
               sourceT0Field => connection%sourceItemsPtr(j)%ptr%sourceT0FieldPtr
               sourceT1Field => connection%sourceItemsPtr(j)%ptr%sourceT1FieldPtr
               sourceMissing =  connection%sourceItemsPtr(j)%ptr%quantityPtr%fillvalue
               targetField   => connection%targetItemsPtr(j)%ptr%targetFieldPtr
               targetValues  => targetField%arr1dPtr
               !
               s2D_T0(1:n_cols,1:n_rows) => sourceT0Field%arr1d
               s2D_T1(1:n_cols,1:n_rows) => sourceT1Field%arr1d
               do i=1, n_points
                  np = indexWeight%indices(1,i)
                  mp = indexWeight%indices(2,i)
                  if (mp > 0 .and. np > 0) then
                     nmiss = 0
                     do jj=0,1
                        do ii=0,1
                           if ( comparereal(s2D_T0(mp+ii, np+jj), sourceT0Field%missingValue, .true.)==0 .or.   &
                                comparereal(s2D_T1(mp+ii, np+jj), sourceT1Field%missingValue, .true.)==0 ) then
                                nmiss = nmiss + 1
                           end if
                        end do
                     end do

                     if (nmiss == 0) then     ! if sufficient data for bi-linear interpolation
                        if ((connection%converterPtr%operandType == operand_replace) .or. &
                            (connection%converterPtr%operandType == operand_replace_if_value)) then
                           targetValues(i) = 0.0_hp
                        end if
                        wf_i = indexWeight%weightFactors(1:4,i)
                        targetValues(i) = targetValues(i)                                 &
                            + a0 * (wf_i(1)*s2D_T0(mp  ,np)    + &
                                    wf_i(2)*s2D_T0(mp+1,np)    + &
                                    wf_i(3)*s2D_T0(mp+1,np+1)  + &
                                    wf_i(4)*s2D_T0(mp  ,np+1))   &
                            + a1 * (wf_i(1)*s2D_T1(mp  ,np)    + &
                                    wf_i(2)*s2D_T1(mp+1,np)    + &
                                    wf_i(3)*s2D_T1(mp+1,np+1)  + &
                                    wf_i(4)*s2D_T1(mp  ,np+1))
                     end if
                  end if
               end do
               targetField%timesteps = timesteps
               !
            end do
         case default
            call setECMessage("ERROR: ec_converter::ecConverterCurvi: Unsupported interpolation type requested.")
            return
         end select
         success = .true.
      end function ecConverterCurvi


      ! =======================================================================
      !> Perform the configured conversion, if supported, for a arcinfo FileReader.
      !! Supports linear interpolation in time, interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array.
      !! meteo1 : gettimespacevalue
      function ecConverterArcinfo(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp) :: x01, y01, dx1, dy1 !< uniform grid parameters
         integer  :: mx                 !< n_cols (x or latitude coordinate)
         integer  :: grid_width                 !< n_rows (y or longitude coordinate)
         integer  :: n                  !< loop counter
         real(hp) :: x1, y1             !<
         integer  :: i1, j1             !<
         real(hp) :: di1, dj1           !<
         real(hp), dimension(4) :: f    !< spatial weights
         real(hp), dimension(4) :: fmask!< spatial mask
         real(hp) :: fsum               !< summed spatial weights*masks
         real(hp), dimension(4) :: u    !< source u, v or p at timesteps=t0
         real(hp), dimension(4) :: v    !< source u, v or p at timesteps=t1
         real(hp) :: a0, a1             !<
         real(hp) :: t0, t1             !<
         real(hp) :: vv0                !< target u, v or p at timesteps=t0
         real(hp) :: vv1                !< target u, v or p at timesteps=t1
         real(hp) :: rr                 !< target u, v or p at timesteps=t
         type(tEcField), pointer :: sourceT0Field !< helper pointer
         type(tEcField), pointer :: sourceT1Field !< helper pointer
         type(tEcMask), pointer :: srcmask
         !
         success = .false.
         sourceT0Field => null()
         sourceT1Field => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_timespace, interpolate_spacetime, interpolate_time_extrapolation_ok)
               ! TODO: cleanup this FM-compliancy code segment.
               if (connection%sourceItemsPtr(1)%ptr%elementSetPtr%ofType == elmSetType_cartesian) then
                  ! Only legal for uniform grids...
                  x01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(1)
                  y01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(1)
                  dx1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(2)-connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(1)
                  dy1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(2)-connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(1)
               else
                  x01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x0
                  y01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y0
                  dx1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%dx
                  dy1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%dy
               end if
               mx = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_cols
               grid_width = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_rows
               sourceT0Field => connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr
               sourceT1Field => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr
               !
               do n=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                  ! TODO : Calculate target value depending on ElementSet mask.
                  ! === interpolate in space ===
                  x1 = (connection%targetItemsPtr(1)%ptr%elementSetPtr%x(n) - x01)/dx1
                  if (x1 < -0.5_hp .or. x1 > mx - 0.5_hp) cycle
                  !
                  y1 = (connection%targetItemsPtr(1)%ptr%elementSetPtr%y(n) - y01)/dy1
                  if (y1 < -0.5_hp .or. y1 > grid_width - 0.5_hp) cycle
                  !
                  i1  = int(x1 + 1)
                  i1  = min(mx - 1,max(1, i1))
                  di1 = x1 + 1 - i1
                  !
                  j1  = int(y1 + 1)
                  j1  = min(grid_width - 1,max(1, j1))
                  dj1 = y1 + 1 - j1
                  ! spatial weight factors
                  f(1) = (1-di1)*(1-dj1)
                  f(2) = (  di1)*(1-dj1)
                  f(3) = (  di1)*(  dj1)
                  f(4) = (1-di1)*(  dj1)
                  !
                  u(1) = sourceT0Field%arr1dPtr(i1+mx*(j1-1))
                  u(2) = sourceT0Field%arr1dPtr(i1+1+mx*(j1-1))
                  u(3) = sourceT0Field%arr1dPtr(i1+1+mx*j1)
                  u(4) = sourceT0Field%arr1dPtr(i1+mx*j1)
                  v(1) = sourceT1Field%arr1dPtr(i1+mx*(j1-1))
                  v(2) = sourceT1Field%arr1dPtr(i1+1+mx*(j1-1))
                  v(3) = sourceT1Field%arr1dPtr(i1+1+mx*j1)
                  v(4) = sourceT1Field%arr1dPtr(i1+mx*j1)

                  ! === deal with missing values ===
                  srcmask => connection%converterPtr%srcmask
                  fmask = 1

                  ! For now, the mask consists of ones and zeroes
                  if (allocated(srcmask%msk)) then
                     fmask(1) = (srcmask%msk((j1   -srcmask%nmin)*srcmask%mrange+i1   -srcmask%mmin+1))
                     fmask(2) = (srcmask%msk((j1   -srcmask%nmin)*srcmask%mrange+i1+1 -srcmask%mmin+1))
                     fmask(3) = (srcmask%msk((j1+1 -srcmask%nmin)*srcmask%mrange+i1+1 -srcmask%mmin+1))
                     fmask(4) = (srcmask%msk((j1+1 -srcmask%nmin)*srcmask%mrange+i1   -srcmask%mmin+1))
                  endif

                  fsum = sum(f*(1.d0-fmask))
                  if (fsum>=1.0e-03) then
                     f = (f*(1.d0-fmask))/fsum
                  endif

                  vv0  = u(1)*f(1) + u(2)*f(2) + u(3)*f(3) + u(4)*f(4)
                  vv1  = v(1)*f(1) + v(2)*f(2) + v(3)*f(3) + v(4)*f(4)
                  ! === linear interpolation in time ===
                  t0 = sourceT0Field%timesteps
                  t1 = sourceT1Field%timesteps
                  call time_weight_factors(a0, a1, timesteps, t0, t1)
                  rr = a0*vv0 + a1*vv1
                  select case(connection%converterPtr%operandType)
                     case(operand_replace, operand_replace_if_value)
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = rr
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     case(operand_add)
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) + rr
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     case default
                        call setECMessage("ERROR: ec_converter::ecConverterArcinfo: Unsupported operand type requested.")
                        return
                  end select
               end do
            case default
               call setECMessage("ERROR: ec_converter::ecConverterArcinfo: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterArcinfo

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a samples FileReader.
      !! Supports linear triangle interpolation in space, no time, no weights.
      !! meteo1 : timespaceinitialfield
      function ecConverterSamples(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp) :: x01, y01           !< uniform grid parameters
         real(hp) :: rr                 !< target u, v or p at timesteps=t
         type(tEcField), pointer :: sourceT0Field !< helper pointer
         type(tEcField), pointer :: sourceT1Field !< helper pointer
         integer :: nSamples
         !
         success = .false.
         sourceT0Field => null()
         sourceT1Field => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_triangle)
               x01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(1)
               y01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(1)

               nSamples = connection%sourceItemsPtr(1)%ptr%elementSetPtr%nCoordinates
               sourceT1Field => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr

               call setECMessage('ERROR: ec_converter::ecConverterSamples: triangle interpolation is work in progress.')
               return
               rr = 0d0 ! TODO: AvD: WIP
                  !select case(connection%converterPtr%operandType)
                  !   case(operand_replace)
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = rr
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                  !   case(operand_add)
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) + rr
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                  !   case default
                  !      call setECMessage("ERROR: ec_converter::ecConverterSamples: Unsupported operand type requested.")
                  !      return
                  !end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterSamples: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterSamples

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a qhtable FileReader.
      !! No interpolation is supported. Data is constant over time.
      !! Supports overwriting an array element of the target Field's data array.
      function ecConverterQhtable(connection) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            pointer       :: input      !< input value to the lookup table (referenced by pointer
         !
         integer :: j
         integer :: start_j
         integer :: grid_width, tgtndx
         !
         success = .false.
         !
         select case(connection%converterPtr%interpolationType)
            case (interpolate_passthrough)
               select case(connection%converterPtr%operandType)
                  case(operand_replace_element)
                     tgtndx = connection%converterPtr%targetIndex
                     grid_width = connection%sourceItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                     input => connection%converterPtr%inputptr
                     if (input < connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)) then
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(tgtndx) = connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(1) ! waterlevel(i)
                     else if (input > connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(grid_width)) then
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(tgtndx) = connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(grid_width) ! waterlevel(grid_width)
                     else
                        do j=2, grid_width
                           if (input < connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(j)) then ! discharge(j)
                              start_j = j
                              exit
                           end if
                        end do
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(tgtndx) = connection%sourceItemsPtr(3)%ptr%sourceT0FieldPtr%arr1dPtr(start_j-1) * input &
                                                                                         + connection%sourceItemsPtr(4)%ptr%sourceT0FieldPtr%arr1dPtr(start_j-1)
                     endif
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterQhtable: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterQhtable: Unsupported interpolation type requested.")
               return
         end select
         success = .true.

      end function ecConverterQhtable
      ! =======================================================================

      !> Perform the configured conversion, if supported, for a unimagdir FileReader.
      !! No interpolation is supported. Data is generated from seed values.
      !! Supports overwriting and adding-to the target Field's single data value, as well all as overwriting only one array element.
      !! Converts angular velocity, phase and magnitude into an amplitude.
      !! meteo1 : readfouriercompstim
      function ecConverterFourier(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         type(c_time),        intent(in)    :: timesteps  !< time in mjd
         !
         integer                                          :: i, j      !< loop counters
         character(len=maxNameLen)                        :: str       !< helper variable
         logical                                          :: is_astro  !< flag for astronomical signal
         real(hp)                                         :: refdate   !< reference date for this source in mjd
         real(hp)                                         :: tseconds  !< time in seconds
         real(hp)                                         :: tnodal    !< start time (seconds after reftime)
         real(hp)                                         :: omega     !< angular velocity [rad/minute]
         real(hp)                                         :: phase0    !< phase at t=t0 [rad]
         real(hp)                                         :: magnitude !< magnitude [m]
         real(hp)                                         :: phase     !< phase at t=timesteps [rad]
         real(hp)                                         :: deflection !< summed result of the Fourier component effects
         !
         success = .false.
         deflection = 0.0_hp
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
         case (interpolate_passthrough)
            ! Source data at timesteps t is generated from the seed data in sourceT0Field.
            ! === FileReader contains periods. ===
            is_astro = .false.
            refdate = connection%sourceItemsPtr(1)%ptr%tframe%ec_refdate
            do j=1, connection%sourceItemsPtr(1)%ptr%elementSetPtr%nCoordinates
               do i=1, connection%nSourceItems
                  str = connection%sourceItemsPtr(i)%ptr%quantityPtr%name
                  if (trim(str) == 'period') then
                     omega = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                     is_astro = allocated(connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%astro_components)
                     tnodal = (connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%timesteps - refdate)
                  else if (trim(str) == 'phase') then
                     phase0 = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                  else if (trim(str) == 'magnitude') then
                     magnitude = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                  end if
               end do
               if(is_astro) then
                  tseconds = timesteps%seconds() - tnodal * 86400.0_hp
               else
                  tseconds = timesteps%seconds()
               endif
               phase = omega * (tseconds/60.0_hp) - phase0                      ! omega, angle velocity [rad/minute]
               deflection = deflection + magnitude * cos(phase)
            end do
            select case(connection%converterPtr%operandType)
               case(operand_replace, operand_replace_if_value)
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1) = deflection
               case(operand_replace_element)
                  ! Only used by poly_tim.
                  if (connection%converterPtr%targetIndex == ec_undef_int) then
                     call setECMessage("ERROR: ec_converter::ecConverterFourier: Converter's target Field array index not set.")
                     return
                  end if
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(connection%converterPtr%targetIndex) = deflection
               case(operand_add)
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1) + deflection
               case default
                  call setECMessage("ERROR: ec_converter::ecConverterFourier: Unsupported operand type requested.")
                  return
            end select
         case default
            call setECMessage("ERROR: ec_converter::ecConverterFourier: Unsupported interpolation type requested.")
            return
         end select
         success = .true.
      end function ecConverterFourier

      ! =======================================================================

      !> Cyclic interpolation of two scalars, based on periodicity of 360 (degrees)
      !! Sort data in monotonically increasing order and rotate over smallest angle
      function cyclic_interpolation(var1, var2, weight1, weight2)
         real(hp), intent(in) :: var1                !< First input argument for in interpolation functions using a scalar weight value
         real(hp), intent(in) :: var2                !< Second input argument for in interpolation functions using a scalar weight value
         real(hp), intent(in) :: weight1             !< Value for weighing two variables: 'weight1' holds for var1
         real(hp), intent(in) :: weight2             !< Value for weighing two variables: 'weight2' holds for var2
         real(hp)             :: cyclic_interpolation   !< Result value after linear interpolation between var1 and var2 using weightvalue weight
         !
         real(hp)             :: minangle
         real(hp)             :: maxangle
         real(hp)             :: delta
         real(hp)             :: weightfac

         minangle  = var1
         maxangle  = var2
         weightfac = weight1
         if (var2 < var1) then
            minangle  = var2
            maxangle  = var1
            weightfac = 1.0_hp - weightfac
         end if
         delta = maxangle - minangle

         ! Carry out the interpolation
         if (delta <= 180.0_hp) then
            cyclic_interpolation = (1.0_hp - weightfac)*delta
         else
            cyclic_interpolation = (1.0_hp - weightfac)*delta + weightfac*360.0_hp
         end if

         ! Rotate backwards over the smallest angle
         cyclic_interpolation = cyclic_interpolation + minangle
         cyclic_interpolation = modulo(cyclic_interpolation, 360.0_hp)
      end function

      ! =======================================================================

      !> Execute the Converters in the Connection sequentially.
      !! meteo1: gettimespacevalue
      function ecConverterSpiderweb(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer  :: i
         real(hp) :: spwdphi !< angular increment: 2pi/#colums
         real(hp) :: spwdrad !< radial increment: radius/#rows
         real(hp) :: a0, a1
         real(hp) :: t0, t1
         real(hp) :: xeye0
         real(hp) :: yeye0
         real(hp) :: xeye1
         real(hp) :: yeye1
         real(hp) :: xeye
         real(hp) :: yeye
         real(hp) :: dlat
         real(hp) :: dlon
         real(hp) :: xc, yc
         real(hp) :: spwradhat
         real(hp) :: spwphihat
         real(hp) :: uintp
         real(hp) :: vintp
         real(hp) :: pintp
         real(hp) :: spwr1
         real(hp) :: spwd1
         real(hp) :: spwp1
         real(hp) :: spwr2
         real(hp) :: spwd2
         real(hp) :: spwp2
         real(hp) :: spwr3
         real(hp) :: spwd3
         real(hp) :: spwp3
         real(hp) :: spwr4
         real(hp) :: spwd4
         real(hp) :: spwp4
         real(hp) :: wphi
         real(hp) :: wrad
         real(hp) :: spwrA
         real(hp) :: spwrB
         real(hp) :: spwdA
         real(hp) :: spwdB
         real(hp) :: spwpA
         real(hp) :: spwpB
         real(hp) :: rintp
         real(hp) :: dintp
         real(hp) :: h1, h2
         real(hp) :: fa, fi
         real(hp) :: earthrad
         real(hp) :: rcycl, yy, spwf, spw_merge_frac          !< temporary variables used for blending spiderwebdata with the background
         real(hp) :: tmp !< helper temporary
         integer :: n_rows, n_cols
         integer :: n !< loop counter
         integer :: mf, nf
         integer :: twx, twy, twp                             !< numbering of target items
         integer :: swr, swd, swp                             !< numbering of source items

         !
         success = .false.
         fa = pi / 180.0_hp
         fi = 180.0_hp / pi
         earthrad = 6378137.0_hp
         n_rows = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_rows
         n_cols = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_cols
         !
         ! Which quantities should be updated and in what target items ?
         twx = 0
         twy = 0
         twp = 0
         swr = 0
         swd = 0
         swp = 0
         select case(connection%targetItemsPtr(1)%ptr%quantityPtr%name)
            case ('airpressure_windx_windy')
               twx = 1
               twy = 2
               twp = 3
            case ('airpressure','atmosphericpressure')
               twp = 1
            case ('windx')
               twx = 1
            case ('windy')
               twy = 1
            case ('windxy')
               twx = 1
               twy = 2
            case default
               call setECMessage("ERROR: ec_converter::ecConverterSpiderweb: '"     &
                  // trim(connection%targetItemsPtr(1)%ptr%quantityPtr%name)         &
                  // "' is not a known spiderweb quantity.")
               return
            end select
            do i = 1, connection%nSourceItems
              select case (connection%sourceItemsPtr(i)%ptr%quantityPtr%name)
                 case ('windspeed')
                    swr = i
                 case ('winddirection')
                    swd = i
                 case ('p_drop')
                    swp = i
              end select
            end do

         !
         ! Calculate the basic spiderweb grid settings
         spwdphi = 360.0_hp / (n_cols - 1) ! 0 == 360 degrees, so -1
         spwdrad = connection%sourceItemsPtr(1)%ptr%elementSetPtr%radius / (n_rows - 1)
         spw_merge_frac = connection%sourceItemsPtr(1)%ptr%elementSetPtr%spw_merge_frac
         !
         ! Determine the (x,y) coordinate pair of the cyclone eye at t=timesteps.
         t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
         t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
         call time_weight_factors(a0, a1, timesteps, t0, t1)
         xeye0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%x_spw_eye
         yeye0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%y_spw_eye
         xeye1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%x_spw_eye
         yeye1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%y_spw_eye
         xeye = cyclic_interpolation(xeye0, xeye1, a0, a1) ! cyclic interpolation (spheric coord)
         yeye = cyclic_interpolation(yeye0, yeye1, a0, a1) ! cyclic inteprolation (spheric coord)
         !
         do n=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
            xc = connection%targetItemsPtr(1)%ptr%elementSetPtr%x(n)
            yc = connection%targetItemsPtr(1)%ptr%elementSetPtr%y(n)
            dlat = modulo(yc, 360.0_hp) - yeye
            dlon = modulo(xc, 360.0_hp) - xeye
            h1 = (sin(dlat/2.0_hp*fa))**2 + cos(yeye*fa)*cos(yc*fa)*(sin(dlon/2.0_hp*fa))**2
            h2 = 2.0_hp*atan(sqrt(h1)/sqrt(1.0_hp-h1))
            spwradhat = earthrad*h2
            spwphihat = cos(yeye*fa)*sin(yc*fa) - sin(yeye*fa)*cos(yc*fa)*cos(dlon*fa)
            if (.not. comparereal(spwphihat, 0d0) == 0) then
               spwphihat = atan( sin(dlon*fa)*cos(yc*fa) / (spwphihat) )*fi
            else
               spwphihat = 0d0
            end if
            if (comparereal(dlon, 0d0) == 0) then     ! exceptional case of being excatly SOUTH of the eye, phi should be 180 degrees
               if (dlat<0) then
                  spwphihat = 180.0d0
               endif
            endif
            if (dlon*spwphihat<0) then                ! relative longitude should have the same sign as phi
               spwphihat = spwphihat + 180.0_hp
            endif
            spwphihat = modulo(spwphihat, 360.0_hp)
            ! Find the four nearest points in the spiderweb
            mf = floor(spwphihat/spwdphi) + 1 ! find orientation in windrose
            nf = floor(spwradhat/spwdrad) + 1 ! find orientation on radius
            ! If outside spiderweb or exactly in eye, then set windspeed to zero, else find weightfactors and interpolate
            if (nf >= connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_rows .or. (dlat == 0.0_hp .and. dlon == 0.0_hp)) then
               uintp = 0.0_hp
               vintp = 0.0_hp
               pintp = 0.0_hp
            else
               ! Get data from stencil (mf (+1), nf (+1))
               if ((twx>0).or.(twy>0)) then
                  spwr1 = connection%sourceItemsPtr(swr)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a0 + &
                          connection%sourceItemsPtr(swr)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a1 ! linear time interp of magnitude
                  spwr2 = connection%sourceItemsPtr(swr)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a0 + &
                          connection%sourceItemsPtr(swr)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a1 ! linear time interp of magnitude
                  spwr3 = connection%sourceItemsPtr(swr)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*nf)*a0 + &
                          connection%sourceItemsPtr(swr)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*nf)*a1 ! linear time interp of magnitude
                  spwr4 = connection%sourceItemsPtr(swr)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a0 + &
                          connection%sourceItemsPtr(swr)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a1 ! linear time interp of magnitude

                  spwd1 = cyclic_interpolation(connection%sourceItemsPtr(swd)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*(nf-1)), &
                          connection%sourceItemsPtr(swd)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*(nf-1)), a0, a1) ! cyclic time interp of direction
                  spwd2 = cyclic_interpolation(connection%sourceItemsPtr(swd)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1)), &
                          connection%sourceItemsPtr(swd)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1)), a0, a1) ! cyclic time interp of direction
                  spwd3 =  cyclic_interpolation(connection%sourceItemsPtr(swd)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*nf), &
                          connection%sourceItemsPtr(swd)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*nf), a0, a1) ! cyclic time interp of direction
                  spwd4 = cyclic_interpolation(connection%sourceItemsPtr(swd)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*nf), &
                          connection%sourceItemsPtr(swd)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*nf), a0, a1) ! cyclic time interp of direction
                  ! Safety at center
                  if (nf == 1) then
                     spwr1 = 0.0_hp
                     spwd1 = spwd3
                  endif
               end if
               if (twp>0) then
                  spwp1 = connection%sourceItemsPtr(swp)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a0 + &
                          connection%sourceItemsPtr(swp)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a1 ! linear time interp of pressure
                  spwp2 = connection%sourceItemsPtr(swp)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a0 + &
                          connection%sourceItemsPtr(swp)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a1 ! linear time interp of pressure
                  spwp3 = connection%sourceItemsPtr(swp)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*nf)*a0 + &
                          connection%sourceItemsPtr(swp)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*nf)*a1 ! linear time interp of pressure
                  spwp4 = connection%sourceItemsPtr(swp)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a0 + &
                          connection%sourceItemsPtr(swp)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a1 ! linear time interp of pressure
               end if
               !
               wphi  = 1.0_hp - (spwphihat - (mf-1)*spwdphi)/(spwdphi)   ! weightfactor for the direction
               wrad  = 1.0_hp - (spwradhat - (nf-1)*spwdrad)/(spwdrad)   ! weightfactor for the radius

               if ((twx>0).or.(twy>0)) then
                  spwrA = spwr1*wphi + spwr2*(1.0_hp-wphi)                  ! space interp magnitude (direction)
                  spwrB = spwr3*wphi + spwr4*(1.0_hp-wphi)                  ! space interp magnitude (direction)
                  tmp = 1.0_hp-wphi
                  spwdA = cyclic_interpolation(spwd1, spwd2, wphi, tmp) ! space interp direction (direction)
                  spwdB = cyclic_interpolation(spwd3, spwd4, wphi, tmp) ! space interp direction (direction)
                  rintp = spwrA*wrad + spwrB*(1.0_hp-wrad)              ! space interp (radius)
                  tmp = 1.0_hp-wrad
                  dintp = cyclic_interpolation(spwdA, spwdB, wrad, tmp) ! space interp (radius)
                  dintp =  90.0_hp - dintp         ! revert from nautical conventions
                  dintp =  modulo(dintp, 360.0_hp) ! for debug purposes
                  uintp = -rintp*cos(dintp*fa)     ! minus sign: wind from N points to S
                  vintp = -rintp*sin(dintp*fa)     ! minus sign: wind from N points to S
               end if

               if (twp>0) then
                  spwpA = spwp1*wphi + spwp2*(1.0_hp-wphi)                  ! space interp pressure (direction)
                  spwpB = spwp3*wphi + spwp4*(1.0_hp-wphi)                  ! space interp pressure (direction)
                  pintp = spwpA*wrad + spwpB*(1.0_hp-wrad)                  ! space interp (radius)
               end if

            endif
            select case(connection%converterPtr%operandType)
               case(operand_replace, operand_replace_if_value)
                  if (twx>0) then
                     connection%targetItemsPtr(twx)%ptr%targetFieldPtr%arr1dPtr(n) = uintp
                     connection%targetItemsPtr(twx)%ptr%targetFieldPtr%timesteps = timesteps
                  end if
                  if (twy>0) then
                     connection%targetItemsPtr(twy)%ptr%targetFieldPtr%arr1dPtr(n) = vintp
                     connection%targetItemsPtr(twy)%ptr%targetFieldPtr%timesteps = timesteps
                  end if
                  if (twp>0) then
                     ! Only pressure is considered relative here, so we don't overwrite the background pressure
                     connection%targetItemsPtr(twp)%ptr%targetFieldPtr%arr1dPtr(n) =                          &
                                   connection%targetItemsPtr(twp)%ptr%targetFieldPtr%arr1dPtr(n) - pintp
                     connection%targetItemsPtr(twp)%ptr%targetFieldPtr%timesteps = timesteps
                  end if
               case(operand_add)
                  rcycl = connection%sourceItemsPtr(1)%ptr%elementSetPtr%radius
                  yy = spwradhat
                  spwf = 0.d0
                  if (yy<rcycl) then
                     spwf   = min((1.0_hp - yy/rcycl)/spw_merge_frac,1.0_hp)
                     ! spwf is the weightfactor for the spiderweb! Differs from the Delft3D implementation
                     if (twx>0) then
                        connection%targetItemsPtr(twx)%ptr%targetFieldPtr%arr1dPtr(n) =                       &
                              (1.0_hp-spwf) * connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n)     &
                            +         spwf  * uintp
                        connection%targetItemsPtr(twx)%ptr%targetFieldPtr%timesteps = timesteps
                     end if
                     if (twy>0) then
                        connection%targetItemsPtr(twy)%ptr%targetFieldPtr%arr1dPtr(n) =                       &
                              (1.0_hp-spwf) * connection%targetItemsPtr(2)%ptr%targetFieldPtr%arr1dPtr(n)     &
                            +         spwf  * vintp
                        connection%targetItemsPtr(twy)%ptr%targetFieldPtr%timesteps = timesteps
                     end if
                     if (twp>0) then
                        connection%targetItemsPtr(twp)%ptr%targetFieldPtr%arr1dPtr(n) =                       &
                              connection%targetItemsPtr(twp)%ptr%targetFieldPtr%arr1dPtr(n) - spwf * pintp
                        connection%targetItemsPtr(twp)%ptr%targetFieldPtr%timesteps = timesteps
                     end if
                  end if
               case default
                  call setECMessage("ERROR: ec_converter::ecConverterSpiderweb: Unsupported operand type requested.")
                  return
            end select
         end do
         success = .true.
      end function ecConverterSpiderweb

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a NetCDF FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting .
      !! Converts source(i) to target(i).
      function ecConverterNetcdf(connection, timesteps) result (success)
      use m_alloc
      use kdtree2Factory
      implicit none
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer                 :: i,j,k,ipt       !< loop counters
         integer                 :: kbot, ktop
         logical                 :: extrapolated  !< .true.: timesteps is outside [t0,t1]
         real(hp)                :: t0            !< source item t0
         real(hp)                :: t1            !< source item t1
         real(hp)                :: a0            !< weight for source t0 data
         real(hp)                :: a1            !< weight for source t1 data
         real(hp)                :: wb            !< weight for source data below
         real(hp)                :: wt            !< weight for source data above
         real(hp)                :: a_s, b_s      !< coefficients for a linear transformation of source vertical coordinates to elevation above datum
         real(hp)                :: a_t, b_t      !< coefficients for a linear transformation of target vertical coordinates to elevation above datum
         real(hp)                :: sourceValueT0 !< source value at t0
         real(hp)                :: sourceValueT1 !< source value at t1
         type(tEcField), pointer :: targetField   !< Converter's result goes in here
         real(hp)                :: targetMissing !< Target side missing value
         type(tEcField), pointer :: sourceT0Field !< helper pointer
         type(tEcField), pointer :: sourceT1Field !< helper pointer
         real(hp)                :: sourceMissing !< Source side missing value
         real(hp), dimension(:,:,:), pointer :: s3D_T0, s3D_T1   !< 3D representation of linearly indexed array arr1D
         real(hp), dimension(:,:),   pointer :: s2D_T0, s2D_T1   !< 2D representation of linearly indexed array arr1D
         real(hp), dimension(:),     pointer :: s1D_T0, s1D_T1   !< 1D representation of linearly indexed array arr1D
         type(tEcIndexWeight), pointer :: indexWeight !< helper pointer, saved index weights
         type(tEcElementSet), pointer :: sourceElementSet !< source ElementSet
         type(tEcElementSet), pointer :: targetElementSet !< target ElementSet
         type(tEcItem), pointer :: sourceItem !< source Item
         type(tEcItem), pointer :: targetItem !< target item
         integer :: n_layers, n_cols, n_rows, n_points, mp, np, kp, dkp, k_inc
         type(tEcItem), pointer  :: windxPtr ! pointer to item for windx
         type(tEcItem), pointer  :: windyPtr ! pointer to item for windy
         logical :: has_x_wind, has_y_wind
         logical :: has_harmonics            !< Indicate if the quantity is defined in phase and amplitude instead of time.
         real(hp), dimension(:), pointer     :: targetValues
         real(hp), dimension(:), allocatable :: zsrc
         real(hp)                :: ztgt
         double precision        :: PI, phi, xtmp
         integer                 :: time_interpolation
         logical, dimension(:), allocatable  :: missing
         real(hp), dimension(2,2,2,2) :: sourcevals
         real(hp), dimension(2,2)   :: val
         real(hp)                   :: lastvalue
         integer                    :: ii, jj, kk, LL
         integer                    :: jamissing
         integer                    :: ierr
         integer                    :: jsferic
         type(kdtree_instance)      :: treeinst
         real(hp), dimension(:), allocatable :: x_extrapolate    ! temporary array holding targetelementset x for setting up kdtree for interpolation
         integer                    :: col0, row0, col1, row1    ! bounding box in meteo-space spanned by the target elementset

         real(hp) :: amplitude       ! harmonics: amplitude
         real(hp) :: omega           ! harmonics: omega of current component (radians/second)
         real(hp) :: delta_t         ! harmonics: time delta (in seconds) relative to reference time.
         real(hp) :: phase0          ! harmonics: current phase offset angle (in radians)
         integer  :: n_phase_rows    ! harmonics: number of phase rows
         integer  :: n_phase_cols    ! harmonics: number of phase columns
         integer  :: index1d         ! harmonics: 1d array index.

         integer                        :: issparse
         integer, dimension(:), pointer :: ia                    ! sparsity pattern in CRS format, startpointers
         integer, dimension(:), pointer :: ja                    ! sparsity pattern in CRS format, column numbers

         integer                        :: Ndatasize
         integer, dimension(2)          :: idx

         !
         PI = atan(1.d0)*4.d0
         success = .false.
         targetField => null()
         sourceT0Field => null()
         sourceT1Field => null()
         indexWeight => null()
         sourceElementSet => null()
         targetElementSet => null()
         has_harmonics = .false.
         ! ===== preprocessing: rotate windx and windy of the source fields if the array of rotations exists in the filereader =====
         windxPtr   => null()
         windyPtr   => null()
         has_x_wind = .False.
         has_y_wind = .False.
          do i=1, connection%nSourceItems
            if (connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='eastward_wind') then
               windxPtr => connection%SourceItemsPtr(i)%ptr
            endif
            if (connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='northward_wind') then
               windyPtr => connection%SourceItemsPtr(i)%ptr
            endif
            if (connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='grid_eastward_wind' .or. connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='x_wind') then
               windxPtr => connection%SourceItemsPtr(i)%ptr
               has_x_wind = .True.
            endif
            if (connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='grid_northward_wind' .or. connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='y_wind') then
               windyPtr => connection%SourceItemsPtr(i)%ptr
               has_y_wind = .True.
            endif
         enddo

         if (has_x_wind .and. has_y_wind) then
             ! This should only be performed if the standard_name for the x-component was x_wind or grid_eastward_wind (and similar for the y-component)
             ! If eastward_wind was given, this operation should formally be ommitted, according to the CF-convention
             if (associated(windxPtr%elementsetPtr%dir)) then
               do ipt = 1,  windxPtr%elementsetPtr%ncoordinates
                  phi = windxPtr%elementsetPtr%dir(ipt)*PI/180.d0
                  xtmp = windxPtr%SourceT0fieldptr%arr1dptr(ipt)*cos(phi) + windyPtr%SourceT0fieldptr%arr1dptr(ipt)*sin(phi)
                  windyPtr%SourceT0fieldptr%arr1dptr(ipt) = windxPtr%SourceT0fieldptr%arr1dptr(ipt)*(-sin(phi)) + windyPtr%SourceT0fieldptr%arr1dptr(ipt)*cos(phi)
                  windxPtr%SourceT0fieldptr%arr1dptr(ipt) = xtmp
                  xtmp = windxPtr%SourceT1fieldptr%arr1dptr(ipt)*cos(phi) + windyPtr%SourceT1fieldptr%arr1dptr(ipt)*sin(phi)
                  windyPtr%SourceT1fieldptr%arr1dptr(ipt) = windxPtr%SourceT1fieldptr%arr1dptr(ipt)*(-sin(phi)) + windyPtr%SourceT1fieldptr%arr1dptr(ipt)*cos(phi)
                  windxPtr%SourceT1fieldptr%arr1dptr(ipt) = xtmp
               enddo
            endif
         endif
         !
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_spacetimeSaveWeightFactors, extrapolate_spacetimeSaveWeightFactors)
               ! bilinear interpolation in space
               do i=1, connection%nSourceItems
                  sourceItem => connection%sourceItemsPtr(i)%ptr
                  sourceT0Field => sourceItem%sourceT0FieldPtr
                  sourceT1Field => sourceItem%sourceT1FieldPtr
                  sourceMissing = sourceItem%quantityPtr%fillvalue
                  sourceElementSet => sourceItem%elementSetPtr
                  time_interpolation = sourceItem%quantityptr%timeint
                  has_harmonics = associated(sourceItem%hframe)

                  indexWeight => connection%converterPtr%indexWeight

                  targetItem => connection%targetItemsPtr(i)%ptr
                  targetField => targetItem%targetFieldPtr
                  targetValues => targetField%arr1dPtr
                  targetMissing = targetField%MISSINGVALUE
                  targetElementSet => targetItem%elementSetPtr

                  if (sourceElementSet%ofType == elmSetType_samples) then
                     ! call interpolation based on nearest neighbours
                     n_points = targetElementSet%nCoordinates
                     n_cols = sourceElementSet%n_cols
                     n_layers = sourceElementSet%n_layers
                     t0 = sourceT0Field%timesteps
                     t1 = sourceT1Field%timesteps

                     call time_weight_factors(a0, a1, timesteps, t0, t1, timeint=time_interpolation)
                     if (n_layers==0) then
                        do j=1,n_points
                           if ((connection%converterPtr%operandType == operand_replace) .or. &
                               (connection%converterPtr%operandType == operand_replace_if_value)) then ! Dit hoort in de loop beneden per target gridpunt!
                              targetValues(j) = 0.0_hp
                           end if
                           mp = indexWeight%indices(1,j)
                           if (mp>0 .and. mp<=n_cols) then
                              if ( comparereal(sourceT0Field%arr1d(mp), sourceMissing, .true.)==0 .or.   &
                                   comparereal(sourceT1Field%arr1d(mp), sourceMissing, .true.)==0 ) then
                                  call setECMessage("ERROR: ec_converter::ecConverterNetcdf: 1D arrays with _FillValue not yet supported for meteo from stations.")
                                  return
                              end if
                              targetValues(j) = targetValues(j) + a0 * sourceT0Field%arr1d(mp) + a1 * sourceT1Field%arr1d(mp)
                           end if
                        end do
                     else
                        call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Multiple layers sources not yet supported for meteo from stations.")
                        return
                     end if
                  else
                     col0 = sourceItem%sourceT0FieldPtr%bbox(1)
                     row0 = sourceItem%sourceT0FieldPtr%bbox(2)
                     col1 = sourceItem%sourceT0FieldPtr%bbox(3)
                     row1 = sourceItem%sourceT0FieldPtr%bbox(4)

   !                 note: it is assumed that the sparsity pattern of the T1field is the same as of the T0field
                     issparse = sourceItem%sourceT0FieldPtr%issparse
                     ia => sourceItem%sourceT0FieldPtr%ia
                     ja => sourceItem%sourceT0FieldPtr%ja

                     n_points = targetElementSet%nCoordinates
                     if ( issparse == 1 ) then
                        n_cols = sourceElementSet%n_cols
                        n_rows = sourceElementSet%n_rows
                     else
                        n_rows = row1 - row0 + 1
                        n_cols = col1 - col0 + 1
                     end if
                     n_layers = sourceElementSet%n_layers
                     t0 = sourceT0Field%timesteps
                     t1 = sourceT1Field%timesteps

                     if (has_harmonics) then
                        ! No time interpolation, but we DO have to update source values based on phase and amplitude.
                        ! FieldT0 should contain the currently calculated values. (hence a0 = 1, a1 = 0)
                        a0 = 1.0
                        a1 = 0.0
                        ! FOR SIMPLE HARMONIC only one step needed:
                        !   1. calculate with cosine, and time, phase and source (T1) amplitude.
                        ! note: source file Amplitude lives in T1. Phases are indexed: col, row
                        n_phase_rows = sourceElementSet%n_rows
                        n_phase_cols = sourceElementSet%n_cols
                        omega = 2.0_hp*PI/sourceItem%hframe%ec_period
                        delta_t = (timesteps - sourceItem%tframe%ec_refdate) * 86400.0_hp  !< convert to seconds since refdate.
                        if ( issparse == 1 ) then
                            do j = 1, n_rows
                                if ( ia(j+1) > ia(j) ) then
                                    do ipt = ia(j),ia(j+1)-1
                                        amplitude = sourceT1Field%arr1d(ipt)
                                        phase0 = sourceItem%hframe%phases(ja(ipt), j)
                                        if ( comparereal(amplitude, sourceMissing, .true.)==0 .or. &
                                            comparereal(phase0, sourceMissing, .true.)==0 ) then
                                            sourceT0Field%arr1d(ipt) = sourceMissing
                                        else
                                            sourceT0Field%arr1d(ipt) = amplitude * cos(omega * delta_t - phase0 * PI/180.0_hp)
                                        end if
                                    end do
                                end if
                            end do
                        else
                            do j = 1,n_points
                                np = indexWeight%indices(1,j) ! row
                                mp = indexWeight%indices(2,j) ! col
                                if ( np > 0 .and. mp > 0 ) then
                                    do jj = 0,1
                                        do ii = 0,1
                                            ipt = (mp-1+ii) * n_cols + np-1+jj
                                            amplitude = sourceT1Field%arr1d(ipt)
                                            phase0 = sourceItem%hframe%phases(mp+ii, np+jj)
                                             if ( comparereal(amplitude, sourceMissing, .true.)==0 .or. &
                                                comparereal(phase0, sourceMissing, .true.)==0 ) then
                                                sourceT0Field%arr1d(ipt) = sourceMissing
                                            else
                                                sourceT0Field%arr1d(ipt) = amplitude * cos(omega * delta_t - phase0 * PI/180.0_hp)
                                            end if
                                        end do
                                    end do
                                end if
                            end do
                        end if
                     else
                        call time_weight_factors(a0, a1, timesteps, t0, t1, timeint=time_interpolation)
                     end if

                     if (n_layers>0 .and. associated(targetElementSet%z) .and. associated(sourceElementSet%z)) then
                        allocate(zsrc(n_layers))
                        if ( issparse == 1 ) then
                           Ndatasize = ia(n_rows+1)-1
                           s2D_T0(1:Ndatasize,1:n_layers) => sourceT0Field%arr1d
                           s2D_T1(1:Ndatasize,1:n_layers) => sourceT1Field%arr1d
                        else
                           s3D_T0(1:n_cols,1:n_rows,1:n_layers) => sourceT0Field%arr1d
                           s3D_T1(1:n_cols,1:n_rows,1:n_layers) => sourceT1Field%arr1d
                        end if
                        do j=1, n_points
                           kbot = targetElementSet%kbot(j)
                           ktop = targetElementSet%ktop(j)
                           np = indexWeight%indices(1,j)
                           mp = indexWeight%indices(2,j)
                           if (mp > 0 .and. np > 0) then
                              if ((connection%converterPtr%operandType == operand_replace) .or. &
                                  (connection%converterPtr%operandType == operand_replace_if_value)) then
                                 targetValues(kbot:ktop) = 0.0_hp
                              end if
                              ! The save horizontal weigths are used. The vertical weights are recalculated because z changes.
                              ! transformation coefficients for the z-array, target side:
                              select case (targetElementSet%vptyp)
                              case (BC_VPTYP_ZDATUM)
                                 a_t = 1.0_hp
                                 b_t = 0.0_hp
                              case (BC_VPTYP_ZDATUM_DOWN)
                                 a_t = -1.0_hp
                                 b_t = 0.0_hp
                              case (BC_VPTYP_PERCBED)
                                 a_t = (targetElementSet%zmax(j)-targetElementSet%zmin(j))
                                 b_t = targetElementSet%zmin(j)
                              case (BC_VPTYP_PERCSURF)
                                 a_t = (targetElementSet%zmin(j)-targetElementSet%zmax(j))
                                 b_t = targetElementSet%zmax(j)
                              end select

                              ! transformation coefficients for the z-array, source side:
                              select case (sourceElementSet%vptyp)
                              case (BC_VPTYP_ZDATUM)
                                 a_s = 1.0_hp
                                 b_s = 0.0_hp
                              case (BC_VPTYP_ZDATUM_DOWN)
                                 a_s = -1.0_hp
                                 b_s = 0.0_hp
                              end select

                              ! scale source coordinates with factors of target
                              zsrc = (a_s*sourceElementSet%z + b_s - b_t)/a_t

                              ! initialize upper layer kp
                              kp = 2

                              ! dkp: increase direction of (scaled) source z-coordinate zsrc, i.e. zrsc(kp) > zsrc(kp-dkp)
                              if (zsrc(2)-zsrc(1)>0) then
                                 dkp = 1
                              else
                                 dkp = -1
                              end if      ! write source vertical coordinate in terms of target system

                              if ( issparse == 1 ) then
                                idx = (/ np, mp /) ! (bottom-left, upper-left) indices
                              end if

                              do k = kbot, ktop
                                 ztgt = targetElementSet%z(k)

                                 ! get search direction in zsrc
                                 if (dkp*(ztgt-zsrc(kp))>0) then
                                    k_inc = 1
                                 else
                                    k_inc = -1
                                 end if

                                 ! get new upper layer kp
                                 do while ((zsrc(kp-dkp)>ztgt) .or. (zsrc(kp)<=ztgt))
                                    kp = kp + k_inc
                                    if (kp > n_layers .or. kp < 1) exit
                                    if (kp-dkp > n_layers .or. kp-dkp < 1) exit
                                 enddo
                                 if (dkp>0) then
                                    kp = min(max(kp,2),n_layers)
                                 else
                                    kp = min(max(kp,1),n_layers-1)
                                 end if

                                 ! fill source values
                                 if ( issparse == 1 ) then
                                    do kk=0,1
                                       do jj=0,1
                                          do ii=0,1
                                             sourcevals(1+ii,1+jj,1+kk,1) = s2D_T0(idx(1+jj)+ii, kp+dkp*(kk-1))
                                             sourcevals(1+ii,1+jj,1+kk,2) = s2D_T1(idx(1+jj)+ii, kp+dkp*(kk-1))
                                          end do
                                       end do
                                    end do
                                 else
                                    do kk=0,1
                                       do jj=0,1
                                          do ii=0,1
                                             sourcevals(1+ii,1+jj,1+kk,1) = s3D_T0(mp+ii, np+jj, kp+dkp*(kk-1) )
                                             sourcevals(1+ii,1+jj,1+kk,2) = s3D_T1(mp+ii, np+jj, kp+dkp*(kk-1) )
                                          end do
                                       end do
                                    end do
                                 end if

                                 call extrapolate_missing(sourcevals, sourceMissing, jamissing)

                                 if ( jamissing>0 ) then
                                    targetValues(k) = targetMissing
                                 else

                                    ! horizontal interpolation
                                    val = 0d0   ! (down-up,old-new)
                                    do ll=1,2
                                       do kk=1,2
                                          val(kk,ll) = val(kk,ll) + sourcevals(1, 1, kk, ll) * indexWeight%weightFactors(1,j)    !   4      3
                                          val(kk,ll) = val(kk,ll) + sourcevals(2, 1, kk, ll) * indexWeight%weightFactors(2,j)
                                          val(kk,ll) = val(kk,ll) + sourcevals(2, 2, kk, ll) * indexWeight%weightFactors(3,j)
                                          val(kk,ll) = val(kk,ll) + sourcevals(1, 2, kk, ll) * indexWeight%weightFactors(4,j)    !   1      2
                                       end do
                                    end do
                                    ! get weights for vertical interpolation
                                    wb = (zsrc(kp) - ztgt)/(zsrc(kp)-zsrc(kp-dkp))
                                    wb = min(max(wb,0.0_hp),1.0_hp)                       ! zeroth-order extrapolation beyond range of source vertical coordinates
                                    wt = (1.0_hp - wb)

                                    if (has_harmonics) then
                                       call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Harmonics not (yet) implemented for layers.")
                                       return
                                    else
                                       ! interpolating between times and between vertical layers
                                       targetValues(k) = targetValues(k) + a0*(wb*val(1,1) + wt*val(2,1)) + a1*(wb*val(1,2) + wt*val(2,2))
                                    end if
                                 end if
                              end do

                              ! fill missing values
                              lastvalue = targetMissing
                              do k=ktop,kbot,-1
                                 if ( targetValues(k).ne.targetMissing ) then
                                    lastvalue = targetValues(k)
                                 else if ( lastvalue.ne.targetMissing ) then
                                    targetValues(k) = lastvalue
                                 end if
                              end do

                              lastvalue = targetMissing
                              do k=kbot,ktop
                                 if ( targetValues(k).ne.targetMissing ) then
                                    lastvalue = targetValues(k)
                                 else if ( lastvalue.ne.targetMissing ) then
                                    targetValues(k) = lastvalue
                                 end if
                              end do   ! loop over vertical
                           end if   ! valid mp and np
                        end do   ! loop over the target elementset
                        if (allocated(zsrc)) deallocate(zsrc)
                     else
                        if ( issparse == 1 ) then
                           Ndatasize = ia(n_rows+1)-1
                           S1D_T0(1:Ndatasize) => sourceT0Field%arr1d
                           S1D_T1(1:Ndatasize) => sourceT1Field%arr1d
                        else
                           s2D_T0(1:n_cols,1:n_rows) => sourceT0Field%arr1d
                           s2D_T1(1:n_cols,1:n_rows) => sourceT1Field%arr1d
                        end if

                        if (connection%converterPtr%interpolationType == extrapolate_spacetimeSaveWeightFactors) then
                           allocate(x_extrapolate(n_points))
                           x_extrapolate = targetElementSet%x
                        endif
                        allocate(missing(n_points))
                        missing = .False.
                        do j=1, n_points
                           np = indexWeight%indices(1,j)
                           mp = indexWeight%indices(2,j)
                           jamissing = 0
                           if (mp > 0 .and. np > 0) then ! if mp and np both valid, this is an interior point of the meteo domain, else ignore
                                                         ! check missing values for points with valid mp and np

                              if ( issparse == 1 ) then
                                idx = (/ np, mp /) ! (bottom-left, upper-left) indices
                              end if

                              ! fill source values
                              kk = 0   ! 2D only
                              if ( issparse == 1 ) then
                                 do jj=0,1
                                    do ii=0,1
                                       sourcevals(1+ii,1+jj,1+kk,1) = s1D_T0(idx(1+jj)+ii)
                                       sourcevals(1+ii,1+jj,1+kk,2) = s1D_T1(idx(1+jj)+ii)
                                    end do
                                 end do
                              else
                                 do jj=0,1
                                    do ii=0,1
                                       sourcevals(1+ii,1+jj,1+kk,1) = s2D_T0(mp+ii, np+jj)
                                       sourcevals(1+ii,1+jj,1+kk,2) = s2D_T1(mp+ii, np+jj)
                                    end do
                                 end do
                              end if

                              if (connection%converterPtr%operandType == operand_replace) then
                                 targetValues(j) = 0.0_hp
                              end if

                              kloop2D: do jj=0,1
                                 do ii=0,1
                                    if ( comparereal(sourcevals(1+ii, 1+jj, 1, 1), sourceMissing, .true.)==0 .or.   &
                                       comparereal(sourcevals(1+ii, 1+jj, 1, 2), sourceMissing, .true.)==0 ) then
                                       jamissing = jamissing + 1
                                       exit kloop2D
                                    end if
                                 end do
                              end do kloop2D
                              if (jamissing>0) then                                                                        ! if insufficient data for bi-linear interpolation
                                 missing(j) = .True.    ! Mark missings in the target grid in a temporary logical array
                                 if (allocated(x_extrapolate)) x_extrapolate(j)=ec_undef_hp                                ! no-data -> unelectable for kdtree later
                              else
                                 if (connection%converterPtr%operandType==operand_replace_if_value) then
                                    targetValues(j) = 0.0_hp
                                 end if
                                 targetValues(j) = targetValues(j) + a0 * sourcevals(1, 1, 1, 1) * indexWeight%weightFactors(1,j)        !  4                 3
                                 targetValues(j) = targetValues(j) + a1 * sourcevals(1, 1, 1, 2) * indexWeight%weightFactors(1,j)
                                 targetValues(j) = targetValues(j) + a0 * sourcevals(2, 1, 1, 1) * indexWeight%weightFactors(2,j)
                                 targetValues(j) = targetValues(j) + a1 * sourcevals(2, 1, 1, 2) * indexWeight%weightFactors(2,j)
                                 targetValues(j) = targetValues(j) + a0 * sourcevals(2, 2, 1, 1) * indexWeight%weightFactors(3,j)
                                 targetValues(j) = targetValues(j) + a1 * sourcevals(2, 2, 1, 2) * indexWeight%weightFactors(3,j)
                                 targetValues(j) = targetValues(j) + a0 * sourcevals(1, 2, 1, 1) * indexWeight%weightFactors(4,j)
                                 targetValues(j) = targetValues(j) + a1 * sourcevals(1, 2, 1, 2) * indexWeight%weightFactors(4,j)        !  1                 2
                                 if (allocated(x_extrapolate)) x_extrapolate(j)=targetElementSet%x(j)                      ! x_extrapolate is a copy of the x with missing points marked by ec_undef_hp
                              end if
                           end if   ! 2D or 3D sources
                        end do      ! points j
                        if (connection%converterPtr%interpolationType == extrapolate_spacetimeSaveWeightFactors) then      ! if extrapolation permitted ...
                           do j=1, n_points                                                                                ! Loop over the grid for missing in the target grid
                              if (missing(j)) then                                                                         ! Can only be an interior point with ORIGINALLY valid mp and np
                                 if (indexWeight%substndx(j)==0) then                                                      ! if we had not yet searched for a replacement value in the target grid, do so.
                                    if (treeinst%itreestat /= ITREE_OK) then
   !                                   jsferic = merge(1,0,targetElementSet%ofType == elmSetType_spheric)                  ! adopt the global jsferic setting of the EC-module instance
                                       jsferic = 0                                                                         ! in EC-converter treat all coordinates as carthesian
                                                                                                                           ! Now kdtree over target locations which do NOT yet have a value ('missing')
                                       call build_kdtree(treeinst, n_points, x_extrapolate, targetElementSet%y, ierr, jsferic, ec_undef_hp)
                                    endif
                                    call make_queryvector_kdtree(treeinst, targetElementSet%x(j), targetElementSet%y(j), jsferic)
                                    call kdtree2_n_nearest(treeinst%tree, treeinst%qv, 1, treeinst%results)                ! use the first nearest neighbour
                                    indexWeight%substndx(j) = treeinst%results(1)%idx                                      ! store its index for for later use (in an array that is init to zero)
                                 endif
                                 targetValues(j) = targetValues(indexWeight%substndx(j))                                   ! and copy its value as a target value for the target point with missing data.
                              endif
                           end do   ! points j
                        endif
                        if (allocated(missing)) deallocate(missing)
                        if (allocated(x_extrapolate)) deallocate(x_extrapolate)
                        call delete_kdtree2(treeinst)
                     end if         ! 2d or 3d
                  end if   ! if the elementset type was not samples
                  connection%targetItemsPtr(i)%ptr%targetFieldPtr%timesteps = timesteps
               end do   ! target items i
               if (connection%converterPtr%interpolationType == extrapolate_spacetimeSaveWeightFactors) then
                  connection%converterPtr%extrapolated = .true.
               endif
            case (interpolate_time, interpolate_time_extrapolation_ok)
               ! linear interpolation in time
               do i=1, connection%nSourceItems
                  t0 = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%timesteps
                  t1 = connection%sourceItemsPtr(i)%ptr%sourceT1FieldPtr%timesteps
                  targetField => connection%targetItemsPtr(i)%ptr%targetFieldPtr
                  if (t0 > t1) then
                     call setECMessage("WARNING: ec_converter::ecConverterNetcdf: Only one data field available.")
                     if (connection%converterPtr%interpolationType == interpolate_time) then
                        call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Extrapolation not allowed.")
                        return
                     endif
                     ! ===== operation =====
                     select case(connection%converterPtr%operandType)
                        case(operand_replace, operand_replace_if_value)
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Target ElementSet's number of coordinates not set.")
                              return
                           endif
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates /= &
                             & connection%sourceItemsPtr(i)%ptr%elementSetPtr%nCoordinates) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: ElementSet's number of coordinates differs (Source vs. Target).")
                              return
                           endif
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              targetField%arr1dPtr(j) = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                           enddo
                           targetField%timesteps = timesteps
                        case default
                           call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported operand type requested.")
                           return
                     end select
                  else
                     call time_weight_factors(a0, a1, timesteps, t0, t1, extrapolated)
                     if (extrapolated .and. connection%converterPtr%interpolationType==interpolate_time) then
                        call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Extrapolation not allowed.")
                        return
                     endif
                     ! ===== operation =====
                     select case(connection%converterPtr%operandType)
                        case(operand_replace, operand_replace_if_value)
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Target ElementSet's number of coordinates not set.")
                              return
                           endif
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates /= &
                             & connection%sourceItemsPtr(i)%ptr%elementSetPtr%nCoordinates) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: ElementSet's number of coordinates differs (Source vs. Target).")
                              return
                           endif
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              sourceValueT0           = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                              sourceValueT1           = connection%sourceItemsPtr(i)%ptr%sourceT1FieldPtr%arr1dPtr(j)
                              targetField%arr1dPtr(j) = sourceValueT0*a0 + sourceValueT1*a1
                           enddo
                           targetField%timesteps = timesteps
                        case default
                           call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported operand type requested.")
                           return
                     end select
                  endif
               end do
            case default
               call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterNetcdf

      ! =======================================================================

      !> For every connected target item,
      !> if the target field has an associated scalar pointer, fill it with the first element of the arr1DPtr array.
      !> This scalar pointer is connected with a scalar in a kernel, such as a single field in a derived type
      function ecConverterUpdateScalar(connection) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         type(tEcField), pointer :: fieldPtr
         integer :: itgt
         !
         success = .false.
         do itgt = 1, connection%nTargetItems
            fieldPtr => connection%targetItemsPtr(itgt)%ptr%targetFieldPtr
            if (associated(fieldPtr)) then
               if (associated(fieldPtr%scalarPtr)) then
                  fieldPtr%scalarPtr = fieldPtr%arr1DPtr(1)
               endif
            endif
         enddo
         success = .true.
      end function ecConverterUpdateScalar

      ! =======================================================================

      !> Selects the index of the polyline segment that intersects with line e--en
      !! with the intersection closest to point e.
      !! The search range is thus from e to en, and not a distance rdis as before.
      !! The normal direction is now
      !! defined by e--en and not normal to the polyline. Also, *all* polyline
      !! segments are checked, not the closest based on dbdistance of pli points.
      subroutine polyindexweight( xe, ye, xen, yen, xs, ys, kcs, ns, kL, wL, kR, wR)
         integer,                intent(in)  :: ns       !< Dimension of polygon OR LINE BOUNDARY
         real(hp), dimension(:), intent(in)  :: xs       !< polygon
         real(hp), dimension(:), intent(in)  :: ys
         integer,  dimension(:), intent(in)  :: kcs      !< polygon mask
         real(hp),               intent(in)  :: xe, ye   !
         real(hp),               intent(in)  :: xen, yen !< in input uitstekers, on output SL and CRP
         integer,                intent(out) :: kL       !< Index of left nearest polyline point (with kcs==1!)
         real(hp),               intent(out) :: wL       !< Relative weight of left nearest polyline point.
         integer,                intent(out) :: kR       !< Index of right nearest polyline point (with kcs==1!)
         real(hp),               intent(out) :: wR       !< Relative weight of right nearest polyline point.
         !
         integer  :: k, km, JACROS
         real(hp) :: dis, disM, disL, disR
         real(hp) :: SL,SM,SMM,SLM,XCR,YCR,CRP,CRPM,DEPS
         !
         DISM = huge(DISM)
         kL   = 0 ! Default: No valid point found
         kR   = 0 ! idem
         wL   = 0.0_hp
         wR   = 0.0_hp
         km   = 0
         crpm = 0.0_hp
         disL = 0.0_hp
         disR = 0.0_hp
         DEPS = 1.0E-3_hp
         !
         do k = 1, ns-1
            crp = 0.0_hp
            call CROSS(xe, ye, xen, yen, xs(k), ys(k), xs(k+1), ys(k+1), JACROS,SL,SM,XCR,YCR,CRP)
            if (SL >= 0.0_hp .and. SL <= 1.0_hp .and. SM > -DEPS .and. SM < 1.0_hp+DEPS) then ! instead of jacros==1, solves firmijn's problem
               DIS = DBDISTANCE(XE, YE, XCR, YCR)
               if (DIS < DISM) then ! Found a better intersection point
                  DISM = DIS
                  km   = k
                  SMM  = SM
                  SLM  = SL
                  CRPM = CRP
               end if
            end if
         enddo
         !
         if (km > 0) then
            dis  = dbdistance(xs(km), ys(km), xs(km+1), ys(km+1)) ! Length of this segment.
            ! Find nearest valid polyline point left of the intersection (i.e.: kcs(kL) == 1)
            disL = SMM*dis
            do k = km,1,-1
               if (kcs(k) == 1) then
                  kL = k
                  exit ! Valid point on the left (distance was already included in disL)
               else if (k > 1) then
                  disL = disL + dbdistance(xs(k-1), ys(k-1), xs(k), ys(k)) ! Add entire length of this segment.
               end if
            end do
            ! Find nearest valid polyline point right of the intersection (i.e.: kcs(kR) == 1)
            disR = (1.0_hp-SMM)*dis
            do k = km+1,ns
               if (kcs(k) == 1) then
                  kR = k
                  exit ! Valid point on the left (distance was already included in disL)
               else if (k < ns) then
                  disR = disR + dbdistance(xs(k), ys(k), xs(k+1), ys(k+1)) ! Add entire length of this segment.
               end if
            end do
         end if
         !
         if (kL /= 0 .and. kR /= 0) then
            wL = disR/(disL+disR)
            wR = 1.0_hp-wL
         else if (kL /= 0) then
            wL = 1.0_hp
         else if (kR /= 0) then
            wR = 1.0_hp
         end if
      end subroutine polyindexweight

      !> Checks whether lines 1-2 and 3-4 intersect.
      !! @param[in] x1,y1,x2,y2,x3,y3,x4,y4 x- and y-coords of line endpoints.
      !! @param[out] jacros 1 if lines cross (intersect), 0 if not.
      !! @param[out] sl lambda in [0,1] on line segment 1-2 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] sm lambda in [0,1] on line segment 3-4 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] xcr,ycr x-coord. of intersection point.
      SUBROUTINE CROSS(x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP)
         use ieee_arithmetic, only: ieee_is_nan

         double precision, intent(inout) :: crp !< crp (in)==-1234 will make crp (out) non-dimensional
         double precision :: det
         double precision :: eps
         integer :: jacros, jamakenondimensional
         double precision :: sl
         double precision :: sm
         double precision, intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
         double precision :: x21, y21, x31, y31, x43, y43, xcr, ycr
         double precision                  :: dmiss    = -999d0

   !     safety check on crp (in)
         if ( ieee_is_nan(crp) ) then
            crp = 0.0_hp
         end if

         ! Set defaults for no crossing at all:
         jamakenondimensional = 0
         if ( abs(crp+1234d0).lt.0.5d0 ) then
            jamakenondimensional = 1
            crp = 0.0_hp
         endif

         JACROS = 0
         EPS    = 0.00001_hp
         SL     = DMISS
         SM     = DMISS

         !call getdxdy(x1,y1,x2,y2,x21,y21) ! TODO : all is cartesian, kernel must provide it as such
         x21 = x2-x1
         y21 = y2-y1
         !call getdxdy(x3,y3,x4,y4,x43,y43) ! TODO : all is cartesian, kernel must provide it as such
         x43 = x4-x3
         y43 = y4-y3
         !call getdxdy(x1,y1,x3,y3,x31,y31) ! TODO : all is cartesian, kernel must provide it as such
         x31 = x3-x1
         y31 = y3-y1

         DET =  X43*Y21 - Y43*X21

   !     SPvdP: make eps have proper dimension
         EPS = max(EPS*MAXVAL((/ X21,Y21,X43,Y43,X31,Y31 /)), tiny(0.0_hp))
         IF (ABS(DET) .LT. EPS) THEN
            RETURN
         ELSE
            SM = (Y31*X21 - X31*Y21) / DET
            IF (ABS(X21) .GT. EPS) THEN
               SL = (SM*X43 + X31) / X21
            ELSE IF (ABS(Y21) .GT. EPS) THEN
               SL = (SM*Y43 + Y31) / Y21
            ELSE
               SL   = 0.0_hp
            ENDIF
            IF (SM .GE. 0.0_hp .AND. SM .LE. 1d0 .AND. &
                SL .GE. 0.0_hp .AND. SL .LE. 1d0) THEN
               JACROS = 1
            ENDIF
            XCR = X1 + SL*(X2-X1)
            YCR = Y1 + SL*(Y2-Y1)
            if ( jamakenondimensional == 1 ) then  ! make crp non-dimensional (for spline2curvi)
               CRP = -DET / ( sqrt(x21**2+y21**2) * sqrt(x43**2 + y43**2) + 1d-8 )
            else
               CRP = -DET
            end if
         ENDIF
         RETURN
      END SUBROUTINE CROSS

      !> distance point 1 -> 2
      real(hp) function dbdistance(x1,y1,x2,y2)
         real(hp) :: x1, y1, x2, y2
         ! locals
         real(hp) :: ddx, ddy, rr
         ! double precision :: getdx, getdy ! TODO : all is cartesian, kernel must provide it as such
         real(hp) :: dmiss    = -999d0
         !
         if ( x1 == DMISS .or. x2 == DMISS .or. y1 == DMISS .or. y2 == DMISS ) then
            dbdistance = 0.0_hp
            return
         end if
         !
         !ddx = getdx(x1,y1,x2,y2) ! TODO : all is cartesian, kernel must provide it as such
         ddx = x2-x1
         !ddy = getdy(x1,y1,x2,y2) ! TODO : all is cartesian, kernel must provide it as such
         ddy = y2-y1
         rr  = ddx*ddx + ddy*ddy
         if (rr == 0.0_hp) then
            dbdistance = 0.0_hp
         else
            dbdistance = sqrt(rr)
         endif
      end function dbdistance

!>    get field bounding box indices
      subroutine ecConverterGetBbox(instancePtr, itemID, t01, col0, col1, row0, row1, ncols, nrows, issparse, Ndatasize)
         implicit none

         type(tEcInstance), pointer :: instancePtr  !< intent(in)
         integer,       intent(in)  :: itemId       !< unique Item id
         integer,       intent(in)  :: t01          !< field 0 (0) or 1 (other)
         integer,       intent(out) :: col0,col1,row0,row1  !< boundix box start (0) and end (1) indices
         integer,       intent(out) :: ncols, nrows

         type(tEcItem),     pointer :: itemPtr       !< Item corresponding to itemId
         type(tEcField),    pointer :: FieldPtr

         integer,       intent(out) :: issparse
         integer,       intent(out) :: Ndatasize

         col0 = 0
         row0 = 0
         col1 = 0
         row1 = 0

         itemPtr => ecSupportFindItem(instancePtr, itemId)

         if ( associated(itemPtr) ) then
            if ( t01 == 0 ) then
               FieldPtr => itemPtr%sourceT0FieldPtr
            else
               FieldPtr => itemPtr%sourceT1FieldPtr
            end if
            col0 = FieldPtr%bbox(1)
            row0 = FieldPtr%bbox(2)
            col1 = FieldPtr%bbox(3)
            row1 = FieldPtr%bbox(4)

            ncols = itemPtr%elementSetPtr%n_cols
            nrows = itemPtr%elementSetPtr%n_rows

            issparse = FieldPtr%issparse
            Ndatasize = FieldPtr%ia(nrows+1)-1
         end if

         return
      end subroutine ecConverterGetBbox

      subroutine MaskToSparse(n_cols,n_rows,imask, ia, ja)
         implicit none

         integer,                            intent(in)  :: n_cols    !< number of columns
         integer,                            intent(in)  :: n_rows    !< number of rows
         integer, dimension(n_cols,n_rows),  intent(in)  :: imask     !< active (1) or not (0)
         integer, dimension(:), allocatable, intent(out) :: ia        !< startpointers
         integer, dimension(:), allocatable, intent(out) :: ja        !< column numbers

         integer                                         :: mp        ! column
         integer                                         :: np        ! row
         integer                                         :: i

!        allocate startpointer (increments)
         allocate(ia(n_rows+1))

!        store increments of startpointer in ia
         do np=1,n_rows
           ia(np+1) = 0
           do mp=1,n_cols
              if ( imask(mp,np) == 1 ) then
                 ia(np+1) = ia(np+1) + 1
              end if
           end do
         end do

!        make startpointers from increments
         ia(1) = 1
         do np=1,n_rows
            ia(np+1) = ia(np) + ia(np+1)
         end do

!        allocate 2nd-index numbers
         allocate(ja(ia(n_rows+1)-1))

!        fill 2nd-index numbers
         i = 0
         do np=1,n_rows
            do mp=1,n_cols
               if ( imask(mp,np) == 1 ) then
                  i = i+1
                  ja(i) = mp
               end if
            end do
         end do

         return
      end subroutine MaskToSparse

      subroutine SetSparsityPattern(srcfld, n_cols, n_rows, ia, ja)
         implicit none

         type(tEcField),                       intent(inout) :: srcfld  !< source field
         integer,                              intent(in)    :: n_cols  !< number of columns
         integer,                              intent(in)    :: n_rows  !< number of rows
         integer, dimension(:),   allocatable, intent(in)    :: ia      !< sparsity pattern in CRS format, start pointers
         integer, dimension(:),   allocatable, intent(in)    :: ja      !< sparsity pattern in CRS format, column numbers


         if ( associated(srcfld%ia) ) deallocate(srcfld%ia)
         allocate(srcfld%ia(n_rows+1))
         srcfld%ia = ia
         if ( associated(srcfld%ja) ) deallocate(srcfld%ja)
         allocate(srcfld%ja(ia(n_rows+1)-1))
         srcfld%ja = ja
         srcfld%issparse = 1

!        effectively bounding box
         srcfld%bbox = (/ 1, 1, n_cols, n_rows/)

         return
      end subroutine SetSparsityPattern

!< convert (1:ncol,2:mrow) indices of lower-left source point to sparse indices of (1:lower-left,2:upper-left) source points (out),
!<  left-right: increasing column index,
!<  down-up: increasing row index
!< note: input indices are (row,col), not (col,row)
      subroutine ConvertToSparseIndices(n_points, indices, n_rows, ia, ja)
         implicit none

         integer,                        intent(in)    :: n_points  !< number of target points
         integer, dimension(2,n_points), intent(inout) :: indices   !<(mrow,ncol) indices of lower-left source point (in), sparse index of (lower-left,upper-left) source points (out)
         integer,                        intent(in)    :: n_rows    !< number of rows of source
         integer, dimension(n_rows),     intent(in)    :: ia        !< sparsity pattern in CRS format, start pointers
         integer, dimension(:),          intent(in)    :: ja        !< sparsity pattern in CRS format, column numbers

         integer                                       :: idx
         integer                                       :: i, j, last
         integer                                       :: irow, idownup
         integer                                       :: mcol, nrow

         do i=1,n_points
!           get (mcol,nrow) of lower-left source point
            mcol = indices(2,i)
            nrow = indices(1,i)

!           check if indices are assigned
            if ( mcol == 0 .or. nrow == 0 ) then
               cycle
            end if

!           find sparse indeces of lower-left and upper-left source points
            idx = 0
            idownup = 0
            do idownup=1,2
               irow = nrow + idownup-1

               idx = 0
               if (irow < size(ia)) then
                  last = ia(irow+1)-1
               else
                  last = size(ja)
               endif
               do j=ia(irow),last
                  if ( ja(j) == mcol ) then
                     idx = j
                     exit
                  end if
               end do

!              check if index is found
               if ( idx.gt.0 ) then
!                 overwrite index
                  indices(idownup,i) = idx
               else
!                 error
                  call setECMessage("ERROR: conversion to sparse indices failed for point ", i)
               end if
            end do

         end do

         return
      end subroutine ConvertToSparseIndices

end module m_ec_converter
