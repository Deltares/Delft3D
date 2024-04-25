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

!> This module contains the spatial extrapolation methods for the EcConverter.
!! @author Sander.vanderPijl@deltares.nl
!! @author Edwin.Spee@deltares.nl
module m_ec_spatial_extrapolation
   use precision, only : hp
   use m_ec_typedefs, only : tEcField, tEcElementSet, tEcItem, tEcIndexWeight, tFlexibleIndexWeightFactor
   use m_ec_message, only : setECMessage

   use kdtree2Factory
   implicit none

   private

   public :: extrapolate_missing, init_spatial_extrapolation, updateInterpolation, extrapolateValue

   real(kind=hp) :: max_search_radius = 1.0e6_hp  !< in meter
   integer       :: jsferic           = 1

   contains

   ! =======================================================================

       !> set the value of max_search_radius and jsferic,
       !! max_search_radius is updated if given value > 0
       subroutine init_spatial_extrapolation(rvalue, ivalue)
          real(kind=hp), intent(in) :: rvalue      !< new value for max_search_radius
          integer,       intent(in) :: ivalue      !< new value for jsferic

          if (rvalue > 0.0_hp) then
             max_search_radius = rvalue
          endif
          jsferic = ivalue
       end subroutine init_spatial_extrapolation

!> extrapolate missing values
      subroutine extrapolate_missing(vals, Missing, jamissing)
         implicit none

         real(kind=hp), dimension(2,2,2,2), intent(inout) :: vals      !< values
         real(kind=hp),                     intent(in)    :: Missing   !< missing values
         integer,                           intent(out)   :: jamissing !< missing values left (1) or not (0)

         real(kind=hp)                                    :: value
         integer, dimension(4)                            :: imiss, jmiss
         integer                                          :: nummissing, n

         integer :: i, j, k, L
         integer :: i1, j1, i2, j2

         jamissing = 0

!        first two dimensions (horizontal)
         do L=1,2
            do k=1,2

!              count number of missing values
               nummissing = 0
               imiss = 0
               jmiss = 0
               n = 0
               do j=1,2
                  do i=1,2
                     if ( vals(i,j,k,L)==Missing ) then
                        nummissing = nummissing+1
                        imiss(nummissing) = i
                        jmiss(nummissing) = j
                     else
                        n = n+1
                        imiss(5-n) = i
                        jmiss(5-n) = j
                     end if
                  end do
               end do

               if ( nummissing == 1 ) then
!                 linear extrapolation
                  i = imiss(1)
                  j = jmiss(1)
                  i1 = 3-i
                  j1 = 3-j
                  vals(i,j,k,L) = vals(i,j1,k,L) + vals(i1,j,k,L) - vals(i1,j1,k,L)
               else if ( nummissing == 2 ) then
!                 linear extrapolation in one direction, constant in other
                  i1 = imiss(1)
                  j1 = jmiss(1)
                  i2 = imiss(2)
                  j2 = jmiss(2)
                  if ( i1 == i2 ) then
                     vals(i1,:,k,L) = vals(3-i1,:,k,L)
                  else if ( j1 == j2 ) then
                     vals(:,j1,k,L) = vals(:,3-j1,k,L)
                  else
                     vals(i1,j1,k,L) = 0.5d0*(vals(i2,j1,k,L)+vals(i1,j2,k,L))
                     vals(i2,j2,k,L) = vals(i1,j1,k,L)
                  end if
               else if ( nummissing == 3 ) then
                  i = imiss(4)
                  j = jmiss(4)
                  value = vals(i,j,k,L)
                  vals(:,:,k,L) = value
               else if ( nummissing == 4 ) then
!                 can not extrapolate
               end if
            end do
         end do
         
!        third dimension
         do L=1,2
            do j=1,2
               do i=1,2
                  if ( vals(i,j,1,L) == Missing .or. vals(i,j,2,L) == Missing ) then
                     if ( vals(i,j,1,L) == Missing .and. vals(i,j,2,L) /= Missing ) then
                        vals(i,j,1,L) = vals(i,j,2,L)
                     else if ( vals(i,j,2,L) == Missing .and. vals(i,j,1,L) /= Missing ) then
                        vals(i,j,2,L) = vals(i,j,1,L)
                     else
!                       can not extrapolate
                        jamissing = 1
                     end if
                  end if
               end do
            end do
         end do

      end subroutine extrapolate_missing

      !> search for nearest neighbour using a kdtree
      function nearest_sample_wrapper(kdtree, sourceItem, targetElementSet, targetElementID, field, p, q, w) result(success)
         type(kdtree_instance), intent(inout) :: kdtree           !< kdtree instance
         type(tEcElementSet),   intent(in)    :: targetElementSet !< target element set
         type(tEcItem),         intent(in)    :: sourceItem       !< source item
         integer,               intent(in)    :: targetElementID  !< target element Id
         real(kind=hp),         intent(in)    :: field(:)         !< source field
         integer,               intent(out)   :: p(:)             !< resulting m points
         integer,               intent(out)   :: q(:)             !< resulting n points
         real(kind=hp),         intent(out)   :: w(:)             !< resulting interpolation weights
         logical                              :: success          !< function result

         integer                            :: NN
         real(kind=hp)                      :: xk, yk, dmiss, sumw, distance
         real(kind=hp), allocatable         :: xs(:), ys(:)
         integer                            :: Ns, ierror, dim1, dim2, i, j, ii

         success = .true.

         NN = size(w)

         xk = targetElementSet%x(targetElementID)
         yk = targetElementSet%y(targetElementID)

         dim1 = size(sourceItem%elementsetptr%x)
         dim2 = size(sourceItem%elementsetptr%y)

         if (kdtree%itreestat /= ITREE_OK) then
            !
            ! first time: preparations for call to build_kdtree
            !
            dmiss = sourceItem%sourcet0fieldptr%missingvalue
            Ns = dim1 * dim2
            allocate(xs(Ns), ys(Ns), stat=ierror)
            if (ierror /= 0) then
               call setECMessage("Allocate error in nearest_sample_wrapper with size ", 4*Ns)
               success = .false.
            else
               ii = 0
               do j = 1, dim2
                  do i = 1, dim1
                     ii = ii + 1
                     if (field(ii) == dmiss) then
                        xs(ii) = dmiss
                        ys(ii) = dmiss
                     else
                        xs(ii) = sourceItem%elementsetPtr%x(i)
                        ys(ii) = sourceItem%elementsetPtr%y(j)
                     endif
                  enddo
               enddo
               call build_kdtree(kdtree, NS, xs, ys, ierror, jsferic, dmiss)
               deallocate(xs, ys)
               success = (ierror == 0)
            endif
         endif

         if (success) then
            call make_queryvector_kdtree(kdtree, xk, yk, jsferic)
            !    find nearest sample points
            call kdtree2_n_nearest(kdtree%tree, kdtree%qv, NN, kdtree%results)
            !    copy to output
            sumw = 0.0_hp
            do i = 1, NN
               ii = kdtree%results(i)%idx
               p(i) = 1 + mod(ii-1, dim1)
               q(i) = 1 + (ii-1) / dim1
               distance = sqrt(kdtree%results(i)%dis)
               if (i == 1 .or. distance < max_search_radius) then
                  w(i) = 1.0_hp / distance
                  sumw = sumw + w(i)
               else
                  w(i) = 0.0_hp
               endif
            enddo
            do i = 1, NN
               w(i) = w(i) / sumw
            enddo
         endif

      end function nearest_sample_wrapper

      !> add extrapolated indices and weights
      function addExtrapolatedIndicesAndWeights(indexWeight, p, q, w, j) result(success)
         type(tEcIndexWeight), intent(inout) :: indexWeight !< struct holding all indices and weights
         integer,              intent(in)    :: j           !< source index
         integer,              intent(in)    :: p(:)        !< first coordinate in 2D case
         integer,              intent(in)    :: q(:)        !< second coordinate in 2D case
         real(kind=hp),        intent(in)    :: w(:)        !< interpolation weights
         logical                             :: success     !< function result

         type(tFlexibleIndexWeightFactor) :: flexIndexWeight  !< temp. instance of tFlexibleIndexWeightFactor
         integer                          :: NN               !< dimension of p, q and w
         integer                          :: i                !< loop counter
         integer                          :: ierror           !< error code of allocate
         integer                          :: newIndex         !< index for new entry in indexWeight%flexIndexWeights

         NN = count(w > 0.0_hp)
         newIndex = indexWeight%curSizeFlex + 1

         allocate(flexIndexWeight%indices(2, NN), flexIndexWeight%weights(NN), stat=ierror)
         success = (ierror == 0)

         if (size(indexWeight%flexIndexWeights) <= newIndex) then
            success = success .and. resize(indexWeight%flexIndexWeights)
         endif

         if (success) then
            do i = 1, NN
               flexIndexWeight%indices(1, i) = p(i)
               flexIndexWeight%indices(2, i) = q(i)
               flexIndexWeight%weights(i)    = w(i)
            enddo

            indexWeight%indices(1,j)               = -newIndex   ! note the minus sign
            indexWeight%indices(2,j)               =  NN         ! not used
            indexWeight%weightFactors(1:4,j)       = -999.0_hp   ! not used
            indexWeight%flexIndexWeights(newIndex) = flexIndexWeight
            indexWeight%curSizeFlex                = newIndex
         endif
      end function addExtrapolatedIndicesAndWeights

      !> helper function to resize the flexIndexWeights
      function resize(flexIndexWeights) result (success)
         type(tFlexibleIndexWeightFactor), pointer, intent(inout) :: flexIndexWeights(:)  !< the array to be resized
         logical                                                  :: success              !< function result

         type(tFlexibleIndexWeightFactor), pointer :: newFlexIndexWeights(:)  !< new array of type tFlexibleIndexWeightFactor
         integer                                   :: oldSize                 !< size of input array flexIndexWeights
         integer                                   :: newSize                 !< size of newFlexIndexWeights
         integer                                   :: i                       !< loop counter
         integer                                   :: ierror                  !< error code of allocate
         integer, parameter                        :: startLength = 8         !< minimal length of newFlexIndexWeights

         oldSize = size(flexIndexWeights)
         newSize = max(startLength, oldSize * 2)

         allocate(newFlexIndexWeights(newSize), stat=ierror)
         success = (ierror == 0)

         if (success) then
            do i = 1, oldSize
               newFlexIndexWeights(i) = flexIndexWeights(i)
            enddo

            if (oldSize > 0) deallocate(flexIndexWeights)

            flexIndexWeights => newFlexIndexWeights
         endif
      end function resize

      !> get extrapolated value
      subroutine extrapolateValue(targetValue, indexWeight, targetElementID, a0, a1, s2D_T0, s2D_T1)
         real(kind=hp),        intent(inout) :: targetValue      !< function result (extrapolated values are added)
         type(tEcIndexWeight), intent(in)    :: indexWeight      !< struct holding all indices and weights
         integer,              intent(in)    :: targetElementID  !< target element Id
         real(kind=hp),        intent(in)    :: a0               !< weigth factor first time
         real(kind=hp),        intent(in)    :: a1               !< weigth factor 2nd time
         real(kind=hp),        intent(in)    :: s2D_T0(:,:)      !< field for first time
         real(kind=hp),        intent(in)    :: s2D_T1(:,:)      !< field for 2nd time

         integer                                   :: ii         !< loop counter
         integer                                   :: m          !< first coordinate index
         integer                                   :: n          !< 2nd coordinate index
         integer                                   :: mp         !< index in flexIndexWeights array
         type(tFlexibleIndexWeightFactor), pointer :: flexIndexW !< helper pointer to current flexIndexWeight
         real(kind=hp)                             :: weight     !< interpolation weight

         mp = indexWeight%indices(1, targetElementID)
         flexIndexW => indexWeight%flexIndexWeights(-mp)
         do ii = 1, size(flexIndexW%weights)
            weight = flexIndexW%weights(ii)
            m = flexIndexW%indices(1,ii)
            n = flexIndexW%indices(2,ii)
            targetValue = targetValue + weight* (a0 * s2D_T0(m, n) + a1 * s2D_T1(m, n))
         enddo
      end subroutine extrapolateValue

      !> update interpolation indices and weigth in case of extrapolation using a kdtree
      function updateInterpolation(kdtree, sourceItem, targetElementSet, targetElementID, field, indexWeight) result(success)
         type(kdtree_instance), intent(inout) :: kdtree           !< kdtree instance
         type(tEcElementSet),   intent(in)    :: targetElementSet !< target element set
         type(tEcItem),         intent(in)    :: sourceItem       !< source item
         integer,               intent(in)    :: targetElementID  !< target element Id
         real(kind=hp),         intent(in)    :: field(:)         !< source field
         type(tEcIndexWeight),  intent(inout) :: indexWeight      !< struct holding all indices and weights
         logical                              :: success          !< function result

         integer, parameter                   :: NN = 10          !< maximum number of results of search with kdtree
         integer                              :: p(NN)            !< resulting m points
         integer                              :: q(NN)            !< resulting n points
         real(kind=hp)                        :: w(NN)            !< resulting interpolation weights

         success = nearest_sample_wrapper(kdtree, sourceItem, targetElementSet, targetElementID, field, p, q, w)
         if (success) then
            success = addExtrapolatedIndicesAndWeights(indexWeight, p, q, w, targetElementID)
         endif
      end function updateInterpolation

end module m_ec_spatial_extrapolation
