!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

module m_monitoring_runupgauges
   use m_crspath
   use m_missing
   use MessageHandling, only: IdLen

   implicit none

   type trug
      character(len=IdLen)                 :: name          !< Name
      type(tcrspath)                       :: path          !< Polyline+crossed flow links that defines this runup gauge.
      double precision                     :: maxx          !< flow node xz of max runup
      double precision                     :: maxy          !< flow node xy of max runup
      double precision                     :: maxruh        !< Runup water level
   end type trug

    type (trug), allocatable               :: rug(:)
    integer                                :: nrug = 0
    integer                                :: maxrug = 2
    character(len=*), parameter, private   :: defaultName_ = 'Rug'
    integer, private                       :: iUniq_ = 1

   contains

!> Increases memory for crs
subroutine increaseRunupGauges(n)
    integer, intent(in) :: n !< Desired number of rugs.

    type(trug), allocatable :: crst(:) ! Temp storage

    if (n < maxrug .and. allocated(rug)) then
        return
    end if

    call allocRunupGauges(crst, maxrug)

    if (n > maxrug) then
        maxrug    = max(maxrug, int(1.2*n))
    end if

    if (allocated(rug)) then
       call copyRunupGauges(rug, crst)
    end if
    call allocRunupGauges(rug, maxrug)
    call copyRunupGauges(crst, rug)

    call deallocRunupGauges(crst)

end subroutine increaseRunupGauges

!> Allocates an array of runup gauges, deallocating any existing memory.
subroutine allocRunupGauges(cs, n)

   implicit none

    type(trug), allocatable, intent(inout) :: cs(:)   !< Array of gauges
    integer,                 intent(in)    :: n       !< Desired nr of gauges

    call deallocRunupGauges(cs)
    allocate(cs(n))

end subroutine allocRunupGauges

!> Deallocates an array of rugs
subroutine deallocRunupGauges(cs)
    type(trug), allocatable, intent(inout) :: cs(:)

    integer :: i, n

    if (.not. allocated(cs)) return

    n = size(cs)
    do i=1,n
        call deallocCrossSectionPath(cs(i)%path)
    end do
    deallocate(cs)
end subroutine deallocRunupGauges

!> Copies array of rug into another array of rug.
subroutine copyRunupGauges(rfrom, rto)
use m_alloc
    type(trug), intent(inout) :: rfrom(:)
    type(trug), intent(inout) :: rto(:)

    integer :: i, n

    n = size(rfrom)
    if (n > size(rto) .or. n == 0) return

    do i=1,n
       rto(i) = rfrom(i)
    end do
end subroutine copyRunupGauges


subroutine delRunupGauges()
    nrug = 0
end subroutine delRunupGauges

!> Converts a set of polylines into runup gauges.
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_runupgauges(xpl, ypl, npl, names)
    use m_missing

    double precision, intent(in)           :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in)           :: npl            !< Total number of polyline points
    character(len=*), optional, intent(in) :: names(:)       !< Optional names for cross sections

    integer :: i, i1, i2, ic, numnam
    character(len=IdLen) :: name

    if (present(names)) then
        numnam = size(names)
    else
        numnam = 0
    end if

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    ic = 0 ! Nr of polylines found so far
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                ! 1: Special name for this rug or not?
                ic = ic + 1
                if (ic <= numnam) then
                    name = names(ic)
                else
                    name = ' '
                end if

                ! 2: add the current polyline as a new rug.
                call addRunupgauges(name, xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_runupgauges


!> Reads observation rug and adds them to the normal rug adm
subroutine loadRunupGauges(filename, jadoorladen)
   use unstruc_messages

   implicit none
   character(len=*), intent(in   ) :: filename    !< File containing the observation rug. Either a *_rug.pli.
   integer,          intent(in   ) :: jadoorladen !< Append to existing observation rug or not

   logical :: jawel
   integer :: tok_pli

   inquire(file = filename, exist = jawel)
   if (jawel) then
      if (jadoorladen == 0) then
         call delRunupGauges()
      end if
      tok_pli = index(filename, '_rug.pli')
      if (tok_pli > 0) then
         call loadRunupGauges_from_pli(filename)
      else
         call mess(LEVEL_WARN, "Runup gauge file ('"//trim(filename)//"') does not end with _rug.pli.")
      end if
   else
       call mess(LEVEL_ERROR, "Runup gauge file '"//trim(filename)//"' not found!")
   endif
end subroutine loadRunupGauges

!> Reads rugs from an *.pli file.
subroutine loadRunupGauges_from_pli(filename)
   use messageHandling
   use dfm_error
   use m_polygon
   implicit none
   character(len=*), intent(in) :: filename

   integer :: minp, ipli

   call oldfil(minp, filename)
   ipli = 0
   call reapol_nampli(minp, 0, 1, ipli)
   call pol_to_runupgauges(xpl, ypl, npl, names=nampli)
   call doclose(minp)

end subroutine loadRunupGauges_from_pli


subroutine addRunupgauges(name, xp, yp)
    character(len=*), intent(in) :: name
    double precision, intent(in) :: xp(:), yp(:)

    integer :: m
    character(len=1) :: cdigits

    call increaseRunupgauges(nrug+1)

    nrug           = nrug + 1
    call setCrossSectionPathPolyline(rug(nrug)%path, xp, yp)
    rug(nrug)%path%lnx  = 0

    ! Set name (or generate one)
    m = len_trim(name)
    if (m > 0) then
        m = min(len(rug(nrug)%name), len(name))
        rug(nrug)%name = ' '
        rug(nrug)%name(1:m) = name(1:m)
    else ! No name given, generate one.
        write(cdigits, '(i1)') max(2, int(floor(log10(dble(iUniq_))+1)))
        write(rug(nrug)%name, '(a,i'//cdigits//'.'//cdigits//')'), trim(defaultName_), iUniq_
        iUniq_ = iUniq_ + 1
    end if

    ! Set default values
    rug(nrug)%maxx = 0d0
    rug(nrug)%maxy = 0d0
    rug(nrug)%maxruh = -huge(0d0)

end subroutine addRunupgauges

subroutine clearRunupGauges()
   integer :: i
   ! Reset data for next iteration
   do i=1, nrug
      rug(i)%maxruh = -huge(0d0)
      rug(i)%maxx   = 0d0
      rug(i)%maxy   = 0d0
   enddo
end subroutine

end module
