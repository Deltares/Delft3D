subroutine out2d( kcs , xcor, ycor, mmax, nmax, &
                & xsw , ysw ,mmaxs,nmaxs,       &
                  wght, ref                   )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    !
    implicit none
!
! Global variables
!
    integer                          , intent(in)  :: mmax
    integer                          , intent(in)  :: nmax
    integer                          , intent(in)  :: mmaxs
    integer                          , intent(in)  :: nmaxs
    integer, dimension(mmax,nmax)    , intent(in)  :: kcs
    integer, dimension(4,mmaxs*nmaxs), intent(in)  :: ref
    real(kind=hp)   , dimension(mmax,nmax)    , intent(in)  :: xcor
    real(kind=hp)   , dimension(mmax,nmax)    , intent(in)  :: ycor
    real   , dimension(mmaxs,nmaxs)  , intent(in)  :: xsw
    real   , dimension(mmaxs,nmaxs)  , intent(in)  :: ysw
    real   , dimension(4,mmaxs*nmaxs), intent(in)  :: wght
!
! Local variables
!
    integer               :: i
    integer               :: iswn
    integer               :: m
    integer, dimension(4) :: mm
    integer               :: mn
    integer               :: ms
    integer               :: n
    integer, dimension(4) :: nn
    integer               :: ns
    integer               :: lunfil
!
!! executable statements -------------------------------------------------------
!
    iswn = 0
    !
    open (newunit=lunfil,file='ref_in2.dat')
    !
    do mn = 1, mmaxs*nmaxs
       n = int((mn-1)/mmax) + 1
       m = mn - (n - 1) * mmax
       write (lunfil,'(6i5)') m,n, ref(1,mn),ref(2,mn), ref(3,mn), ref (4,mn)
    enddo
    !
    do ns = 1, nmaxs
       do ms = 1, mmaxs
          iswn = iswn + 1
          do i = 1, 4
             if (ref(i,iswn) > 0) then
                nn(i)  = int ((ref(i,iswn)-1)/mmax) +1
                mm(i)  = ref(i,iswn) - (nn(i) - 1) * mmax
                !
                if (mm(i) > 0 .and. nn(i) > 0) then
                   write (lunfil,'(4i5,2f12.3,i5)') ms,ns,mm(i),nn(i),&
                                                  & xcor(mm(i),nn(i)),&
                                                  & ycor(mm(i),nn(i)),&
                                                  & kcs(mm(i),nn(i))
                endif
             endif
          enddo
       enddo
    enddo
    close(lunfil)
end subroutine out2d
