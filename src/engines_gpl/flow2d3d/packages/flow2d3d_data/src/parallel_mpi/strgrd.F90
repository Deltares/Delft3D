subroutine strgrd ( icom, runid, mmax, nmax, mmaxgl, nmaxgl, &
                  & nfg , nlg  , mfg , mlg , gdp   )
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
!
!    Function: Stores array ICOM for own subdomain in scratch file to be later used
! Method used:
!
!!--pseudo code and references--------------------------------------------------
!
!   allocate own array ICOMO
!   put copies of parts of ICOM for each subdomain
!   open scratch file
!   store own array ICOM in scratch file
!   close file
!
!
!!--declarations----------------------------------------------------------------
    use globaldata
    use string_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer, intent(in)                                 :: mfg    ! Description and declaration in dfparall.igs
    integer, intent(in)                                 :: mlg    ! Description and declaration in dfparall.igs
    integer, intent(in)                                 :: mmax   ! Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                 :: mmaxgl ! Description and declaration in dfparall.igs
    integer, intent(in)                                 :: nfg    ! Description and declaration in dfparall.igs
    integer, intent(in)                                 :: nlg    ! Description and declaration in dfparall.igs
    integer, intent(in)                                 :: nmax   ! Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                 :: nmaxgl ! Description and declaration in dfparall.igs
    integer, dimension(-1:mmaxgl+2, nmaxgl), intent(in) :: icom   ! mask array for the water level points in global domain
                                                                  !  = 0 : point is not active
                                                                  ! <> 0 : point is active
    character(*), intent(in)                            :: runid  ! Run identification code for the current
                                                                  ! simulation (used to determine then names
                                                                  ! of the in- /output files used by the system)
!
! Local variables
!
    integer                              :: lfnm   ! actual length of file name
    integer                              :: lrid   ! Length of character string runid
    integer                              :: lungrd ! Unit number of local scratch file for array ICOM
    integer                              :: m      ! loop variable
    integer                              :: n      ! loop variable
    integer                              :: newlun
    integer, dimension(:,:), allocatable :: icomo  ! array ICOM for own subdomain
    character(256)                       :: filnam ! String containing complete file name "TMP_RUNID.extension"
    character(256)                       :: fixid  ! fixed size version of runid, needed for character concatenation
!
!! executable statements -------------------------------------------------------
!
    allocate(icomo(mmax,nmax))
    icomo = 0
    !
    ! put copies of parts of ICOM for each subdomain
    !
    do m = mfg, mlg
       do n = nfg, nlg
          icomo(m-mfg+1,n-nfg+1) = icom(m,n)
       enddo
    enddo
    !
    ! define length of runid and put in fixed size array
    ! size is tested in iniid
    !
    call remove_leading_spaces(runid     ,lrid      )
    fixid(1:lrid) = runid(1:lrid)
    !
    ! open scratch file
    !
    filnam = 'TMP_' // fixid(1:lrid) // '.grd'
    !
    ! append node number to file name
    !
    call remove_leading_spaces(filnam,lfnm)
    write(filnam(lfnm+1:lfnm+4),666) inode
    open (newunit=lungrd, file = trim(filnam), form = 'unformatted', status = 'unknown')
    !
    ! store own array ICOM in scratch file
    !
    write (lungrd) ((icomo(m,n), m=1, mmax), n=1, nmax)
    !
    ! close file
    !
    close (lungrd)
    deallocate(icomo)
    !
666 format('-',i3.3)
end subroutine strgrd
