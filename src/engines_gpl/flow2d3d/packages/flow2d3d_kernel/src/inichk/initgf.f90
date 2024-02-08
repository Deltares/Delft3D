subroutine initgf(tgfcmp    ,gdp       )
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
!    Function:
!       - Determine tgfnam, tgftyp and tgfcoe as a function of tgfcmp
! Method used:
!       - note: no longer use of TMP file and subroutine tidgen
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, dimension(:)     , pointer :: tgftyp
    integer                   , pointer :: nrcmp
    character*8, dimension(:) , pointer :: tgfdef
    character*8, dimension(:) , pointer :: tgfnam
    real(fp)   , dimension(:) , pointer :: tgfcoe
    real(fp)   , dimension(:) , pointer :: tgfcon
!
! Global variables
!
    character(36), intent(in)      :: tgfcmp !  Description and declaration in tricom.igs
!
!
! Local variables
!
    integer                        :: icmp    ! Loop counter over MXCMP 
    integer                        :: kbeg    ! Start character in string TGFCMP for component ICMP 
    integer                        :: kend    ! Stop character in string TGFCMP for component ICMP 
 !
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !     GLOBAL DATA INITIALISATION
    tgftyp    => gdp%gdtfzeta%tgftyp
    nrcmp     => gdp%gdtfzeta%nrcmp
    tgfdef    => gdp%gdtfzeta%tgfdef
    tgfnam    => gdp%gdtfzeta%tgfnam
    tgfcoe    => gdp%gdtfzeta%tgfcoe
    tgfcon    => gdp%gdtfzeta%tgfcon
    include 'tfzeta.gdt'
    !
    !-----Determine tgftyp, tgfnam and tgfcoe
    !
    nrcmp = 0
    tgfnam(0) = ' '
    do icmp = 1, mxcmp
       kbeg = (icmp - 1)*3 + 1
       kend = icmp*3
       if (tgfcmp(kbeg:kend)/='---') then
          !
          !-----------First 4 components (M2, S2, N2, K2) are for semi diurnal
          !                   tide:   => tgftyp = 2
          !-----------Next 4 components (K1, O1, P1, Q1) are for diurnal tide:
          !                           => tgftyp = 1
          !-----------Last 3 components (Mf, Mm, Ssa) are for long Period tide:
          !                           => tgftyp = 0
          !
          nrcmp = nrcmp + 1
          tgftyp(nrcmp) = 2 - (icmp - 1)/4
          tgfnam(nrcmp) = tgfdef(icmp)(1:3)
          tgfcoe(nrcmp) = tgfcon(icmp)
       endif
    enddo
    !
end subroutine initgf
