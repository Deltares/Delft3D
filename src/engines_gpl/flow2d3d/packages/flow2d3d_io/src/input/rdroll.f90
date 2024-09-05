subroutine rdroll(lunmd     ,lundia    ,lunscr    ,lerror    ,nrrec     , &
                & mdfrec    ,wavcmp    ,filrol    ,ncmax     ,gdp       )
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
! - Reads the roller parameters from the MD-file :
! alfarol   - default  1.0
! betarol   - default  0.1
! gamdis    - default  0.55
! ndis      - default 10
! thr       - default  0.01
! f_lam     - default  0.0
! fwee      - default  0.0
! disform   - default  'R1993'
! wavfrc    - default  .true.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer       , pointer :: itis
    integer       , pointer :: ndis
    integer       , pointer :: rolcorr
    real(fp)      , pointer :: alfarol
    real(fp)      , pointer :: betarol
    real(fp)      , pointer :: gamdis
    real(fp)      , pointer :: thr
    real(fp)      , pointer :: f_lam
    real(fp)      , pointer :: fwee
    logical       , pointer :: wavfrc
    character(5)  , pointer :: disform
!
! Global variables
!
    integer                  :: lundia !  Description and declaration in inout.igs
    integer                  :: lunmd  !  Description and declaration in inout.igs
    integer     , intent(in) :: lunscr !  Description and declaration in inout.igs
    integer     , intent(in) :: ncmax
    integer                  :: nrrec
    logical                  :: lerror
    logical     , intent(in) :: wavcmp
    character(*)             :: filrol
    character(*)             :: mdfrec
!
! Local variables
!
    character(43)                :: txtput
    character(6)                 :: keyw
!
!! executable statements -------------------------------------------------------
!
    itis     => gdp%gdrdpara%itis
    ndis     => gdp%gdbetaro%ndis
    rolcorr  => gdp%gdbetaro%rolcorr
    alfarol  => gdp%gdbetaro%alfarol
    betarol  => gdp%gdbetaro%betarol
    gamdis   => gdp%gdbetaro%gamdis
    thr      => gdp%gdbetaro%thr
    f_lam    => gdp%gdbetaro%f_lam
    disform  => gdp%gdbetaro%disform
    fwee     => gdp%gdbetaro%fwee
    wavfrc   => gdp%gdbetaro%wavfrc
    !
    ! locate 'Alfaro'  record
    !
    alfarol = 1.0_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Alfaro', alfarol)
    !
    ! locate 'Betaro' record
    !
    betarol = 0.1_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Betaro', betarol)
    !
    ! locate 'Gamdis' record
    !
    gamdis  = 0.55_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Gamdis', gamdis)
    !
    ! locate 'Thr' record
    !
    thr = 0.01_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Thr', thr)
    !
    ! locate 'Ndis' record
    !
    ndis = 10
    call prop_get(gdp%mdfile_ptr, '*', 'Ndis', ndis)
    !
    ! Roller input file specified?
    !
    if (wavcmp) then
       filrol = ' '
       call prop_get(gdp%mdfile_ptr, '*', 'Filwcm', filrol)
       if (filrol == ' ') then
          !
          ! This is really an error: the keyword did exist in dimpro since
          ! otherwise wavcmp would not be set.
          !
          call prterr(lundia, 'P004', 'Roller functionality switched on but keyword "filrol" not found')
          call d3stop(1, gdp)
       endif
    endif
    !
    ! locate 'F_lam' record
    !
    f_lam = 0.0_fp
    call prop_get(gdp%mdfile_ptr, '*', 'F_lam', f_lam)
    !
    ! locate 'Fwee' record
    !
    fwee = 0.0_fp 
    call prop_get(gdp%mdfile_ptr, '*', 'Fwee', fwee)
    !
    ! locate 'Disform' record
    !
    disform = 'R1993'
    call prop_get(gdp%mdfile_ptr, '*', 'Disform', disform)
    !
    ! locate 'Wavrfc' record
    !
    wavfrc = .true.
    call prop_get(gdp%mdfile_ptr, '*', 'Wavfrc', wavfrc)
    !
    ! locate 'RolCor' record
    ! rolcorr is already initialized in dimpro
    !
    call prop_get(gdp%mdfile_ptr, '*', 'RolCor', rolcorr)
    !
    ! output values to file
    !
    write (lundia, '(//,2a)') '*** Input (or default) parameters for ',        &
                              & 'roller model ***'
    txtput = 'Alpharol'
    write (lundia, '(2a,f10.4)') txtput, ':', alfarol
    txtput = 'Betarol'
    write (lundia, '(2a,f10.4)') txtput, ':', betarol
    txtput = 'Gamdis'
    write (lundia, '(2a,f10.4)') txtput, ':', gamdis
    txtput = 'Thr'
    write (lundia, '(2a,f10.4)') txtput, ':', thr
    txtput = 'Ndis'
    write (lundia, '(2a,i5)')    txtput, ':', ndis
    txtput = 'F_lam'
    write (lundia, '(2a,f10.4)') txtput, ':', f_lam
    txtput = 'Fwee'
    write (lundia, '(2a,f10.4)') txtput, ':', fwee
    txtput = 'Disform'
    write (lundia, '(3a)')       txtput, ':', disform
    if (wavfrc) then
       txtput = 'Wavfrc'
       write (lundia, '(3a)')       txtput, ':    ', 'YES'
    else
       txtput = 'Wavfrc'
       write (lundia, '(3a)')       txtput, ':    ', 'NO'
    endif
    txtput = 'RolCor'
    if (rolcorr==0) then
       write (lundia, '(3a)')       txtput, ':    ', 'NO'
    elseif (rolcorr==1) then
       write (lundia, '(3a)')       txtput, ':    ', 'Uniform over water column'
    elseif (rolcorr==2) then
       write (lundia, '(3a)')       txtput, ':    ', 'Using Eulerian velocities'
    endif
    if (wavcmp) then
       txtput = 'File containing wave components '
       write (lundia, '(3a)')    txtput, ':', filrol
       txtput = 'Number of wave components '
       write (lundia, '(2a,i5)') txtput, ':', ncmax
    else
       txtput = 'No wave components imposed at boundary'
       write (lundia, '(a)')     txtput
    endif
    call prterr(lundia, 'U190', 'Background viscosity is added to the Roller contribution (since version 3.50.09.01)')
end subroutine rdroll
