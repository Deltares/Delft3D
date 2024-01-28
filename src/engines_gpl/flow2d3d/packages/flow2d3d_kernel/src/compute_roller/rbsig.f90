subroutine rbsig(ncmax     ,ampbc     ,ombc      ,phibc     ,thetbc    , &
               & filrol    ,lundia    ,gdp       )
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
! The subroutine reads the input file 'wavcmp' which
! contains the Fourier contributions to the incoming
! (free surface) wave signal at the boundary.
! Frequencies are given in Hz and translated to angular frequencies.
! Angles are given in degrees and translated to rads.
!  Structure of input file
!
! ncomp
! fs(Hz)
! f_1 (Hz)      ampbc_1 (m)      phibc_1 (deg)      thetbc_1 (deg)
!  ...           ...              ...                ...
!  ...           ...              ...                ...
! f_ncomp (Hz)  ampbc_ncomp (m)  phibc_ncomp (deg)  thetbc_ncomp (deg)
! nmskf   nmskl   mmskf   mmskl
! timtap(sec)
!
!
! where it is assumed that f_j<f_p if j<p .
!
! The signal will at the boundary (in (x,y)) be determined by
! the wave components (f,ampbc,phibc,thetbc).
!
! Only the components which have a frequency lower than fs will be
! prescribed at the boundary as free waves. The other components are
! used to determine the incoming high frequency wave energy field which
! will generate forced waves in the model.
!
! nmskf is the grid number in ETA dir BELOW which the wave forces are
! gradually and artificially reduced to zero at the lower boundary.
! nmskl is the grid number in ETA dir ABOVE which the wave forces are
! gradually and artificially reduced to zero at the upper boundary.
! mmskf is the grid number in KSI dir BELOW which the wave forces are
! gradually and artificially reduced to zero at the left  boundary.
! mmskl is the grid number in KSI dir ABOVE which the wave forces are
! gradually and artificially reduced to zero at the right boundary.
! By using -1 for one or more of these four parameters the reduction of
! wave forces does not take place near the corresponding boundary.
!
! timtap is the time in second that is used by the taper for the
! incoming signals.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: nmskf
    integer  , pointer :: nmskl
    integer  , pointer :: mmskf
    integer  , pointer :: mmskl
    integer  , pointer :: ncomp
    integer  , pointer :: nsplit
    real(fp) , pointer :: timtap
    real(fp) , pointer :: depbnd
!
! Global variables
!
    integer               , intent(in)  :: lundia !  Description and declaration in inout.igs
    integer               , intent(in)  :: ncmax
    real(fp), dimension(ncmax), intent(out) :: ampbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)              :: ombc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)              :: phibc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)              :: thetbc !  Description and declaration in esm_alloc_real.f90
    character(*)          , intent(in)  :: filrol
!
! Local variables
!
    integer           :: i
    integer           :: jj
    integer           :: lenc
    integer           :: lfile    ! Length of file name
    integer           :: uw
    integer, external :: newlun
    integer           :: version  ! to detect the version number of the file
    logical           :: ex       ! file existence flag
    real(fp)          :: omspl
    character(35)     :: msg
    character(78)     :: string
!
!! executable statements -------------------------------------------------------
!
    nmskf     => gdp%gdbcdat%nmskf
    nmskl     => gdp%gdbcdat%nmskl
    mmskf     => gdp%gdbcdat%mmskf
    mmskl     => gdp%gdbcdat%mmskl
    ncomp     => gdp%gdbcdat%ncomp
    nsplit    => gdp%gdbcdat%nsplit
    timtap    => gdp%gdbcdat%timtap
    depbnd    => gdp%gdbcdat%depbnd
    !
    call remove_leading_spaces(filrol    ,lfile     )
    !
    inquire (file = filrol(1:lfile), exist = ex)
    if (ex) then
       open (newunit=uw, file = filrol(1:lfile), form = 'formatted',               &
           & status = 'old')
       write (lundia, *)
       write (lundia, '(a)') '*** Wave components input'
       !
       version = 0
       lenc    = 999
       read (uw, '(a)') string
       do while (string(:1)=='*')
          call small(string    ,lenc      )
          i = index(string, 'version')
          if (i/=0) then
             read (string(i + 8:), '(i2)') version
          endif
          read (uw, '(a)') string
       enddo
       if (version < 1) then
          call prterr (lundia,'U021','Version number of wave components file is lower than 1')
          write (lundia, '(a)') '    The wave components file must contain a reference depth'
          call d3stop(1, gdp)
       endif
       msg = 'Version number of file'
       write (lundia, '(a,a,i4)') msg,':', version
       !
       rewind (uw)
       call skipstarlines(uw)
       read (uw, *) ncomp
       msg = 'Number of wave components'
       write (lundia, '(a,a,i4)') msg,':',ncomp
       if (ncomp>ncmax) then
          call prterr (lundia,'U021','# wave components > ncmax')
          call d3stop(1, gdp)
       endif
       !
       read (uw, *) depbnd
       msg = 'Reference depth on open boundary'
       write (lundia, '(a,a,e12.4)') msg,':',depbnd
       !
       read (uw, *) omspl
       msg = 'Split frequency'
       write (lundia, '(a,a,e12.4)') msg,':',omspl
       omspl = 2.0*pi*omspl
       !
       nsplit = 0
       ombc = 0.0_fp
       write (lundia, '(a)') 'component  fbc         ampbc       phibc       thetbc'
       do jj = 1, ncomp
          read (uw, *) ombc(jj), ampbc(jj), phibc(jj), thetbc(jj)
          write (lundia, '(i4,5x,4e12.4)') jj,ombc(jj), ampbc(jj), phibc(jj), thetbc(jj)
          ombc(jj) = 2.0_fp*pi*ombc(jj)
          if (ombc(jj)<omspl) nsplit = nsplit + 1
          if (jj>1) then
             if (comparereal(ombc(jj),ombc(jj-1)) < 0) then
                call prterr (lundia,'U021','wave frequencies must be increasing')
                call d3stop(1, gdp)
             endif
          endif
          phibc(jj) = degrad*phibc(jj)
          thetbc(jj) = degrad*thetbc(jj)
       enddo
       !
       read (uw, *) nmskf, nmskl, mmskf, mmskl
       msg = 'nmskf'
       write (lundia, '(a,a,i4)') msg,':',nmskf
       msg = 'nmskl'
       write (lundia, '(a,a,i4)') msg,':',nmskl
       msg = 'mmskf'
       write (lundia, '(a,a,i4)') msg,':',mmskf
       msg = 'mmskl'
       write (lundia, '(a,a,i4)') msg,':',mmskl
       !
       read (uw, *) timtap
       msg = 'timtap'
       write (lundia, '(a,a,e12.4)') msg,':',timtap
       !
       write (lundia, '(a)') '*** End  of wave components input'
       write (lundia, *)
       close (uw)
    else
       !
       ! file not found
       !
       call prterr(lundia    ,'G004'    ,filrol(1:lfile)    )
    endif
end subroutine rbsig
