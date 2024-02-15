subroutine read_triana(lundia    ,error     ,kc       ,statns    ,nto        ,gdp       )
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
!    Function: compose FLOW boundary conditions from cross ref
!              FLOW support points and TRIANA tidal stations,
!              listed in *.bnd file
!              astronomical phases into FLOW phases,
!              astronomical amplitudes into FLOW amplitudes
! Method used: 1. stations already listed in statns
!              2. read TRIANA file / GETIJSYS files
!              3. determine actual amplitudes and phases
!              4. optionally, read corrections
!              5. copy amplitudes and phases to gdp%gdbcdat
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
    integer     , dimension(:)    , pointer :: pindex
    character(8), dimension(:)    , pointer :: compnames
    real(fp)    , dimension(:,:,:), pointer :: hydrbcf
!
! Global variables
!
    integer                            :: lundia !  Description and declaration in inout.igs
    integer                            :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                            :: kc     !!  Actual number of components used
    logical                            :: error  !!  errorflag
    character(12), dimension(mxnto, 2) :: statns !!  References to tidal stations at ns boundary support points
!
! Local variables
!
    integer                                      :: i                ! Help var. 
    integer                                      :: ii
    integer                                      :: ierrs
    integer                                      :: istat
    integer                                      :: j                ! Help var. 
    integer                                      :: k                ! Help var. 
    integer                                      :: ks 
    integer                                      :: kcmp
    integer                                      :: nosup
    logical                                      :: with_corrections
    real(fp)       , dimension(:,:), allocatable :: ampl             ! Amplitudes 
    real(fp)       , dimension(:,:), allocatable :: phas             ! Phase 
    real(fp)       , dimension(:,:), allocatable :: ampl_corr
    real(fp)       , dimension(:,:), allocatable :: phas_corr
    character(256)                               :: filana           ! Filename of the TRIANA-file or for GETIJSYS filana = ' '
    character(256)                               :: filcor           ! Filename for the correction file
    character(400)                               :: message
!
! External functions
!
    character(8), external                     :: cmpnum
!
!! executable statements -------------------------------------------------------
!
    pindex => gdp%gdbcdat%pindex
    !
    nosup  = nto*2
    ierrs  = 0
    !
    filana = ''
    filcor = ''
    call prop_get(gdp%mdfile_ptr, '*', 'Filana', filana)
    call prop_get(gdp%mdfile_ptr, '*', 'Filcor', filcor)
    with_corrections = (filcor /= ' ')
    !
    ! allocate memory, both for local as global arrays
    !
                                         allocate(gdp%gdbcdat%compnames(      kc), stat=ierrs)
    if (ierrs==0)                        allocate(gdp%gdbcdat%hydrbcf  (4,nto,kc), stat=ierrs)
    if (ierrs==0)                        allocate(            ampl     (nosup,kc), stat=ierrs)
    if (ierrs==0)                        allocate(            phas     (nosup,kc), stat=ierrs)
    if (ierrs==0 .and. with_corrections) allocate(            ampl_corr(nosup,kc), stat=ierrs)
    if (ierrs==0 .and. with_corrections) allocate(            phas_corr(nosup,kc), stat=ierrs)
    if (ierrs/=0) then
       call prterr(lundia, 'U021', 'read_triana: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    compnames => gdp%gdbcdat%compnames
    hydrbcf   => gdp%gdbcdat%hydrbcf
    !
    ampl  = 0.0_fp
    phas  = 0.0_fp
    !
    ! read TRIANA component file, amplitudes and phases at proper position
    !
    call reacmp(lundia    ,error     ,filana    ,statns    ,nto       , &
              & ampl      ,phas      ,0         ,kc        ,gdp       )
    if (error) goto 9999
    !
    ! if correction-file specified, read it like a triana file
    !
    if (with_corrections) then
       ampl_corr = 1.0_fp
       phas_corr = 0.0_fp
       if (filcor == filana) then
          error   = .true.
          message = 'Triana file and Correction file are the same file: ' // filana
          call prterr(lundia, 'U021', message)
          goto 9999
       else
          call reacmp(lundia    ,error     ,filcor    ,statns    ,nto       , &
                    & ampl_corr ,phas_corr ,1         ,kc        ,gdp       )
          if (error) goto 9999
       endif
       !
       ! If a NaN is read -> error
       !
       do j = 1, nosup
          do k = 1, kc
             if ( isnan(ampl(j, k)) .or. isnan(phas(j, k)) ) then 
                write(message,'(2a)') 'NaN found in file ', filana
                call prterr(lundia, 'P004', message)
                !
                error = .true.
                goto 9999
             endif
          enddo
       enddo
       do j = 1, nosup
          do k = 1, kc
             if ( isnan(ampl_corr(j, k)) .or. isnan(phas_corr(j, k)) ) then 
                write(message,'(2a)') 'NaN found in file ', filcor
                call prterr(lundia, 'P004', message)
                !
                error = .true.
                goto 9999
             endif
          enddo
       enddo
       !
       ! adjust amplitudes and phases with corrections
       !
       do j = 1, nosup
          do k = 1, kc
             ampl(j, k) = ampl(j, k) * ampl_corr(j, k)
             phas(j, k) = phas(j, k) + phas_corr(j, k)
          enddo
       enddo
    endif     
    !
    ! keep phases between 0 and 360 degrees
    ! (also done in update_nodal_factors)
    !
    do j = 1, nosup
       !
       ! for all components, except A0
       !
       do k = 2, kc
          if (phas(j, k) > 360.0_fp) then
             phas(j, k) = phas(j, k) - 360.0_fp
          elseif (phas(j, k) < 0.0_fp) then
             phas(j, k) = phas(j, k) + 360.0_fp
          endif
       enddo
    enddo
    !
    ! copy compnames in gdp%gdbcdat%compnames
    !
    compnames(1) = cmpnum(0)
    ii = 2
    do k = 1, mxkc
       if (pindex(k) > 1) then
           compnames(ii) = cmpnum(k)
           ii = ii + 1
           if (ii > kc) exit
       endif
    enddo
    !
    ! copy amplitudes and phases in gdp%gdbcdat%hydrbcf
    !
    ii = 1
    do i = 1, nosup, 2
       do k = 1, kc
          hydrbcf(1, ii, k) = ampl (i  , k)
          hydrbcf(2, ii, k) = ampl (i+1, k)
          hydrbcf(3, ii, k) = phas (i  , k)
          hydrbcf(4, ii, k) = phas (i+1, k)
       enddo
       ii = ii + 1
    enddo
    !
 9999 continue
    deallocate(ampl,stat=istat)
    deallocate(phas,stat=istat)
    if (with_corrections) then
       deallocate(ampl_corr,stat=istat)
       deallocate(phas_corr,stat=istat)
    endif
    deallocate(gdp%gdbcdat%pindex,stat=istat) ! pindex is no longer used
    nullify(pindex)
end subroutine read_triana
