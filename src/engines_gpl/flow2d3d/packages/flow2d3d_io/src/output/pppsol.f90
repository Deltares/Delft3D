subroutine pppsol(mmax      ,nmax      ,kmax      ,lstsci    ,ltur      , &
                & runid     ,kcu       ,kcv       ,kcs       ,kfu       , &
                & kfv       ,kfs       ,u1        ,v1        ,s1        , &
                & r1        ,rtur1     ,dps       ,gdp       )
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
!    Function: PPPSOL prints the computed solution
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                                      , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in)  :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                               :: runid  !!  Run identification code for the cur-
                                                                                                         !!  rent simulation (used to determine
                                                                                                         !!  the names of the in- /output files
                                                                                                         !!  used by the system)
!
! Local variables
!
    integer           :: ddb
    integer           :: k      ! Help var. 
    integer           :: l      ! Help var. 
    integer           :: lenid  ! Length of runid 
    integer           :: lunsol ! Unit for print file ZSOL 
    integer           :: m      ! Help var. counter for array index in the X-/M-direction 
    integer           :: n      ! Help var. counter for array index in the Y-/N-direction 
    integer, external :: newlun
    character(300)    :: filnam ! Name of file to write to 
!
!! executable statements -------------------------------------------------------
!
    ddb = gdp%d%ddbound
    call remove_leading_spaces(runid     ,lenid     )
    filnam = 'tstprt.' // runid(:lenid)
    open (newunit=lunsol, file = filnam)
    !
    ! print water elevation
    !
    write (lunsol, *) mmax, nmax, kmax, lstsci, ltur
    write (lunsol, *) '   '
    write (lunsol, *) ' Water elevation'
    write (lunsol, *) '  m   n    value'
    do m = 1 - ddb, mmax
       do n = 1 - ddb, nmax
          if (kcs(n, m)/=0) &
           & write (lunsol, '(2i4,f10.6,f18.2)') m, n, s1(n, m), real(dps(n, m),fp)
       enddo
    enddo
    write (lunsol, *) '  0  0  0'
    !
    ! print u-velocity
    !
    write (lunsol, *) '   '
    write (lunsol, *) ' U-velocity'
    write (lunsol, *) '  m   n  value(1:kmax)'
    do m = 1 - ddb, mmax
       do n = 1 - ddb, nmax
          if (kcu(n, m)/=0) &
           & write (lunsol, '(2i4,100f10.6)') m, n, (u1(n, m, k), k = 1, kmax)
       enddo
    enddo
    write (lunsol, '(''  0  0'',100f10.6)') (u1(1, 1, k), k = 1, kmax)
    !
    ! print v-velocity
    !
    write (lunsol, *) '   '
    write (lunsol, *) ' V-velocity'
    write (lunsol, *) '  m   n  value(1:kmax)'
    do m = 1 - ddb, mmax
       do n = 1 - ddb, nmax
          if (kcv(n, m)/=0) &
           & write (lunsol, '(2i4,100f10.6)') m, n, (v1(n, m, k), k = 1, kmax)
       enddo
    enddo
    write (lunsol, '(''  0  0'',100f10.6)') (v1(1, 1, k), k = 1, kmax)
    !
    ! print transport
    !
    if (lstsci>0) then
       write (lunsol, *) '   '
       write (lunsol, *) ' Concentrations'
       do l = 1, lstsci
          write (lunsol, *) ' Constituent no. ', l
          write (lunsol, *) '  m   n    value(1:kmax)'
          do m = 1 - ddb, mmax
             do n = 1 - ddb, nmax
                if (kcs(n, m)/=0) &
                 & write (lunsol, '(2i4,100f10.6)') m, n, (r1(n, m, k, l), k = 1,  &
                 & kmax)
             enddo
          enddo
          write (lunsol, '(''  0  0'',100f10.6)') (r1(1, 1, k, l), k = 1, kmax)
       enddo
    endif
    !
    ! print turbulence
    !
    if (ltur>0) then
       write (lunsol, *) '   '
       write (lunsol, *) ' Turbulence'
       do l = 1, ltur
          write (lunsol, *) ' Turbulence no. ', l
          write (lunsol, *) '  m   n    value(0:kmax)'
          do m = 1 - ddb, mmax
             do n = 1 - ddb, nmax
                if (kcs(n, m)/=0) &
                 & write (lunsol, '(2i4,100f10.6)') m, n, (rtur1(n, m, k, l), k = 0,  &
                 & kmax)
             enddo
          enddo
          write (lunsol, '(''  0  0'',100f10.6)') &
              & (rtur1(1, 1, k, l), k = 0, kmax)
       enddo
    endif
    close (lunsol)
end subroutine pppsol
