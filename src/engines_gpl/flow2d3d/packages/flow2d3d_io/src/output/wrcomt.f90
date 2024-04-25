subroutine wrcomt(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
                & itimc     ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                & nsrc      ,mnksrc    ,lstsci    ,lsal      ,ltem      , &
                & lsecfl    ,kfu       ,kfv       ,ibuff     ,s1        , &
                & u1        ,v1        ,qu        ,qv        ,taubmx    , &
                & r1        ,dicuv     ,dicww     ,discum    ,windu     , &
                & windv     ,dzu1      ,dzv1      ,kmaxz     ,hu        , &
                & hv        ,thick     ,gdp       )
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
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical , pointer :: wind
!
! Global variables
!
    integer                                                                     :: itcur  !!  Current time counter for the com-
                                                                                          !!  munication file, where starting
                                                                                          !!  point depend on CYCLIC
    integer                                                                     :: itimc  !!  Current time step counter for 2D
                                                                                          !!  system
    integer                                                                     :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: kmaxz  !!  = KMAX for Z-model, = 0 for sigma-model
                                                                                          !!  Needed for correct dimensioning of DZU1 and DZV1
    integer                                                                     :: lsal   !  Description and declaration in dimens.igs
    integer                                                                     :: lsecfl !  Description and declaration in dimens.igs
    integer                                                                     :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: ltem   !  Description and declaration in dimens.igs
    integer                                                                     :: lundia !  Description and declaration in inout.igs
    integer                                                                     :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: ntcur  !!  Total number of timesteps on com-
                                                                                          !!  munication file (to write to)
    integer , dimension(7, nsrc)                                                :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(nmaxus, mmax)                                           :: ibuff  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: taubmx !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2)     :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxz)        :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxz)        :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: qu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: qv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: windu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: windv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                                   :: discum !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                   :: thick  !  Description and declaration in esm_alloc_real.f90
    logical                                                                     :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                                :: comfil !!  Name for communication file
                                                                                          !!  com-<case><label>
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    wind       => gdp%gdprocs%wind
    !
    ! Write groups KENMNT and KENMTIM
    !
    call wrkent(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
              & itimc     ,mmax      ,nmax      ,nmaxus    ,kfu       , &
              & kfv       ,ibuff     ,gdp       )
    if (error) goto 9999
    !
    ! Write groups CURNT and CURTIM
    !
    call wrcurt(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
              & itimc     ,mmax      ,nmax      ,kmax      ,nmaxus    , &
              & lstsci    ,lsecfl    ,s1        ,u1        ,v1        , &
              & r1        ,qu        ,qv        ,dzu1      ,dzv1      , &
              & kmaxz     ,hu        ,hv        ,thick     ,gdp       )
    !
    if (error) goto 9999
    !
    ! Write group DWQTIM
    !
    call wrdwqt(comfil    ,lundia    ,error     ,itcur     ,itimc     , &
              & nsrc      ,mnksrc    ,mmax      ,nmax      ,kmax      , &
              & nmaxus    ,lstsci    ,lsal      ,ltem      ,r1        , &
              & dicuv     ,dicww     ,discum    ,taubmx    ,gdp       )
    if (error) goto 9999
    !
    !
    ! Write group WIND
    !
    if (wind) then
       call wrcomwind(error     ,comfil    ,itcur     ,itimc     , &
                    & mmax      ,nmaxus    , &
                    & windu     ,windv     ,gdp       )
       if (error) goto 9999
    endif
 9999 continue
end subroutine wrcomt
