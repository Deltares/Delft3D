subroutine wrcomi(comfil    ,lundia    ,error     ,zmodel    ,mmax      , &
                & nmax      ,kmax      ,nmaxus    ,nsrc      ,norow     , &
                & nocol     ,noroco    ,nto       ,nrob      ,zbot      , &
                & dt        ,nfltyp    ,tscale                          , &
                & itlen     ,it01      ,it02      ,tzone                , &
                & rouflo    ,xcor      ,ycor      ,guu                  , &
                & gvv       ,guv       ,gvu       ,gsqs      ,gsqd      , &
                & alfas     ,thick     ,zk        ,namsrc    ,mnksrc    , &
                & xyzsrc    ,irocol    ,mnbnd     ,nob       ,kcu       , &
                & kcv       ,kcs       ,dp        ,dps       ,cfurou    , &
                & cfvrou    ,ibuff     ,rbuff     ,rbuffz    ,sferic    , &
                & gdp       )
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
!
! Method used:
!
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
!
! Global variables
!
    integer                                                             :: it01   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: it02   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: lundia !  Description and declaration in inout.igs
    integer                                                             :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nfltyp !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: noroco !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                             :: nto    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(7, nto)                                       :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(5, noroco)                                    :: irocol !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(7, nsrc)                                      :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(8, nrob)                                      :: nob    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nmaxus, mmax)                                 :: ibuff  !  Description and declaration in esm_alloc_int.f90
    logical                                                             :: error  !!  Flag=TRUE if an error is encountered
    logical                                                             :: zmodel !  Description and declaration in procs.igs
    logical                                                             :: sferic
    real(fp)                                                            :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                            :: tscale !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                            :: tzone  !  Description and declaration in exttim.igs
    real(fp)                                                            :: zbot   !  Description and declaration in zmodel.igs
    real(fp)  , dimension(3, nsrc)                                      :: xyzsrc !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: dp     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: gsqd   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)  :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)  :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                         :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax)                                       :: zk     !!  Vertical coordinates of cell interfaces
                                                                                  !!  Flag for activation of Z-MODEL
    real(fp)  , dimension(nmaxus, mmax)                                 :: rbuff  !  Description and declaration in r-i-ch.igs
    real(fp)  , dimension(kmax+1)                                       :: rbuffz !  Description and declaration in r-i-ch.igs
    character(*)                                                        :: comfil !!  Name for communication file
    character(20), dimension(nsrc)                                      :: namsrc !  Description and declaration in esm_alloc_char.f90
    character(4)                                                        :: rouflo !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Write group PARAMS
    !
    call wrparm(comfil    ,lundia    ,error     , &
              & dt        ,nfltyp    ,tscale    ,itlen     ,it01      , &
              & it02      ,tzone     ,gdp       )
    if (error) goto 9999
    !
    ! Write group GRID
    !
    call wrgrid(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
              & kmax      ,nmaxus    , &
              & xcor      ,ycor      ,guu       ,gvv       ,guv       , &
              & gvu       ,gsqs      ,gsqd      ,alfas     ,thick     , &
              & rbuff     ,rbuffz    ,sferic    ,zmodel    ,zbot      , &
              & zk        ,gdp       )
    if (error) goto 9999
    !
    ! Write group SPECPOINTS
    !
    call wrspcp(comfil    ,lundia    ,error     ,nsrc      ,namsrc    , &
              & mnksrc    ,xyzsrc    ,gdp       )
    if (error) goto 9999
    !
    ! Write group BOUNDCNST
    !
    call wrboun(comfil    ,lundia    ,error     ,norow     ,nocol     , &
              & noroco    ,nrob      ,nto       ,irocol    ,mnbnd     , &
              & nob       ,gdp       )
    if (error) goto 9999
    !
    ! Write group KENMCNST
    !
    call wrkenc(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
              & nmaxus    ,kcu       ,kcv       ,kcs       ,ibuff     , &
              & gdp       )
    if (error) goto 9999
    !
    ! Write group INITBOT
    !
    call wribot(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
              & nmaxus    ,dp        ,dps       ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    ! Write group ROUGHNESS
    !
    call wrrouf(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
              & nmaxus    ,rouflo    ,cfurou    ,cfvrou    ,rbuff     , &
              & gdp       )
    if (error) then
    endif
 9999 continue
end subroutine wrcomi
