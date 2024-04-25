subroutine u_whis(lundat    ,header    ,runid     ,itime     ,idate     , &
                & first     ,kmax      ,lmaxd     ,lstsci    ,ltur      , &
                & lsal      ,ltem      ,lsed      ,istat     ,icross    , &
                & ntru      ,ntruv     ,nostat    ,notim     ,zmodel    , &
                & timnow    ,grdang    ,zalfas    ,zwl       ,zcuru     , &
                & zcurv     ,zcurw     ,ztur      ,zqxk      ,zqyk      , &
                & ztauks    ,ztauet    ,zvicww    ,zdicww    ,zrich     , &
                & zrho      ,zbdsed    ,zrsdeq    ,zdpsed    ,zdps      , &
                & zws       ,gro       ,hydprs    ,atr       ,ctr       , &
                & dtr       ,fltr      ,gdp)
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
! Example user routine to write history data for the computed bed stress term
! at a specific station with sequence number ISTAT to a user defined output file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                           , pointer :: eps
    character(20)  , dimension(:)      , pointer :: namst
!
! Global variables
!
    integer                                                       :: icross
    integer                                          , intent(in) :: idate
    integer                                          , intent(in) :: istat
    integer                                          , intent(in) :: itime
    integer                                          , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in) :: lmaxd  !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: lsal   !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in) :: ltem   !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in) :: lundat
    integer                                          , intent(in) :: nostat !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: notim
    integer                                          , intent(in) :: ntru   !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: ntruv  !  Description and declaration in dimens.igs
    logical                                          , intent(in) :: first
    logical                                          , intent(in) :: zmodel !  Description and declaration in procs.igs
    real(fp)                                         , intent(in) :: grdang !  Description and declaration in tricom.igs
    real(fp)                                         , intent(in) :: timnow
    real(fp)       , dimension(nostat)               , intent(in) :: zalfas !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat)               , intent(in) :: zdps   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat)               , intent(in) :: zdpsed !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat)               , intent(in) :: ztauet !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat)               , intent(in) :: ztauks !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat)               , intent(in) :: zwl    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax)       , intent(in) :: zdicww !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax)       , intent(in) :: zrich  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax)       , intent(in) :: zvicww !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax, lsed) , intent(in) :: zws    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax, ltur) , intent(in) :: ztur   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: hydprs !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zcuru  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zcurv  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zcurw  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zqxk   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zqyk   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zrho   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, lsed)         , intent(in) :: zrsdeq !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax, lstsci) , intent(in) :: gro    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, lsed)         , intent(in) :: zbdsed !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv)                , intent(in) :: ctr    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv)                , intent(in) :: fltr   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv, lstsci)        , intent(in) :: atr    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv, lstsci)        , intent(in) :: dtr    !  Description and declaration in esm_alloc_real.f90
    character(*)                                     , intent(in) :: runid
    character(131) , dimension(10)                                :: header !  Description and declaration in postpr.igs
!
! Local variables
!
    integer :: i
    real(fp):: leps   ! local epsilon (differs from epsilon in const.igs)
    real(fp):: hulp
    real(fp):: ztaud
    real(fp):: ztaum
    real(fp):: ztauut
    real(fp):: ztauvt
!
!! executable statements -------------------------------------------------------
!
    eps       => gdp%gdconst%eps
    namst     => gdp%gdstations%namst
    !
    ! initialisation
    !
    leps = 1.E-12
    ztauut = 0.
    ztauvt = 0.
    ztaud = 0.
    ztaum = 0.
    !
    ! Header; write for the first time
    !
    if (first) then
       header(1) = '* output at station: ' // namst(istat)
       header(2) = '* simulation results from run:' // runid
       header(3) = '* column    1 : date(yyyymmdd)'
       header(4) = '* column    2 : time(dummy)'
       header(5) = '* column    3 : elapsed time (minutes)'
       header(6) = '* column    4 : tau bottom u'
       header(7) = '* column    5 : tau bottom v'
       header(8) = '* column    6 : tau bottom magnitude'
       header(9) = '* column    7 : tau bottom direction'
       header(10) = 'TA01 Bot.friction at ' // namst(istat) // 'run: ' // runid
       do i = 1, 10
          write (lundat, '(  a)') header(i)
       enddo
       write (lundat, '(2i6)') notim, 7
    endif
    !
    ! Transform ZTAUKS and ZTAUET to cartesian coordinate using angle
    ! between sigma grid and cartesian grid in zeta point
    !
    ztauut = ztauks(istat)*cos(zalfas(istat)*degrad) - ztauet(istat)            &
           & *sin(zalfas(istat)*degrad)
    ztauvt = ztauks(istat)*sin(zalfas(istat)*degrad) + ztauet(istat)            &
           & *cos(zalfas(istat)*degrad)
    !
    ! Calculate ZTAUMagnitude and ZTAUDirection
    !
    ztaum = sqrt(ztauut*ztauut + ztauvt*ztauvt)
    if (abs(ztauut)<leps) then
       ztauut = leps
    endif
    if (abs(ztauvt)<leps) then
       ztauvt = leps
    endif
    !
    ! direction in cartesian coordinate system [degrees]:
    !         alpha = atan (v,u) * 180./pi
    ! define angle to y-axis, positive clockwise (east=+90)
    !         beta  = 90. - atan(v,u) * 180./pi
    ! correction for north axis
    !         beta  = beta + grdang
    ! ztaud should be defined between 0. en  360. degrees
    !         atan2 <-180,180>, grdang [0,360] => mod (.. + 360)
    !
    hulp = 90. - atan2(ztauvt, ztauut)*raddeg + grdang
    ztaud = mod(hulp + 360., 360.0_fp)
    !
    ! Write values to output file
    !
    write (lundat, '(1x,i8.8,1x,i6.6,1x,f11.3,3(1x,g13.6),1x,f8.1)') &
        & idate, itime, timnow, ztauut, ztauvt, ztaum, ztaud
end subroutine u_whis
