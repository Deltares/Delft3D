subroutine wetdis(i         ,isrc      ,jsrc      ,dpd       ,xcor      , &
                & ycor      ,kcu       ,kcv       ,kfs       ,kfd       , &
                & j         ,nmmaxj    ,icx       ,icy       ,gdp       )
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
! NONE
!
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
    integer , pointer :: lundia
!
! Global variables
!
    integer                                   , intent(in) :: i      !!  discharge number
    integer                                   , intent(in) :: icx    !!  increment in the x-dir., if icx= nmax
                                                                     !!  then computation proceeds in the x-
                                                                     !!  dir. if icx=1 then computation pro-
                                                                     !!  ceeds in the y-dir.
    integer                                   , intent(in) :: icy    !!  increment in the y-dir. (see icx)
    integer                                                :: isrc   !!  m-index of discharge
    integer                                   , intent(in) :: j      !!  begin pointer for arrays which have
                                                                     !!  been transformed into 1d arrays.
                                                                     !!  due to the shift in the 2nd (m-)
                                                                     !!  index, j = -2*nmax + 1
    integer                                                :: jsrc   !!  n-index of discharge
    integer                                   , intent(in) :: nmmaxj !  Description and declaration in dimens.igs
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfd    !!  mask array for the zeta points
                                                                     !!  =0 not yet used for searching
                                                                     !!  =1 already used
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: dpd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: ycor   !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: ddb
    integer                        :: icxy
    integer                        :: iincr ! increment of m-index 
    integer                        :: istep
    integer                        :: jincr ! increment of n-index  
    integer                        :: ndm
    integer                        :: ndmd
    integer                        :: nisrc ! new m-index of discharge location 
    integer                        :: njsrc ! new n-index of discharge location 
    integer                        :: nm
    integer                        :: nmd
    integer                        :: nmu
    integer                        :: num
    real(fp)                       :: dp1
    real(fp)                       :: dp2
    real(fp)                       :: dp3
    real(fp)                       :: dp4
    real(fp)                       :: dpm   ! maxium depth at velocity points 
    real(fp)                       :: x
    real(fp)                       :: xt
    real(fp)                       :: y
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    lundia  => gdp%gdinout%lundia
    !
    ddb = gdp%d%ddbound
    istep = 0
    nisrc = isrc
    njsrc = jsrc
    icxy = max(icx, icy)
    do nm = j, nmmaxj
       kfd(nm) = 0
    enddo
    !
    ! start search procedure
    !
    !
    ! determine depth at surrounding velocity points
    !
  100 continue
    nm = njsrc + ddb + ((nisrc + ddb) - 1)*icxy
    nmu = nm + icx
    nmd = nm - icx
    num = nm + icy
    ndm = nm - icy
    ndmd = nm - icx - icy
    iincr = 0
    jincr = 0
    dpm = -999.999
    kfd(nm) = 1
    x = 0.25_fp * (xcor(nm) + xcor(nmd) + xcor(ndmd) + xcor(ndm))
    y = 0.25_fp * (ycor(nm) + ycor(nmd) + ycor(ndmd) + ycor(ndm))
    if (icx==1) then
       xt = x
       x = y
       y = xt
    endif
    istep = istep + 1
    !     write(lundia,'(5x,3i5,2(1pe11.3))') istep,nisrc,njsrc,x,y
    !
    dp1 = 0.5_fp * (dpd(ndmd) + dpd(nmd))
    dp2 = 0.5_fp * (dpd(ndm ) + dpd(nm ))
    dp3 = 0.5_fp * (dpd(ndmd) + dpd(ndm))
    dp4 = 0.5_fp * (dpd(nmd ) + dpd(nm ))
    !
    ! find greatest depth
    !
    if (dp1>dpm .and. kcu(nmd)>0 .and. kfd(nmd)==0) then
       dpm = dp1
       iincr = -1
       jincr = 0
    endif
    if (dp2>dpm .and. kcu(nm)>0 .and. kfd(nmu)==0) then
       dpm = dp2
       iincr = 1
       jincr = 0
    endif
    if (dp3>dpm .and. kcv(ndm)>0 .and. kfd(ndm)==0) then
       dpm = dp3
       iincr = 0
       jincr = -1
    endif
    if (dp4>dpm .and. kcv(nm)>0 .and. kfd(num)==0) then
       dpm = dp4
       iincr = 0
       jincr = +1
    endif
    !
    ! if there is no change in discharge location, then generate warning
    !
    if (iincr==0 .and. jincr==0) then
       write (lundia, '(a,i4)') '*** WARNING: No new wet discharge location found for discharge ', i
       goto 999
    endif
    !
    ! change discharge location
    !
    if     (icy == 1) then
       nisrc = nisrc + iincr
       njsrc = njsrc + jincr
    elseif (icx == 1) then
       nisrc = nisrc + jincr
       njsrc = njsrc + iincr
    endif
    !
    ! if new location is dry-point then start a new search procedure
    !
    nm = njsrc + ddb + ((nisrc + ddb) - 1)*icxy
    if (kfs(nm) == 0) goto 100
    !
    ! reset discharge location
    !
    isrc = nisrc
    jsrc = njsrc
    !
  999 continue
    nm = jsrc + ddb + ((isrc + ddb) - 1)*icxy
    nmd = nm - icx
    ndm = nm - icy
    ndmd = nm - icx - icy
    x = 0.25_fp * (xcor(nm) + xcor(nmd) + xcor(ndmd) + xcor(ndm))
    y = 0.25_fp * (ycor(nm) + ycor(nmd) + ycor(ndmd) + ycor(ndm))
    if (icx == 1) then
       xt = x
       x  = y
       y  = xt
    endif
    !
    istep = istep + 1
end subroutine wetdis
