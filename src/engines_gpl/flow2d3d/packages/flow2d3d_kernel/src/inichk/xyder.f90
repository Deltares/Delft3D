subroutine xyder(j         ,nmmaxj    ,nmmax     ,icx       ,icy       , &
               & xcor      ,ycor      ,kcs       ,x3        ,x2y       , &
               & xy2       ,y3        ,gdp       )
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
!    Function: Computes the coefficients for the curvature
!              equation for streaklines
!              derived by dr. h.f.p. van den boogaard
! Method used:
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
    logical                             , pointer :: sferic
!
! Global variables
!
    integer, intent(in)            :: icx
                                   !!  Increment in the x-dir., if icx= nmax
                                   !!  then computation proceeds in the x-
                                   !!  dir. if icx=1 then computation pro-
                                   !!  ceeds in the y-dir.
    integer, intent(in)            :: icy
                                   !!  Increment in the y-dir. (see icx)
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1d arrays.
                                   !!  due to the shift in the 2nd (m-)
                                   !!  index, j = -2*nmax + 1
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: x2y !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: x3 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xcor !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: xy2 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: y3 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: ycor !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: kcsd
    integer                        :: kcsu
    integer                        :: kenm
    integer                        :: nddm                 ! NM - ICY-ICY
    integer                        :: nddmd                ! NM - ICY-ICY-ICX
    integer                        :: ndm                  ! NM - ICY
    integer                        :: ndmd                 ! NM - ICY-ICX
    integer                        :: ndmdd                ! NM - ICY-ICX-ICX
    integer                        :: ndmu                 ! NM - ICY+ICX
    integer                        :: nm                   ! Loop parameter 1,NMMAX
    integer                        :: nmd                  ! NM - ICX
    integer                        :: nmdd                 ! NM - ICX-ICX
    integer                        :: nmu                  ! NM + ICX
    integer                        :: num                  ! NM + ICY
    integer                        :: numd                 ! NM + ICY-ICX
    real(fp)                       :: x01                  ! D(xcor)/deta
    real(fp)                       :: x02                  ! D^2(xcor)/(deta)^2
    real(fp)                       :: x10                  ! D(xcor)/dksi
    real(fp)                       :: x11                  ! D^2(xcor)/(dksi deta)
    real(fp)                       :: x20                  ! D^2(xcor)/(dksi)^2
    real(fp)                       :: y01                  ! D(ycor)/deta
    real(fp)                       :: y02                  ! D^2(ycor)/(deta)^2
    real(fp)                       :: y10                  ! D(ycor)/dksi
    real(fp)                       :: y11                  ! D^2(ycor)/(dksi deta)
    real(fp)                       :: y20                  ! D^2(ycor)/(dksi)^2
    !
    real(fp) :: xxu
    real(fp) :: xxv
    real(fp) :: getdx ! distance function
    real(fp) :: getdy ! distance function
!
!
!! executable statements -------------------------------------------------------
!
    sferic              => gdp%gdtricom%sferic
    !
    nmdd = -icx - icx
    nmd = -icx
    nmu = icx
    num = icy
    numd = icy - icx
    ndmu = -icy + icx
    ndm = -icy
    ndmd = -icy - icx
    ndmdd = -icy - icx - icx
    nddm = -icy - icy
    nddmd = -icy - icy - icx
    do nm = 1, nmmax
       nmdd = nmdd + 1
       nmd = nmd + 1
       nmu = nmu + 1
       num = num + 1
       numd = numd + 1
       ndmu = ndmu + 1
       ndm = ndm + 1
       ndmd = ndmd + 1
       ndmdd = ndmdd + 1
       nddm = nddm + 1
       nddmd = nddmd + 1
       if (kcs(nm)==1) then
          !
          ! determine the first derivative
          !
          x10  = 0.5_hp*(getdx(sferic,xcor(nmd) ,ycor(nmd) ,xcor(nm) ,ycor(nm) ,gdp) +    &
                         getdx(sferic,xcor(ndmd),ycor(ndmd),xcor(ndm),ycor(ndm),gdp))
          y10  = 0.5_hp*(getdy(sferic,xcor(nmd) ,ycor(nmd) ,xcor(nm) ,ycor(nm) ,gdp) +    &
                         getdy(sferic,xcor(ndmd),ycor(ndmd),xcor(ndm),ycor(ndm),gdp))

          x01  = 0.5_hp*(getdx(sferic,xcor(ndm) ,ycor(ndm) ,xcor(nm) ,ycor(nm) ,gdp) +    &
                         getdx(sferic,xcor(ndmd),ycor(ndmd),xcor(nmd),ycor(nmd),gdp))
          y01  = 0.5_hp*(getdy(sferic,xcor(ndm) ,ycor(ndm) ,xcor(nm) ,ycor(nm) ,gdp) +    &
                         getdy(sferic,xcor(ndmd),ycor(ndmd),xcor(nmd),ycor(nmd),gdp))
          !
          ! determine the derivative to ksi- and eta-direction
          !
          x11 = getdx(sferic,xcor(nmd),ycor(nmd),xcor(nm)  ,ycor(nm)  ,gdp)  +    &
                getdx(sferic,xcor(ndm),ycor(ndm),xcor(ndmd),ycor(ndmd),gdp)
          y11 = getdy(sferic,xcor(nmd),ycor(nmd),xcor(nm)  ,ycor(nm)  ,gdp)  +    &
                getdy(sferic,xcor(ndm),ycor(ndm),xcor(ndmd),ycor(ndmd),gdp)
          !
          ! determine the second derivative in ksi-direction
          !
          x20 = 0.0_fp
          y20 = 0.0_fp
          x02 = 0.0_fp
          y02 = 0.0_fp
          !
          kcsu = 0
          if (kcs(nmu)==1) kcsu = 1
          kcsd = 0
          if (kcs(nmd)==1) kcsd = 1
          kenm = kcsu + kcsd
          if (kenm/=0) then
             xxu = getdx(sferic,xcor(nm)  ,ycor(nm)  ,xcor(nmu)  ,ycor(nmu)  ,gdp) +  &
                   getdx(sferic,xcor(ndm) ,ycor(ndm) ,xcor(ndmu) ,ycor(ndmu) ,gdp) +  &
                   getdx(sferic,xcor(nm)  ,ycor(nm)  ,xcor(nmd)  ,ycor(nmd)  ,gdp) +  &
                   getdx(sferic,xcor(ndm) ,ycor(ndm) ,xcor(ndmd) ,ycor(ndmd) ,gdp)

             xxv = getdx(sferic,xcor(nmd) ,ycor(nmd) ,xcor(nm)   ,ycor(nm)   ,gdp) +  &
                   getdx(sferic,xcor(ndmd),ycor(ndmd),xcor(ndm)  ,ycor(ndm)  ,gdp) +  &
                   getdx(sferic,xcor(nmd) ,ycor(nmd) ,xcor(nmdd) ,ycor(nmdd) ,gdp) +  &
                   getdx(sferic,xcor(ndmd),ycor(ndmd),xcor(ndmdd),ycor(ndmdd),gdp)

             x20 = 0.5*(kcsu*xxu + kcsd*xxv) / kenm

             xxu = getdy(sferic,xcor(nm)  ,ycor(nm)  ,xcor(nmu)  ,ycor(nmu)  ,gdp) +  &
                   getdy(sferic,xcor(ndm) ,ycor(ndm) ,xcor(ndmu) ,ycor(ndmu) ,gdp) +  &
                   getdy(sferic,xcor(nm)  ,ycor(nm)  ,xcor(nmd)  ,ycor(nmd)  ,gdp) +  &
                   getdy(sferic,xcor(ndm) ,ycor(ndm) ,xcor(ndmd) ,ycor(ndmd) ,gdp)

             xxv = getdy(sferic,xcor(nmd) ,ycor(nmd) ,xcor(nm)   ,ycor(nm)   ,gdp) +  &
                   getdy(sferic,xcor(ndmd),ycor(ndmd),xcor(ndm)  ,ycor(ndm)  ,gdp) +  &
                   getdy(sferic,xcor(nmd) ,ycor(nmd) ,xcor(nmdd) ,ycor(nmdd) ,gdp) +  &
                   getdy(sferic,xcor(ndmd),ycor(ndmd),xcor(ndmdd),ycor(ndmdd),gdp)

             y20 = 0.5*(kcsu*xxu + kcsd*xxv) / kenm
          endif
          !
          ! determine the second derivative in eta-direction
          !
          kcsu = 0
          if (kcs(num)==1) kcsu = 1
          kcsd = 0
          if (kcs(ndm)==1) kcsd = 1
          kenm = kcsu + kcsd
          if (kenm/=0) then
             xxu = getdx(sferic,xcor(nm)  ,ycor(nm)  ,xcor(num)  ,ycor(num)  ,gdp) +  &
                   getdx(sferic,xcor(nmd) ,ycor(nmd) ,xcor(numd) ,ycor(numd) ,gdp) +  &
                   getdx(sferic,xcor(nm)  ,ycor(nm)  ,xcor(ndm)  ,ycor(ndm)  ,gdp) +  &
                   getdx(sferic,xcor(nmd) ,ycor(nmd) ,xcor(ndmd) ,ycor(ndmd) ,gdp)

             xxv = getdx(sferic,xcor(ndm) ,ycor(ndm) ,xcor(nm)   ,ycor(nm)   ,gdp) +  &
                   getdx(sferic,xcor(ndmd),ycor(ndmd),xcor(nmd)  ,ycor(nmd)  ,gdp) +  &
                   getdx(sferic,xcor(ndm) ,ycor(ndm) ,xcor(nddm) ,ycor(nddm) ,gdp) +  &
                   getdx(sferic,xcor(ndmd),ycor(ndmd),xcor(nddmd),ycor(nddmd),gdp)

             x02 = 0.5*(kcsu*xxu + kcsd*xxv) / kenm
             !
             xxu = getdy(sferic,xcor(nm)  ,ycor(nm)  ,xcor(num)  ,ycor(num)  ,gdp) +  &
                   getdy(sferic,xcor(nmd) ,ycor(nmd) ,xcor(numd) ,ycor(numd) ,gdp) +  &
                   getdy(sferic,xcor(nm)  ,ycor(nm)  ,xcor(ndm)  ,ycor(ndm)  ,gdp) +  &
                   getdy(sferic,xcor(nmd) ,ycor(nmd) ,xcor(ndmd) ,ycor(ndmd) ,gdp)

             xxv = getdy(sferic,xcor(ndm) ,ycor(ndm) ,xcor(nm)   ,ycor(nm)   ,gdp) +  &
                   getdy(sferic,xcor(ndmd),ycor(ndmd),xcor(nmd)  ,ycor(nmd)  ,gdp) +  &
                   getdy(sferic,xcor(ndm) ,ycor(ndm) ,xcor(nddm) ,ycor(nddm) ,gdp) +  &
                   getdy(sferic,xcor(ndmd),ycor(ndmd),xcor(nddmd),ycor(nddmd),gdp)

             y02 = 0.5*(kcsu*xxu + kcsd*xxv) / kenm
          endif
          !
          ! determine coefficients for curvature equation
          !
          x3(nm)  =                              x10*y20 - x20*y10
          x2y(nm) = 2.0_fp*(x10*y11 - x11*y10) + x01*y20 - x20*y01
          xy2(nm) = 2.0_fp*(x01*y11 - x11*y01) + x10*y02 - x02*y10
          y3(nm)  =                              x01*y02 - x02*y01
          !
       endif
    enddo
end subroutine xyder
