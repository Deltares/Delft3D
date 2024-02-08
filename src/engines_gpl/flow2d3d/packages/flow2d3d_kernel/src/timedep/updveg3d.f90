subroutine updveg3d(mmax      ,nmax      ,kmax      ,sig       ,thick     , &
                   & dps       ,kfs       ,s1        ,u1        ,v1        , &
                   & diapl     ,rnpl      ,gdp       )
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
    use mathconsts
    use globaldata
    use dfparall

    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: amiss
    integer, dimension(:,:)              , pointer :: planttype
    real(fp), dimension(:,:)             , pointer :: nplants
    type (dpm_vegetation), dimension(:)  , pointer :: vegs
    integer                              , pointer :: lundia
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: rhow
!
! Constants
!
    integer , parameter :: numiter = 30        ! number of iterations for calculating phi
    real(fp), parameter :: px      = 0.15_fp
    real(fp), parameter :: pm      = 0.001_fp
!
! Global variables
!
    integer                                                            , intent(in)  :: kmax
    integer                                                            , intent(in)  :: mmax
    integer                                                            , intent(in)  :: nmax
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfs
    real(fp), dimension(kmax)                                          , intent(in)  :: sig
    real(fp), dimension(kmax)                                          , intent(in)  :: thick
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)    , intent(in)  :: dps
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: s1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: u1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: v1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(out) :: diapl
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(out) :: rnpl
!
! Local variables
!
    integer           :: i
    integer           :: k
    integer           :: l
    integer           :: m
    integer           :: n
    integer           :: vegtype
    real(fp)          :: a
    real(fp)          :: apln
    real(fp)          :: aplo
    real(fp)          :: aplt
    real(fp)          :: arm
    real(fp)          :: aspn
    real(fp)          :: aspo
    real(fp)          :: aspt
    real(fp)          :: b
    real(fp)          :: bendm
    real(fp)          :: buoym
    real(fp)          :: cdn
    real(fp)          :: cos_angle
    real(fp)          :: dian
    real(fp)          :: diao
    real(fp)          :: dz
    real(fp)          :: dzf
    real(fp)          :: el
    real(fp)          :: fb
    real(fp)          :: fbuoy
    real(fp)          :: frac
    real(fp)          :: height
    real(fp)          :: pdens
    real(fp)          :: ph
    real(fp)          :: phi         ! angle between vegatation and z-axis
    real(fp)          :: phi_old
    real(fp)          :: phl
    real(fp)          :: pi4         ! pi/4
    real(fp)          :: rhop
    real(fp)          :: rlbot
    real(fp)          :: rltop
    real(fp)          :: rnn
    real(fp)          :: rno
    real(fp)          :: sin_angle
    real(fp)          :: tt
    real(fp)          :: u
    real(fp)          :: v
    real(fp)          :: vegcount
    real(fp)          :: vmag2
    real(fp)          :: zfl
    real(fp)          :: zflb
    real(fp)          :: zflow
    real(fp)          :: zflt
    real(fp)          :: zplb
    real(fp)          :: zplt
    character(80)     :: message
!
!! executable statements -------------------------------------------------------
!
    amiss      => gdp%gdconst%amiss
    planttype  => gdp%gdveg3d%planttype
    nplants    => gdp%gdveg3d%nplants
    vegs       => gdp%gdveg3d%vegs
    lundia     => gdp%gdinout%lundia
    ag         => gdp%gdphysco%ag
    rhow       => gdp%gdphysco%rhow
    pi4 = pi * 0.25_fp
    !
    ! Initialization
    !
    do k = 1, kmax
       do n = 1, nmax
          do m = 1, mmax
             diapl(n, m, k) = 0.0_fp
             rnpl(n, m, k)  = 0.0_fp
          enddo
       enddo
    enddo
    do m = 1, mmax 
       do n = 1, nmax 
          if (abs(kfs(n, m)) == 1) then
             if (planttype(n,m) /= 0) then
                vegtype  = planttype(n,m)
                vegcount = nplants(n,m)
                height   = s1(n, m) + real(dps(n, m),fp)
                !
                ! Calculate angle of plant
                ! Assumptions:
                ! - The vegetation is fully stiff
                ! - The vegetation is "tied" to the bottom
                !
                if (comparereal(vegs(vegtype)%rho(1),amiss) == 0) then
                   phi = 0.0_fp
                else
                   phi = 0.3_fp
                   do i = 1,numiter
                      phi_old   = phi
                      cos_angle = cos(phi)
                      sin_angle = sin(phi)
                      bendm     = 0.0_fp
                      buoym     = 0.0_fp
                      do k  = 1,kmax
                         u     = (u1(n,m,k) + u1(n,m-1,k)) / 2.0_fp
                         v     = (v1(n,m,k) + v1(n-1,m,k)) / 2.0_fp
                         vmag2 = u*u+v*v
                         zflow  = (1.0_fp+sig(k)) * height       !center layer
                         dzf    = thick(k)   * height            !thickness layer
                         zflt   = zflow + dzf/2.0_fp             !upper boundary layer
                         zflb   = zflow - dzf/2.0_fp + 0.0001_fp !lower boundary layer
                         do l = 1,vegs(vegtype)%nvps-1
                            zplt = vegs(vegtype)%z(l+1)* cos_angle       !upper side plantelement
                            zplb = vegs(vegtype)%z(l)  * cos_angle       !lower side plantelement
                            if (      comparereal(zflb,zplb) /= -1 &
                              & .and. comparereal(zflb,zplt) /=  1) then
                               a     =   (zflow                - vegs(vegtype)%z(l)) &
                                     & / (vegs(vegtype)%z(l+1) - vegs(vegtype)%z(l))
                               b     =  1.0_fp - a
                               cdn   = (b*vegs(vegtype)%cdcoef(l) + a*vegs(vegtype)%cdcoef(l+1))
                               dian  = (b*vegs(vegtype)%dia   (l) + a*vegs(vegtype)%dia   (l+1))*cdn
                               rnn   = (b*vegs(vegtype)%nstem (l) + a*vegs(vegtype)%nstem (l+1))*vegcount
                               fb    = 0.5_fp * cdn * dian * rnn * vmag2 * rhow
                               frac  = (zplt - max(zplb,zflb)) / dzf  !fraction plant in layer
                               frac  = min(1.0_fp,frac)
                               fb    = fb * thick(k) * height * frac
                               arm   = zflow - 0.5_fp*thick(k)*height*(1.0_fp-frac)
                               bendm = bendm + fb*arm
                            endif
                         enddo
                      enddo
                      arm   = 0.0_fp
                      buoym = 0.0_fp
                      do l  = 1,vegs(vegtype)%nvps-1  ! then upwelling moment buoym
                         dian  = 0.5_fp * (vegs(vegtype)%dia  (l) + vegs(vegtype)%dia  (l+1))
                         rnn   = 0.5_fp * (vegs(vegtype)%nstem(l) + vegs(vegtype)%nstem(l+1))
                         rhop  = 0.5_fp * (vegs(vegtype)%rho  (l) + vegs(vegtype)%rho  (l+1))
                         apln  = dian * dian * rnn * pi4
                         rltop = min(vegs(vegtype)%z(l+1) , height/cos_angle)
                         rlbot = min(vegs(vegtype)%z(l  ) , height/cos_angle)
                         el    = rltop  - rlbot
                         fbuoy = (rhow  - rhop) * ag * apln * el
                         arm   = rltop  - 0.5_fp*el
                         buoym = buoym + fbuoy*arm*sin_angle
                      enddo
                      if (comparereal(bendm,0.0_fp) /= 0) then
                         ph  = (bendm - buoym) / (bendm + buoym)
                         tt  = real(i-1,fp) / real(numiter,fp)
                         phl = tt*pm + (1-tt)*px
                         ph  = max( -phl , min(phl,ph) )
                      else
                         ph = 0.0_fp
                      endif
                      phi = phi + ph
                      phi = min( pi*0.5_fp , max(0.0_fp,phi) )
                      !
                      ! Jump out of iteration loop when phi has not changed much
                      !
                      if (comparereal(abs(phi-phi_old),0.01_fp) == -1) exit
                   enddo
                endif
                cos_angle = cos(phi)
                do k = 1, kmax
                   zflow = (1.0_fp+sig(k)) * height
                   dzf   = thick(k)        * height
                   zflt  = zflow + dzf/2.0_fp
                   zflb  = zflow - dzf/2.0_fp + 0.0001_fp
                   do l = 1, vegs(vegtype)%nvps - 1
                      zplt = vegs(vegtype)%z(l+1) * cos_angle
                      zplb = vegs(vegtype)%z(l)   * cos_angle
                      if (        comparereal(zflb,zplb) >  -1 &
                          & .and. comparereal(zflb,zplt) == -1  ) then
                         dz = zplt - zplb
                         if (comparereal(dz,0.0_fp) == 0) then
                            write(message,'(a,i0)') 'Identical z coordinates in specification of vegetation type ',vegtype
                            call prterr(lundia, 'U021', message)
                            call d3stop(1, gdp)
                         endif
                         zfl  = 0.5_fp * (min(zplt,zflt) + zflb)
                         a    = (zfl - zplb)/dz
                         b    = 1.0_fp - a
                         diao = diapl(n,m,k)
                         rno  = rnpl (n,m,k)
                         aspo = diao * rno                 ! aspect  old
                         aplo = diao * diao * rno * pi4    ! a plane old
                         !
                         ! Version Arjen has cdn = 1
                         !
                         cdn  = (b*vegs(vegtype)%cdcoef(l) + a*vegs(vegtype)%cdcoef(l+1))
                         !
                         ! cd is stored in dia
                         !
                         dian = (b*vegs(vegtype)%dia   (l) + a*vegs(vegtype)%dia   (l+1)) * cdn
                         rnn  = (b*vegs(vegtype)%nstem (l) + a*vegs(vegtype)%nstem (l+1)) * vegcount
                         if (       comparereal(aspo,0.0_fp) == 0 &
                             & .or. comparereal(aplo,0.0_fp) == 0  ) then
                            diapl(n,m,k) = dian
                            rnpl (n,m,k) = rnn
                         else
                            aspn         = dian * rnn               ! aspect  new
                            apln         = dian * dian * rnn * pi4  ! a plane new
                            aspt         = aspo + aspn              ! aspect  tot
                            aplt         = aplo + apln              ! a plane tot
                            diapl(n,m,k) = aplt / (aspt*pi4)
                            rnpl (n,m,k) = (aspt*aspt*pi4) / aplt
                         endif
                      endif
                   enddo
                enddo
             endif
          endif
       enddo
    enddo
    !
    ! Check plant density
    !
    do k = 1, kmax
       do n = 1, nmax
          do m = 1, mmax
             pdens = 1.0 - diapl(n, m, k)*diapl(n, m, k)*rnpl(n, m, k)*pi4
             if (comparereal(pdens,0.01_fp) == -1) then
                write(message,'(a,i0,a,i0,a)') 'plant density higher than possible for (m,n) = (', &
                    &                          m,',',n,')'
                call prterr(lundia, 'U021', message)
                call d3stop(1, gdp)
             endif
          enddo
       enddo
    enddo
end subroutine updveg3d
