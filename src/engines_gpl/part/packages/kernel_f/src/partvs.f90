!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine partvs ( lun2   , itime  , nosubs , nopart , ivtset ,   &
                          ivtime , vsfour , vsfact , wpart  , wsettl ,   &
                          modtyp , nmax   , mmax   , lgrid3 , nolay  ,   &
                          npart  , mpart  , kpart  , nosegp , noseglp ,  &
                          rhopart, rhowatc, spart  , iptime)

!       Deltares Software Centre

!>\file
!>         calculates settling velocitie per particle
!>
!>         Interpolates linearly in the vsfour array, its 6 coefficients:
!>         - 1 base value (m/s)
!>         - 2 amplitude  (m/s)
!>         - 3 period  (hours)
!>         - 4 phase shift(hours)
!>         - 5 minimum allowable value
!>         - 6 maximum allowable value
!>         The linear interpolation in Fourier coefficients is questionable.\n
!>         Fourier coefficients are however seldomly used.\n
!>         Questionable is also that the settling velocity coefficients per substance are weight averaged
!>         for the substance share in the particle to get the coefficients of the particle.\n
!>         The thus computed value is limited by the minimum and maximum values.\n
!>         The routine also computes average settling velocities for all particles and for the settling
!>         particles only and sends them to the output file together with the number of settling and
!>         non settling particles. This is important information in the output file to check the
!>         functioning of the model.
!>
!>         The author writes furthermore:\n
!>         Temporary solution: particles are unique substances or otherwise settling velocity is averaged
!>         over substances hoping that they are defined in the same units responsibility for user.

!     logical unit numbers  : lun2  output report file

!     subroutines called    : none.

!     functions   called    : none.

      use precision_part    ! single/double precision
      use spec_feat_par
      use timers
      use m_part_modeltypes
      implicit none    ! explicit typing

!     Arguments

!     kind            function         name                      description

      integer  ( ip), intent(in   ) :: lun2                    !< unit of output report file
      integer  ( ip), intent(in   ) :: itime                   !< actual time
      integer  ( ip), intent(in   ) :: nosubs                  !< number of substances
      integer  ( ip), intent(in   ) :: nopart                  !< number of particles
      integer  ( ip), intent(in   ) :: ivtset                  !< number of time breakpoints
      integer  ( ip), intent(in   ) :: ivtime(ivtset)          !< time breakpoint values settling velocities
      real     ( rp), intent(in   ) :: vsfour(6,nosubs,ivtset) !< settling velocity parameters
      real     ( rp)                :: vsfact(6,nosubs)        !< local work array
      real     ( rp), intent(in   ) :: wpart (  nosubs,nopart) !< weight of substances per particle
      real     ( rp), intent(  out) :: wsettl(         nopart) !< actual settling velocity per particle
      integer  ( ip), intent(in   ) :: modtyp
      integer  ( ip), intent(in   ) :: nosegp
      integer  ( ip), intent(in   ) :: noseglp
      integer  ( ip), intent(in   ) :: npart( nopart)
      integer  ( ip), intent(in   ) :: mpart( nopart)
      integer  ( ip), intent(in   ) :: kpart( nopart)
      integer  ( ip), intent(in   ) :: nmax                    !< first dimension lgrid
      integer  ( ip), intent(in   ) :: mmax                    !< second dimension lgrid
      integer  ( ip), intent(in   ) :: lgrid3 (nmax,mmax)      !< active grid matrix with noseg numbering
      integer  ( ip), intent(in   ) :: nolay
      real     ( rp), intent(in   ) :: rhopart (nosubs, nopart)
      real     ( rp), intent(in   ) :: rhowatc (nosegp)
      real     ( rp), intent(in   ) :: spart (nosubs,*)        !< size of the particles
      integer  ( ip), intent(in   ) :: iptime (nopart)         !< age of the particles
      

!     local scalars

      integer(ip)     id   , isub    ! loop variables time and substances
      integer(ip)     ipart          ! loop variable for particles
      real   (rp)     fac1 , fac2    ! interpolation factors
      real   (rp)     twopi          ! 2*pi
      real   (rp)     g              ! gravitational acceleration (m/s2)
      real   (rp)     viscosity_water ! viscosity of water (Pa.s)
      real   (rp)     vsfact1        ! help variable
      real   (dp)     vs1  , vs2  , vs3  , vs4  , vs5  , vs6  , vst  ! accumulation help variables
      real   (dp)     w              ! help variable
      integer(ip)     nonset         ! accumulator for non-settling particles
      real   (dp)     waver          ! accumulator of the settling velocities
      integer(ip)     ic, iseg       ! 2D and 3D segmentnumber of particle

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( ivtset .le. 0 ) return
      if ( timon ) call timstrt( "partvs", ithndl )

!     initialisation

      twopi = 8.0*atan(1.0)
      g = 9.81                 ! gravitational acceleration (m/s2)
      viscosity_water=1.002e-3 ! viscosity of water (Pa.s)

!     find the moment

      do id = 1, ivtset
         if ( itime .lt. ivtime(id) .or. id .eq. ivtset ) exit
      enddo

!     determine interpolation factors

      fac1 = float(itime - ivtime(id-1)) / float(ivtime(id) - ivtime(id-1))
      fac2 = 1.0 - fac1

!     calculate ; by linear interpolation between breakpoints; fourier comp.

      do isub = 1, nosubs
          vsfact(1,isub) = fac1*vsfour(1,isub,id) + fac2*vsfour(1,isub,id-1)
          vsfact(2,isub) = fac1*vsfour(2,isub,id) + fac2*vsfour(2,isub,id-1)
          vsfact(3,isub) = fac1*vsfour(3,isub,id) + fac2*vsfour(3,isub,id-1)
          vsfact(4,isub) = fac1*vsfour(4,isub,id) + fac2*vsfour(4,isub,id-1)
          vsfact(5,isub) = fac1*vsfour(5,isub,id) + fac2*vsfour(5,isub,id-1)
          vsfact(6,isub) = fac1*vsfour(6,isub,id) + fac2*vsfour(6,isub,id-1)
      enddo

      waver  = 0.0
      nonset = 0
!$OMP PARALLEL DO PRIVATE   ( vs1, vs2, vs3, vs4, vs5, vs6, vst, w,     &
!$OMP                         isub ),      &
!$OMP             REDUCTION ( + : waver, nonset )
      do 100 ipart = 1, nopart
         vs1 = 0.0
         vs2 = 0.0
         vs3 = 0.0
         vs4 = 0.0
         vs5 = 0.0
         vs6 = 0.0
         vst = 0.0
         do isub = 1, nosubs
            if (modtyp .eq. model_prob_dens_settling) then
               ! density dependent settling velocity 
               ic = lgrid3(npart(ipart), mpart(ipart))
!              active cell's only
               if (ic  >  0) then
                  if(kpart(ipart) <= 0.or.kpart(ipart) > nolay) then
                     write(*,*) ' ipart = ',ipart,' k = ',kpart(ipart)
                     write (*,*) ' K is out of range in partwr '
                     write( lun2,*) ' K is out of range in partwr '
                     call stop_exit(1)
                  endif
                  iseg = (kpart(ipart) - 1)*noseglp + ic
                  vsfact1 = plshapefactor(isub) * 2.0e0 / 9.0e0 * (rhopart(isub,ipart) - rhowatc(iseg)) / &
                            viscosity_water * g * spart(isub,ipart)**2 / (2 ** ((iptime(ipart) / 86400.0e0) * plfragrate(isub)))
               endif
            else
               vsfact1 = vsfact(1,isub)
            endif
            
            if ( abs(vsfact1) .gt. 1.0e-15 .or.             &
                 abs(vsfact(2,isub)) .gt. 1.0e-15 ) then
               vs1 = vs1 + vsfact1*wpart(isub,ipart)
               vs2 = vs2 + vsfact(2,isub)*wpart(isub,ipart)
               vs3 = vs3 + vsfact(3,isub)*wpart(isub,ipart)
               vs4 = vs4 + vsfact(4,isub)*wpart(isub,ipart)
               vs5 = vs5 + vsfact(5,isub)*wpart(isub,ipart)
               vs6 = vs6 + vsfact(6,isub)*wpart(isub,ipart)
               vst = vst + wpart(isub,ipart)
            endif
         enddo

         if ( abs(vst) .gt. 1.0e-20 ) then
            vs1 = vs1/vst
            vs2 = vs2/vst
            vs3 = 3600*vs3/vst
            vs4 = 3600*vs4/vst
            vs5 = vs5/vst
            vs6 = vs6/vst
            if ( abs(vs3) .gt. 1.0e-20 ) then
               w = vs1 + vs2*sin(twopi*(itime+vs4)/vs3)
               wsettl(ipart) = min( max( wsettl(ipart)*w  ,vs5 ), vs6 )
            else
               wsettl(ipart) = min( max( wsettl(ipart)*vs1,vs5 ), vs6 )
            endif
            waver = waver + wsettl(ipart)
         else
            wsettl(ipart) = 0.0
            nonset = nonset + 1
         endif
  100 continue
!$OMP END PARALLEL DO

      write( lun2, '(/)' )
      if ( nopart .gt. 0 ) then
         write ( lun2, 1010 ) waver/nopart
         if ( nonset .gt. 0 ) then
            if ( nonset .lt. nopart ) then
               write( lun2, 1020 ) waver/(nopart-nonset)
            endif
         endif
         write ( lun2, 1030 ) nopart-nonset
         write ( lun2, 1040 ) nonset
      endif

!     end of subroutine

      if ( timon ) call timstop ( ithndl )
      return

 1010 format(6x,'Settling velocity averaged over all particles           : ', es15.7 )
 1020 format(6x,'Settling velocity averaged over settling particles      : ', es15.7 )
 1030 format(6x,'Number of settling     particles (i.e. v-settling # 0)  : ', i12 )
 1040 format(6x,'Number of non-settling particles (i.e. v-settling = 0)  : ', i12 )

      end subroutine
