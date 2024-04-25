      subroutine ININFS(lundia,fout  ,extnef,mnstat,thick ,namcon,
     *                  grdang,tstart,dtmin ,notims,nostat,kmax  ,
     *                  lstci ,itdate                            )
      implicit none
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
!***********************************************************************
! Deltares                         marine and coastal management
!
!     Function: 1) Read general model information from the TRISULA
!                  (NEFIS) history file for hydrodynamics
!
!               Theo van der Kaaij
!               April 1993
!
!     Modified: Adjusted for nesthd2
!               Theo van der Kaaij
!               July 1993
!***********************************************************************
!
      integer       itdate
      integer       lundia
      integer       notims
      integer       ltur
      integer       l
      integer       nostat
      integer       kmax
      integer       lstci

      integer       tyden (2), date(2)

      integer       mnstat(2     ,nostat)

      double precision thick (kmax  )

      character*20  namcon(lstci + 2)  ! ltur has maximum of 2 (tke and eps)
!
!-----For nefis
!
      integer*4     fdNef,
     *              uindex(15), ! 3x5
     *              buflen,
     *              usrord(5)
      integer*4     error
      integer*4     getels, getelt, crenef, clsnef
!
      real dt_sp
      real tunit_sp
      real thick_sp(kmax)
      real grdang_sp
      double precision dt
      double precision tunit
      double precision dtmin
      double precision tstart
      double precision grdang
!
      character*1   coding, access
      character*(*) extnef
      character*256 fildat, fildef
      character*16  elmnam, celnam, grpnam
!
      logical       fout
      external      getels, getelt, crenef, clsnef
!
!              ****************************************
!              *     read dimensions from NEFIS files *
!              ****************************************
!
!     +-----------------------------------------------------------------
!     | Initialize
!     +-----------------------------------------------------------------
      coding    = ' '
      access    = 'r'
      uindex   = 0
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 0
      usrord(1) = 1
!     +-----------------------------------------------------------------
!     | Ophalen dimensies datagroep 'his-info-series'
!     +-----------------------------------------------------------------
      fildef = 'trih-' // trim(extnef) // '.def'
!
      fildat = 'trih-' // trim(extnef) // '.dat'
!
      error  = CRENEF (fdNef, fildat, fildef, coding, access)
      if (error /= 0) then
         write(lundia,'(a)')
     *        '***ERROR: opening FLOW his files'
         fout   = .true.
         goto 999
      endif
!
      celnam = 'his-info-series'
      grpnam = 'his-info-series'

      elmnam     = 'ITHISC'
      buflen     = 4 * notims
      uindex (2) = 2
      uindex (3) = 1
      error      = GETELT (fdNef, grpnam, elmnam, uindex,
     *                     usrord, buflen, tyden                 )
      if (error /= 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading Delft3D-FLOW his data file (ITHISC)'
         fout   = .true.
         goto 999
      endif
!
      grpnam     = 'his-const'
!
      elmnam    = 'ITDATE'
      buflen    = 8
      uindex(2) = 1
      uindex(3) = 1
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,date                )
      if (error /= 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading Delft3D-FLOW his data file (ITDATE)'
         fout   = .true.
         goto 999
      endif

      itdate = date(1)

      elmnam     = 'TUNIT'
      buflen     = 4
      uindex (2) = 1
      error      = getelt (fdNef, grpnam, elmnam, uindex,
     *                     usrord, buflen, tunit_sp              )
      if (error /= 0) then
         buflen     = 8
         error      = getelt (fdNef, grpnam, elmnam, uindex,
     *                        usrord, buflen, tunit                 )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (TUNIT)'
            fout   = .true.
            goto 999
         endif
      else
         tunit = dble(tunit_sp)
      endif
!
      elmnam     = 'DT'
      buflen     = 4
      uindex (2) = 1
      error      = getelt (fdNef, grpnam, elmnam, uindex,
     *                     usrord, buflen, dt_sp                 )
      if (error /= 0) then
         buflen     = 8
         error      = getelt (fdNef, grpnam, elmnam, uindex,
     *                        usrord, buflen, dt                    )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (DT)'
            fout   = .true.
            goto 999
         endif
      else
          dt = dble(dt_sp)
      endif
!----------------------------------------------------------------------
!     time frame
!----------------------------------------------------------------------
      dtmin = (dble(tyden(2)) - dble(tyden(1))) * dt * tunit/60.d0
!
      tstart = dble (tyden(1)) * dt * tunit
      tstart = tstart / 60.0d0
!
      if (nostat > 0) then
         elmnam    = 'MNSTAT'
         buflen    = 2*nostat*4
         uindex(2) = 1
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,mnstat              )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (MNSTAT)'
            fout   = .true.
            goto 999
         endif
      endif
!
      elmnam    = 'THICK'
      buflen    = 4*kmax
      uindex(2) = 1
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,thick_sp             )
      if (error /= 0) then
         buflen    = 8*kmax
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,thick             )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (THICK)'
            fout   = .true.
            goto 999
         endif
      else
          thick = dble(thick_sp)
      endif

      elmnam    = 'GRDANG'
      buflen    = 4
      uindex(2) = 1
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,grdang_sp           )
      if (error /= 0) then
         buflen    = 8
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,grdang              )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (GRDANG)'
            fout   = .true.
            goto 999
         endif
      else
          grdang = dble(grdang_sp)
      endif 

      elmnam    = 'LTUR'
      error     = GETELT(fdNef,grpnam    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
      if (error /= 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading Delft3D-FLOW his data file (LTUR)'
         fout   = .true.
         goto 999
      endif

      if ((lstci + ltur) > 0) then

         buflen    = 20 * (lstci + ltur)
         elmnam    = 'NAMCON'
         error     = GETELS(fdNef,grpnam    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,NAMCON    )
         if (error /= 0) then
            write(lundia,'(a)')
     *        '***ERROR: reading Delft3D-FLOW his data file (NAMCON)'
            fout   = .true.
            goto 999
        endif

         do 10 l = 1, lstci + ltur
            call small (namcon( l),20        )
   10    continue

      endif
!
!-----close NEFIS files
!
  999 continue

      error     = CLSNEF(fdNef)
!
      return

      end
!
!==============================================================================
!
      subroutine SIMHSH(lundia, fout, extnef, kfs, wl, uu, vv,
     *                  alfas ,grdang, notims, nostat, kmax)
      implicit none
!
!-----------------------------------------------------------------------
!     Function: 1) Read time series (hydrodynamics) from the NEFIS
!                  history file
!
!               2) Converts u- and v-velocities to north and south
!                  velocities
!----------------------------------------------------------------------
      integer notims
      integer lundia
      integer neferr
      integer itim
      integer istat
      integer k
      integer hulp
      integer nostat
      integer kmax
!
      double precision zcurut,zcurvt,mag   ,dir   ,eps   ,
     *                 pi    ,deg2rd, grdang
!
      real alfas_sp(nostat)
      double precision ALFAS (nostat)
!
      real wl_sp  (nostat,notims),
     *     uu_sp  (nostat,kmax  ,notims),
     *     vv_sp  (nostat,kmax  ,notims)
      double precision wl  (nostat,notims),
     *                 uu  (nostat,kmax  ,notims),
     *                 vv  (nostat,kmax  ,notims)
      integer          kfs(nostat,notims)

      double precision rd2deg
!----------------------------------------------------------------------
!     Nefis declarations
!----------------------------------------------------------------------
      integer*4     fdNef,
     *              uindex(    15), ! 3x5
     *              buflen,
     *              usrord(5)
      integer*4     error
      integer*4     getelt, crenef, clsnef
!
      character*1   coding, access
      character*(*) extnef
      character*256 fildat,fildef
      character*16  elmnam,grpnam
      character*1024 error_string
!
      logical       fout
      external      getelt, crenef, clsnef
!----------------------------------------------------------------------
!     initialisation
!----------------------------------------------------------------------
      eps    = 1.0e-6
      pi     = 4.*atan(1.)
      deg2rd = pi/180.
      rd2deg = 180./pi
      write (*     ,'('' >>> Reading hydrodynamic data from history '',
     *                  ''file <<<'')')
      write (lundia,'('' >>> Reading hydrodynamic data from history '',
     *                  ''file <<<'')')
!----------------------------------------------------------------------
!     open NEFIS files
!----------------------------------------------------------------------
      coding    = 'N'
      access    = 'R'
      fildef    = 'trih-'//trim(extnef)//'.def'
!
      fildat    = 'trih-'//trim(extnef)//'.dat'
      error  = CRENEF (fdNef, fildat, fildef, coding, access)
      if (error /= 0) then
         write(lundia,'(a)')
     *        '***ERROR: opening FLOW his file(s)'
         fout   = .true.
         goto 999
      endif
!
      usrord    = 0
      usrord(1) = 1
!----------------------------------------------------------------------
!     get ALFAS
!----------------------------------------------------------------------
      grpnam    = 'his-const'
!
      uindex    = 0    
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      buflen    = 4 * nostat
      elmnam    = 'ALFAS'
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,alfas_sp     )
      if (error /= 0) then
          buflen = 8*nostat
         error     = GETELT(fdNef,grpnam    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,alfas     )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (ALFAS)'
            fout   = .true.
            goto 999
         endif
      else
          alfas = dble(alfas_sp)
      endif
!----------------------------------------------------------------------
!     cycle through hisfile
!----------------------------------------------------------------------
      grpnam    = 'his-series'
!
!----------- mask array for wl-point (active (1), inactive (0))
      elmnam    = 'ZKFS'
      buflen    = 4*nostat*notims
      kfs       = 0
      uindex    = 0    
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,kfs )
      if (error /= 0) then
         error = neferr(fdnef, error_string)
         write(lundia,'(a)') trim(error_string)
         kfs = 1
      endif
!
!----------- water levels
      elmnam    = 'ZWL'
      buflen    = 4*nostat*notims
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,wl_sp                  )
      if (error /= 0) then
         buflen    = 8*nostat*notims
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,wl                  )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (ZWL)'
            fout   = .true.
            goto 999
         endif
      else
          wl = dble(wl_sp)
      endif
!
!------- u and v-velocities to get north and south vel
!
      elmnam    = 'ZCURU'
      buflen    = 4*nostat*kmax*notims
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,uu_sp                  )
      if (error /= 0) then
         buflen    = 8*nostat*kmax*notims
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,uu                  )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (ZCURU)'
            fout   = .true.
            goto 999
         endif
      else
          uu = dble(uu_sp)
      endif
      
!
      elmnam    = 'ZCURV'
      buflen    = 4*nostat*kmax*notims
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,vv_sp                  )
      if (error /= 0) then
         buflen    = 8*nostat*kmax*notims
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,vv                  )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (ZCURV)'
            fout   = .true.
            goto 999
         endif
      else
          vv = dble(vv_sp)
      endif

!-------------------------------------------------------------------
!     Translate to south and north velocities
!-------------------------------------------------------------------
      do 10 itim = 1, notims
         do 10 istat = 1, nostat
            do 10 k = 1, kmax
               zcurut = uu   (istat ,k     ,itim  )*
     *                        cos (alfas (istat) * deg2rd) -
     *                  vv   (istat ,k     ,itim  )*
     *                        sin (alfas (istat) * deg2rd)
               zcurvt = uu   (istat ,k     ,itim  )*
     *                         sin (alfas (istat) * deg2rd) +
     *                  vv   (istat ,k     ,itim  )*
     *                         cos (alfas (istat) * deg2rd)
!
             mag    = sqrt(zcurut*zcurut + zcurvt*zcurvt)
             if (abs(zcurut) .lt. eps) zcurut = eps
             if (abs(zcurvt) .lt. eps) zcurvt = eps
             hulp   = 90. -atan2(zcurvt,zcurut)*rd2deg +
     *                grdang
             dir    = amod (hulp + 360.,360.)

             uu (istat,k,itim ) = mag*sin(dir*deg2rd)
             vv (istat,k,itim ) = mag*cos(dir*deg2rd)
   10 continue
!----------------------------------------------------------------------
!     close NEFIS files
!----------------------------------------------------------------------
  999 continue
!
      error     = CLSNEF(fdNef)
!
      return
      end

      subroutine SIMHSC(lundia,fout  ,extnef,
     *                  notims,nostat,kmax  ,lstci ,
     *                  conc                       )
      implicit none
!
!-----------------------------------------------------------------------
!     Function: 1) Read time series (constituents) from the NEFIS
!                  history file
!----------------------------------------------------------------------

      integer lundia, notims, nostat, kmax, lstci

      real*4 concsp   (nostat,kmax  ,lstci ,notims)
      double precision conc   (nostat, kmax  ,lstci ,notims)
      integer       istat, k, iconc, itim
!
!----------------------------------------------------------------------
!     Nefis declarations
!----------------------------------------------------------------------
      integer*4     fdNef,
     *              uindex(    15), ! 3x5
     *              buflen,
     *              usrord(5)
      integer*4     error
      integer*4     getelt, crenef, clsnef
!
      character*1   coding, access
      character*(*) extnef
      character*256 fildat,fildef
      character*16  elmnam,grpnam
!
      logical       fout
      external      getelt, crenef, clsnef
!----------------------------------------------------------------------
!     open NEFIS files
!----------------------------------------------------------------------
      write (*     ,'(/,'' >>> Reading transport data from history '',
     *                  ''file <<<'')')
      write (lundia,'(/,'' >>> Reading transport data from history '',
     *                  ''file <<<'')')

      coding    = ' '
      access    = 'r'
      fildef    = 'trih-'//trim(extnef)//'.def'
      fildat    = 'trih-'//trim(extnef)//'.dat'
      error  = CRENEF (fdNef, fildat, fildef, coding, access)
      if (error /= 0) then
         write(lundia,'(a)')
     *        '***ERROR: opening Delft3D-FLOW his file(s)'
         fout   = .true.
         goto 999
      endif
!
      usrord    = 0
      usrord(1) = 1
!----------------------------------------------------------------------
!     get concentrations
!----------------------------------------------------------------------

      grpnam = 'his-series'
      usrord    = 0
      usrord(1) = 1
!
      buflen = 4 * nostat * kmax * lstci*notims
      elmnam = 'GRO'
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
!-----------------------------------------------------------------------
!--------Read from group 3 GRO
!-----------------------------------------------------------------------
      write(*,'(a,a)') 
     * '     Reading transport data, element ', trim(elmnam)
      error    = GETELT(fdNef,grpnam    ,elmnam    ,
     *                  uindex,usrord ,buflen    ,concsp      )
      if (error /= 0) then
         buflen = 8 * nostat * kmax * lstci * notims
         error    = GETELT(fdNef,grpnam    ,elmnam    ,
     *                     uindex,usrord ,buflen    ,conc      )
         if (error /= 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading Delft3D-FLOW his data file (GRO)'
            fout   = .true.
            goto 999
         endif
      else
         do istat = 1, nostat
            do k = 1, kmax
               do iconc = 1, lstci
                  do itim = 1, notims
                     conc(istat,k,iconc,itim)=concsp(istat,k,iconc,itim)
                  enddo
               enddo
            enddo
         enddo
      endif
      
!----------------------------------------------------------------------
!     close NEFIS files
!----------------------------------------------------------------------
  999 continue
!
      error     = CLSNEF(fdNef)
!
      return
      end
