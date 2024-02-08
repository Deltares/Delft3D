      subroutine nest_hd2 (lun   ,extnef,nostat,notims,kmax  ,lstci ,
     *                     nobnd ,mincon,
     *                     thick ,wl    ,uu    ,vv    ,conc, alfas ,
     *                     bndval,kfs   ,mcbsp ,ncbsp ,mnstat,
     *                     typbnd,nambnd,namcon                     )
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
! subroutine         : main
! version            : v1.3
! date               : October 1997
! programmer         : Theo van der Kaaij
!
! function           :    determine time series boundary conditions
!                         for water levels, velocities and/or
!                         constituents.
! notes              : 1. Assumed is a layer distribution which is
!                         equal in both the overall and the detailed
!                         model.
!                      2. Assumed are identical constituents in both
!                         models
!                      3. vv array also used for concentrations!!!!
!                      4. bndval array used for hydrodynamic and
!                         transport Boundary conditions. Decared on
!                         the maximum of lstci and 1 (mincon)
!***********************************************************************
      integer       itdate

      integer       lun   (  *   ), cgen  (  6   )

      integer       mcbsp (nobnd ,2     )       ,
     *              ncbsp (nobnd ,2     )       ,
     *              mnstat(2     ,nostat)

      double precision           thick (kmax  ),alfas (nostat),
     *              cadd  (  6   ),cmax  (   6  ),
     *              cmin  (  6   )

      double precision           wl    (nostat,notims)
      integer       kfs   (nostat,notims)
      integer       iwet  (nostat)

      double precision uu    (nostat,kmax  ,notims),
     *                 vv    (nostat,kmax  ,notims),
     *                 conc  (nostat,kmax  ,notims, mincon)

      double precision bndval(nobnd ,notims,kmax  ,mincon,2)
      double precision grdang
      double precision tstart
      double precision dtmin
      double precision a0
      integer itypc
      integer nolay
      integer nocon
      integer nostat
      integer notims, itim
      integer kmax, k
      integer lstci, iconc
      integer nobnd, ibnd
      integer mincon

      character*  1 typbnd(nobnd )
      character* 20 nambnd(nobnd )
      character* 20 namcon (lstci  + 2)
      character*(*) extnef

      logical       fout  , new   , dav

!-----------------------------------------------------------------------
!---- 0. Initialisation
!-----------------------------------------------------------------------

      iwet  = 0
      cgen  = 0
      mnstat= 0
      mcbsp = 0
      ncbsp = 0

      thick = 0.0d0
      alfas = 0.0d0
      cadd  = 0.0d0
      cmax  = 0.0d0
      cmin  = 0.0d0

      kfs   = 0
      wl    = 0.0d0
      uu    = 0.0d0
      vv    = 0.0d0
      bndval= 0.0d0

      fout = .false.
      new  = .false.
      dav  = .false.

!-----------------------------------------------------------------------
!---- 0   (Temporary) old or new file types
!-----------------------------------------------------------------------
!
!     write (*,'('' (O)ld or (N)ew file format         : '',$)')
!     read  (*,'(a1)') typbnd(1)
!     call small (typbnd(1),1)
!
!     if (typbnd(1) .eq. 'n') new = .true.

      new = .true.
      write (*,'( )')

!-----------------------------------------------------------------------
!---- 1.0 Read boundary definition
!      .1 Get general infromation from nefis history file
!      .2 Get additional input
!-----------------------------------------------------------------------

      call reabnd(lun(1), mcbsp , ncbsp , typbnd, nambnd,
     *            nobnd                                 )

      call ININFS(lun(5),fout  ,extnef,mnstat,thick ,namcon,
     *            grdang,tstart,dtmin ,notims,nostat,kmax  ,
     *            lstci ,itdate                            )
      if (fout) goto 999

      call addinp(a0    , dav   , cgen  , cadd  , cmax  ,
     *            cmin  , itypc , typbnd, nobnd , namcon,
     *            lstci , kmax                           )

!-----------------------------------------------------------------------
!     2.0 Get all hydrodynamic data
!      .1 Determine if a station is permanently dry
!      .2 determine hydrodynamic boundary conditions (time-series)
!      .3 write to output file
!-----------------------------------------------------------------------

      call SIMHSH(lun(5), fout, extnef, kfs, wl, uu, vv,
     *            alfas, grdang, notims, nostat, kmax)
     
      if (fout) goto 999

      call chkdry(iwet, kfs, notims, nostat)

      call dethyd(lun(5),fout  ,lun(2),bndval,mcbsp ,ncbsp ,
     *            typbnd,mnstat,wl    ,uu    ,vv    ,iwet  ,
     *            nobnd ,notims,nostat,kmax  ,a0           )
      if (fout) goto 999

      call hyd2dh(bndval,typbnd,thick ,nobnd ,notims,kmax  ,
     *            nolay ,dav                               )

      if (new) then
         call wrihyd2(lun(5),lun(3),bndval,typbnd,nambnd,nobnd ,
     *                notims,kmax  ,nolay ,tstart,dtmin ,itdate)
      else
         call wrihyd (lun(5),lun(3),bndval,typbnd,nobnd ,notims,
     *                kmax  ,nolay ,tstart,dtmin )
      endif
!
!-----------------------------------------------------------------------
!     3.1 Get all constituent data
!      .2 determine transport boundary conditions
!      .3 write to output file
!-----------------------------------------------------------------------
!
      if (lstci .eq. 0) goto 999

      call SIMHSC(lun(5),fout  ,extnef,notims,nostat,kmax  ,lstci ,
     *            conc                                            )
      if (fout) goto 999

      bndval = 0.0d0
      do ibnd = 1, nobnd
          do itim = 1, notims
              do k = 1, kmax
                  do iconc = 1, lstci
                      bndval(ibnd,itim,k,iconc,1)=0.0d0
                      bndval(ibnd,itim,k,iconc,2)=0.0d0
                  enddo
              enddo
          enddo
      enddo

      call detcon (lun(5),fout  ,lun(2),bndval,mcbsp ,ncbsp ,
     *             mnstat,conc  ,iwet  ,nobnd ,notims,nostat,
     *             kmax  ,lstci                             )
      if (fout) goto 999

      call corcon (bndval,thick ,namcon,itypc ,nobnd ,notims,
     *             kmax  ,lstci ,nocon ,cgen  ,cadd  ,cmax  ,
     *             cmin                                     )

      if (new) then
         call wricon2(lun(5),lun(4),bndval,itypc ,nobnd ,notims,
     *                kmax  ,lstci ,nocon ,tstart,dtmin ,
     *                namcon,nambnd,itdate                     )
      else
         call wricon (lun(5),lun(4),bndval,itypc ,nobnd ,notims,
     *                kmax  ,lstci ,nocon ,tstart,dtmin )
      endif
!-----------------------------------------------------------------------
!---- end program
!-----------------------------------------------------------------------

  999 continue

      return

      end
