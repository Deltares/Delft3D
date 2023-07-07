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
      module m_difacr
      use m_difhor


      implicit none

      contains


      subroutine difacr(icx       ,icy       ,j         ,nmmaxj    ,
     *                  nmmax     ,kmax      ,
     *                  lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  kadu      ,kadv      ,
     *                  s0        ,dps       ,r0        ,ddkl      ,
     *                  guu       ,gvv       ,guv       ,gvu       ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dsdksi    ,dtdksi    ,dsdeta    ,dtdeta    ,
     *                  dfluxx    ,dfluxy                          )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE DIFACR
!           Function: Computes horizontal diffusion along Z-planes.
!                     Explicit in u-direction, explicit in v-
!                     direction.
!                     Horizontal gradients salinity and temperature
!                     are computed in subroutine dengra.
!                     Only if KMAX > 1 and Anti Creep
!        Method used: Reference : On the approximation of horizontal
!                     gradients in sigma co-ordinates for bathymetry
!                     with steep bottom slopes (G.S. Stelling and J.
!                     van Kester - International Journal for Methods
!                     in Fluids, Vol. 18 1994)
!               Date: 19-07-2000
!         Programmer: G.S. Stelling, J. v. Kester, A.J. Mourits
!         CVS header
!            $Author: Beek_j $
!              $Date: 22-09-03 15:46 $
!            $Source: /u/trisula/cvsroot/trisula/reken/difacr.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routines:              DIFU
!                                  DIFUEX
!                                  DIFUIM
!                                  DIFUVL
!-----------------------------------------------------------------------
!   Called  routines:              DIFHOR
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! DDKL    --  R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Internal work array, diagonal space
!                                  at (n,m,k,l)
! DFLUXX  IO  R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Work array for horizontal diffusion flux
! DFLUXY  IO  R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Work array for horizontal diffusion flux
! DICUV   --  R*4  J:NMMAXJ,KMAX   Horizontal diffusion coeff. [m2/s]
! DPS     I   R*4  J:NMMAXJ        Depth value at zeta points
! DSDETA  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient salinity,
!                                  strictly horizontal in eta-direction
!                                  For Anti Creep: Contribution diffu-
!                                  sive flux interpolated on Cartesian
!                                  grid to cell NM
! DSDKSI  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient salinity,
!                                  strictly horizontal in ksi-direction
!                                  For Anti Creep: Contribution diffu-
!                                  sive flux interpolated on Cartesian
!                                  grid to cell NM
! DTDETA  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient temperature,
!                                  strictly horizontal in eta-direction
!                                  For Anti Creep: Contribution diffu-
!                                  sive flux interpolated on Cartesian
!                                  grid to cell NM
! DTDKSI  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient temperature,
!                                  strictly horizontal in ksi-direction
!                                  For Anti Creep: Contribution diffu-
!                                  sive flux interpolated on Cartesian
!                                  grid to cell NM
! GUU     --  R*4  J:NMMAXJ        Grid distance in the eta-/y-direction
!                                  at u-velocity point
! GUV     --  R*4  J:NMMAXJ        Grid distance in the eta-/y-direction
!                                  at v-velocity point
! GVU     --  R*4  J:NMMAXJ        Grid distance in the ksi-/x-direction
!                                  at u-velocity point
! GVV     --  R*4  J:NMMAXJ        Grid distance in the ksi-/x-direction
!                                  at v-velocity point
! ICX     I   I*4                  Increment in the x-dir., if icx= nmax
!                                  then computation proceeds in the x-
!                                  dir. if icx=1 then computation pro-
!                                  ceeds in the y-dir.
! ICY     I   I*4                  Increment in the y-dir. (see icx)
! J       I   I*4                  Begin pointer for arrays which have
!                                  been transformed into 1d arrays.
!                                  due to the shift in the 2nd (m-)
!                                  index, j = -2*nmax + 1
! KADU    --  I*4  J:NMMAXJ,KMAX   Mask array for adv. term adjustment
!                                  for structures in U-points
!                                  = 1 no structure (HYD)
!                                  = 1 no gate (TRA)
!                                  = 0 structure
!                                  = 0 gate (KSPU(NM,0)*KSPU(NM,K)=4)
! KADV    --  I*4  J:NMMAXJ,KMAX   Mask array for adv. term adjustment
!                                  for structures in V-points
!                                  = 1 no structure (HYD)
!                                  = 1 no gate (TRA)
!                                  = 0 structure
!                                  = 0 gate (KSPV(NM,0)*KSPV(NM,K)=4)
! KCS     --  I*4  J:NMMAXJ        Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  J:NMMAXJ        Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  J:NMMAXJ        Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KMAX    I   I*4                  Number of layers in the z-dir.
! LSAL    --  I*4                  Pointer for salinity in array r
!                                  for constituents (if used, always 1)
! LSTSCI  I   I*4                  Number of Constituents (Salinity,
!                                  Temperature, Sediment, Conservative
!                                  Constituents and Secondary Flow)
! LTEM    --  I*4                  Pointer for temperature in array r
!                                  for constituents (lsal+1)
! NMMAX   --  I*4                  Total number of grid pts. (nmax*mmax)
! NMMAXJ  I   I*4                  End   pointer for arrays which have
!                                  been transformed into 1d arrays.
!                                  due to the shift in the 2nd (m-)
!                                  index, nmmaxj = nmmax + 2 * nmax
! R0      --  R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Concentrations at old time level
! S0      I   R*4  J:NMMAXJ        Zeta at old time level
! SIG     --  R*4  KMAX            Sigma coordinate density points.
! SIGDIF  --  R*4  LSTSCI          Prandtl/schmidt-numbers for const.
! THICK   --  R*4  KMAX            Relative layer thickness
!-----------------------------------------------------------------------
!    local variables:
!    ----------------
!
!   var.      type dimensions
!   -------------------------
!
! AREA        R*4
! CL          R*4
! CR          R*4
! DIFL        R*4
! DIFR        R*4
! FLUX        R*4
! K           I*4
! KENU        I*4
! KENV        I*4
! L           I*4
! NM          I*4
! NMU         I*4
! NUM         I*4
!-----------------------------------------------------------------------
!
! declaration
!
!-----------------------------------------------------------------------
!     GLOBAL DATA
!
!     global data structure definition and access functions
!     include 'globdat.igd'
!-----------------------------------------------------------------------
      use timers

      real        s0    (j:nmmaxj),dps   (j:nmmaxj),
     *            guu   (j:nmmaxj),gvv   (j:nmmaxj),
     *            guv   (j:nmmaxj),gvu   (j:nmmaxj)
!
      integer     kcs   (j:nmmaxj),
     *            kfu   (j:nmmaxj),kfv   (j:nmmaxj),
     *            kadu  (j:nmmaxj,  kmax),
     *            kadv  (j:nmmaxj,  kmax)
!
      real        dicuv (j:nmmaxj,kmax),
     *            r0    (j:nmmaxj,kmax,lstsci),
     *            dsdksi(j:nmmaxj,kmax),dsdeta(j:nmmaxj,kmax  ),
     *            dtdksi(j:nmmaxj,kmax),dtdeta(j:nmmaxj,kmax  ),
     *            ddkl  (j:nmmaxj,kmax,lstsci),
     *            thick (kmax),sigdif(lstsci) ,
     *            sig   (kmax)

      real        dfluxx(j:nmmaxj,kmax,lstsci),
     *            dfluxy(j:nmmaxj,kmax,lstsci)

      integer(4) ithandl /0/

      integer num, nm, nmu, nmmax, nmmaxj
      integer k, j, kmax, icx, icy
      integer lsal, lstsci, ltem
      integer dpnum, dpnm, dpnmu
      
      real sepnum, sepnm, sepnmu

      if ( timon ) call timstrt ( "difacr", ithandl )

      dfluxx = 0.0
      dfluxy = 0.0

!
!***horizontal diffusion in both u- and v-diffusion
!   artificial creep is avoided by use
!   of a special limiter
!
!***calibration for SAL / TEMP in routine DENGRA
!
!$dir scalar
      if (lsal   .ne. 0) then
        do 10 nm=1,nmmax
          do 20 k=1,kmax
            ddkl(nm ,k,lsal)=ddkl(nm ,k,lsal)+dsdksi(nm,k)+
     *                                        dsdeta(nm,k)
 20       continue
 10     continue
      endif
      if (ltem   .ne. 0) then
        do 50 nm=1,nmmax
          do 60 k=1,kmax
            ddkl(nm ,k,ltem)=ddkl(nm ,k,ltem)+dtdksi(nm,k)+
     *                                        dtdeta(nm,k)
 60       continue
 50     continue
      endif
!
      if (lstsci.eq.MAX (lsal,ltem)) goto 1000
!
!***calibration for all other constituents
!   horizontal diffusion in u-diffusion
!   artificial creep is avoided by use
!   of a special limiter
!
      nmu = icx
      do 150 nm=1,nmmax
        nmu =nmu+1
        if (kfu(nm).ne.0) then
          sepnm =s0 (nm)
          sepnmu=s0 (nmu)
          dpnm  =dps(nm)
          dpnmu =dps(nmu)
          call   DIFHOR(nm        ,nmu       ,j         ,nmmaxj    ,
     *                  kmax      ,lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kadu      ,
     *                  sepnm     ,sepnmu    ,dpnm      ,dpnmu     ,
     *                  guu       ,gvu       ,
     *                  r0        ,ddkl      ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dfluxx                                     )

        endif
 150  continue
!
!***calibration for all other constituents
!   horizontal diffusion in v-direction
!   artificial creep is avoided by use
!   of a special limiter
!
      num = icy
      do 250 nm=1,nmmax
        num  =num+1
        if (kfv(nm).ne.0) then
          sepnm =s0 (nm)
          sepnum=s0 (num)
          dpnm  =dps(nm)
          dpnum =dps(num)
          call   DIFHOR(nm        ,num       ,j         ,nmmaxj    ,
     *                  kmax      ,lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kadv      ,
     *                  sepnm     ,sepnum    ,dpnm      ,dpnum     ,
     *                  gvv       ,guv       ,
     *                  r0        ,ddkl      ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dfluxy                                     )

        endif
 250  continue
!
 1000 continue
      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_difacr
