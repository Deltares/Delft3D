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
      module m_difhor

      implicit none

      contains


      subroutine difhor(nm        ,nmu       ,j         ,nmmaxj    ,
     *                  kmax      ,lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kadu      ,
     *                  sepnm     ,sepnmu    ,dpnm      ,dpnmu     ,
     *                  guu       ,gvu       ,
     *                  r0        ,ddkl      ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dflux                                      )

!-----------------------------------------------------------------------
!             Module: SUBROUTINE DIFHOR
!           Function: computes horizontal diffusion along strict
!                     horizontal planes, using a special limiter,
!                     to avoid 'artificial vertical diffusion'.
!                     Recently the limiter was improved.The new
!                     limiter is following Van Leer's scheme for
!                     advection and preserves monotony.
!                     explicit in u-direction, explicit in v-
!                     direction. Fluxes computed in subroutine
!                     dengra are used.
!        Method used: Reference : On the approximation of horizontal
!                     gradients in sigma co-ordinates for bathymetry
!                     with steep bottom slopes (G.S. Stelling and J.
!                     van Kester - International Journal for Methods
!                     in Fluids, Vol. 18 1994) and the discussion
!                     in "A comparison of two 3D shallow
!                     water models using sigma-coordinates and
!                     z-coordinates in the vertical direction."
!                     (M.D. Bijvelds, J. van Kester and G.S. Stelling -
!                      Proceedings 6-th International Conference on
!                      estuarine and coastal modelling, New Orleans,
!                      september 1999)
!               Date: 29-11-2001
!         Programmer: G.S. Stelling, J.A.T.M. van Kester, M.D. Bijvelds
!         CVS header
!            $Author: Beek_j $
!              $Date: 22-09-03 15:46 $
!            $Source: /u/trisula/cvsroot/trisula/reken/difhor.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routines:                DIFACR
!-----------------------------------------------------------------------
!   Called  routines:                NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! DDKL    IO  R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Internal work array, right hand side
!                                  at (n,m,k,l)
! DFLUX   IO  R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Work array for horizontal diffusion flux
! DICUV   I   R*4  J:NMMAXJ,KMAX   Horizontal diffusion coeff. [m2/s]
! GUU     I   R*4  J:NMMAXJ        Grid distance in the eta-/y-direction
!                                  at u-velocity point
! GVU     I   R*4  J:NMMAXJ        Grid distance in the ksi-/x-direction
!                                  at u-velocity point
! J       I   I*4                  Begin pointer for arrays which have
!                                  been transformed into 1d arrays.
!                                  due to the shift in the 2nd (m-)
!                                  index, j = -2*nmax + 1
! KADU    I   I*4  J:NMMAXJ,KMAX   Mask array for adv. term adjustment
!                                  for structures in U-points
!                                  = 1 no structure (HYD)
!                                  = 1 no gate (TRA)
!                                  = 0 structure
!                                  = 0 gate (KSPU(NM,0)*KSPU(NM,K)=4)
! KCS     I   I*4  J:NMMAXJ        Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KMAX    I   I*4                  Number of layers in the z-dir.
! LSAL    I   I*4                  Pointer for salinity in array r
!                                  for constituents (if used, always 1)
! LSTSCI  I   I*4                  Number of Constituents (Salinity,
!                                  Temperature, Sediment, Conservative
!                                  Constituents and Secondary Flow)
! LTEM    I   I*4                  Pointer for temperature in array r
!                                  for constituents (lsal+1)
! NMMAX   I   I*4                  Total number of grid pts. (nmax*mmax)
! NMMAXJ  I   I*4                  End   pointer for arrays which have
!                                  been transformed into 1d arrays.
!                                  due to the shift in the 2nd (m-)
!                                  index, nmmaxj = nmmax + 2 * nmax
! R0      I   R*4  J:NMMAXJ,KMAX,LSTSCI
!                                  Concentrations at old time level
! SIGDIF  I   R*4  LSTSCI          Prandtl/schmidt-numbers for const.
! THICK   I   R*4  KMAX            Relative layer thickness
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! AREA        R*4
! CL          R*4
! CR          R*4
! DIFL        R*4
! DIFR        R*4
! FLUX        R*4
! K           I*4
! KICOL       I*4  2*MXKMAX+1      K-index concentration point left
!                                  for flux kf
! KICOR       I*4  2*MXKMAX+1      K-index concentration point right
!                                  for flux kf
! L           I*4                  Constituent number
! NM          I*4                  Index gridpoint
! NMU         I*4                  Index neighbour
! NUM         I*4
! POCOL       R*4  0:MXKMAX        Z-coordinate concentr. point nm ,k
! POCOR       R*4  0:MXKMAX        Z-coordinate concentr. point nmu,k
! POFLU       R*4  2*MXKMAX+1       Z-coordinate gradient flux
! POINT       R*4  0:2*MXKMAX+1    Merge arrays polal and polar
! POLAL       R*4  0:MXKMAX        Z-coordinate horizontal layers in nm
! POLAR       R*4  0:MXKMAX        Z-coordinate horizontal layers in nmu
!-----------------------------------------------------------------------
!
! declaration
!
!-----------------------------------------------------------------------
!     GLOBAL DATA
!
!     global data structure definition and access functions
!      include 'globdat.igd'
!     global data structure definition and access functions
!      include    'pardef.igd'
!-----------------------------------------------------------------------
!
      use timers

      integer     kcs   (j:nmmaxj),
     *            kadu  (j:nmmaxj,  kmax)
!
      real        guu   (j:nmmaxj),gvu   (j:nmmaxj),
     *            r0    (j:nmmaxj,kmax,lstsci),
     *            ddkl  (j:nmmaxj,kmax,lstsci),
     *            thick (  kmax  ),sig   (  kmax  ),
     *            dicuv (j:nmmaxj,kmax),sigdif(lstsci)

      real        dflux (j:nmmaxj,kmax,lstsci)
!
      real   , allocatable, save :: polal (:),polar (:),pocol (:),pocor (:),
     *                              poflu (:),point (:)
      integer, allocatable, save :: kicol (:),kicor (:)
      logical, save              :: first = .true.
      integer(4) ithandl /0/

      integer ltem, lstsci, lsal
      integer dpnm, dpnmu
      real sepnm, sepnmu

      integer l, j
      integer k, k1, k2, kf, kmax, kpoint, krr, kll, kflux
      integer nm, nmu, nmmaxj

      real cl, cr, h0, flux, difl, difr
      real grad, grad1, grad2, grmax, grmin, grmindpnm

      if ( timon ) call timstrt ( "difhor", ithandl )
!
      if ( first ) then
         first = .false.
         allocate ( polal (0:kmax  ),polar (0:  kmax  ),
     *              pocol (  kmax  ),pocor (    kmax  ),
     *              poflu (2*kmax+1),point (0:2*kmax+1)   )
         allocate ( kicol (2*kmax+1),kicor (  2*kmax+1)   )
      endif
!
!***initialisation
!
!***position horizontal interfaces left
!
      polal (0)=sepnm
      h0       =MAX (sepnm+dpnm,0.01)
!$dir scalar
      do 20 k=1,kmax
         polal (k)=(sig(k)-0.5*thick(k))*h0+sepnm
         pocol (k)=0.5*(polal(k-1)+polal(k))
 20   continue
!
!***position horizontal interfaces right
!
      polar (0)=sepnmu
      h0       =MAX (sepnmu+dpnmu,0.01)
!$dir scalar
      do 30 k=1,kmax
         polar (k)=(sig(k)-0.5*thick(k))*h0+sepnmu
         pocor (k)=0.5*(polar(k-1)+polar(k))
 30   continue
!
!***merge polal and polar
!
      kll=0
      krr=0
!$dir scalar
      do 40 k=0,2*kmax+1
         if (polal(kll).gt.polar(krr)) then
            point(k)=polal(kll)
            kll     =kll+1
            if (kll.gt.kmax) then
               kpoint=k+1
               point(kpoint)=polar(krr)
               goto 41
            endif
         else
            point(k)=polar(krr)
            krr   =krr +1
            if (krr.gt.kmax) then
               kpoint=k+1
               point(kpoint)=polal(kll)
               goto 41
            endif
         endif
  40  continue
      kpoint=2*kmax+1
  41  continue
!
!***position flux points
!
      kflux=kpoint
!$dir scalar
      do 51 k=1,kflux
         poflu(k)=0.5*(point(k)+point(k-1))
  51  continue
!
!***k-index concentration points left and right for flux point
!
      kll=1
      krr=1
!$dir scalar
      do 80 kf=1,kflux
         kicol(kf)=0
         kicor(kf)=0
!$dir scalar
         do 60 k=kll,kmax
            if (poflu(kf).le.polal(k-1).and.poflu(kf).ge.polal(k)) then
               kicol(kf)=k
               kll      =k
               goto 61
            endif
  60     continue
  61     continue
!$dir scalar
         do 70 k=krr,kmax
            if (poflu(kf).le.polar(k-1).and.poflu(kf).ge.polar(k)) then
               kicor(kf)=k
               krr      =k
               goto 71
            endif
  70     continue
  71     continue
  80  continue
!
!***computation diffusive flux using
!   limiter
!
!$dir scalar
      do 200 l= MAX (lsal,ltem)+1,lstsci
!
!***computation of flux
!
!$dir scalar
      do 100 kf=1,kflux
        kll =kicol(kf)
        krr =kicor(kf)
        if (kll.ne.0.and.krr.ne.0) then
          if (kadu(nm,kll)*kadu(nm,krr) .ne. 0) then
!
!***interpolation left point
!
            k2=kll
            if (pocor(krr).gt.pocol(kll)) then
              k1=k2-1
              if (k1.lt.1) then
                cl=r0(nm,k2,l)
              else
                cl=((pocor(krr)-pocol(k2))/(pocol(k1)-pocol(k2)))*
     *               r0(nm,k1,l)+
     *             ((pocor(krr)-pocol(k1))/(pocol(k2)-pocol(k1)))*
     *               r0(nm,k2,l)
              endif
            else
              k1=k2+1
              if (k1.gt.kmax) then
                cl=r0(nm,k2,l)
              else
                cl=((pocor(krr)-pocol(k2))/(pocol(k1)-pocol(k2)))*
     *               r0(nm,k1,l) +
     *             ((pocor(krr)-pocol(k1))/(pocol(k2)-pocol(k1)))*
     *               r0(nm,k2,l)
              endif
            endif
!
!***interpolation left point
!
            k2=krr
            if (pocol(kll).gt.pocor(k2)) then
              k1=k2-1
              if (k1.lt.1) then
                cr=r0(nmu,k2,l)
              else
                cr=((pocol(kll)-pocor(k2))/(pocor(k1)-pocor(k2)))*
     *               r0(nmu,k1,l)+
     *             ((pocol(kll)-pocor(k1))/(pocor(k2)-pocor(k1)))*
     *               r0(nmu,k2,l)
              endif
            else
              k1=k2+1
              if (k1.gt.kmax) then
                cr=r0(nmu,k2,l)
              else
                cr=((pocol(kll)-pocor(k2))/(pocor(k1)-pocor(k2)))*
     *               r0(nmu,k1,l)+
     *             ((pocol(kll)-pocor(k1))/(pocor(k2)-pocor(k1)))*
     *               r0(nmu,k2,l)
              endif
            endif
            grad1=r0(nmu,krr,l)-cl
            grad2=cr-r0(nm,kll,l)
            grmax=max(grad1,grad2)
            grmin=min(grad1,grad2)
!
!***new limiter following Van Leer
!
            if (grmax.ge.0.0.and.grmin.le.0.0) then
               grad=0.0
            else
               grad = 2.* grad1*grad2/(grad1+grad2)
            endif
!
!***flux
!
            difl=dicuv(nm ,kll)
            difr=dicuv(nmu,krr)
            flux=0.5*(point(kf-1)-point(kf))*grad*guu(nm)*
     *               (difl+difr)/sigdif(l)/gvu(nm)
            dflux(nm ,kll,l)=dflux(nm ,kll,l)+flux*MAX(0,2-kcs(nmu))
            dflux(nmu,krr,l)=dflux(nmu,krr,l)-flux*MAX(0,2-kcs(nm ))
            ddkl(nm ,kll,l)=ddkl(nm ,kll,l)+flux*MAX(0,2-kcs(nmu))
            ddkl(nmu,krr,l)=ddkl(nmu,krr,l)-flux*MAX(0,2-kcs(nm ))
          endif
        endif
  100 continue
  200 continue
!
      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_difhor
