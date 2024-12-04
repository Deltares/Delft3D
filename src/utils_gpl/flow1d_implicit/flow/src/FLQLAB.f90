subroutine FLQLAB( steady, ngrid , ngridm, i1    , i2    ,&
&theta , theta2, relstr, q     ,&
&q1    , qltpar,&
&cflpsa, x     , qlat  , a1    ,&
&b1    , c1    , d1    , e1    , dt1   ,&
&nqlat )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         P.R.Evans
!
! Module:             FLQLAB (FLow QLat ABcde)
!
! Module description: Subroutine FLQLAB calculates the coefficients
!                     a, b, c, d and e resulting from lateral discharges
!                     which are used in the continuity equation. All
!                     types of lateral discharge are handled here.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!
!
!
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqlab.pf,v $
! Revision 1.1  1997/11/04  14:17:26  kuipe_j
! Retention basin
!
!
!
!***********************************************************************
!
! declaration of Parameters:
!
   logical steady
   integer ngrid , ngridm, nqlat , i1    , i2
   real    theta , theta2, relstr
   real    qltpar(9, nqlat),&
   &cflpsa(ngrid),&
   &x(ngrid)      , qlat(nqlat,9)

   double precision q(ngrid), q1(ngrid)

   double precision a1(ngridm),&
   &b1(ngridm),&
   &c1(ngridm),&
   &d1(ngridm),&
   &e1(ngridm),&
   &dt1
!
! declaration of local variables:
!
   integer istat , first , last  , iopt  ,&
   &i     , k
   real    dt2   , debi  , dtrp  ,&
!     &        debi1 , Pseudo, Uf    , T2ufot,
!     &        Ccc   , dx    , dtpse ,
   &debi1 ,  dx   , dtpse ,&
   &term  , astr  , bstr  , cstr  ,&
   &estr
   double precision aterm , bterm , cterm ,&
   &dterm , eterm
   real    thetmp, thetm2, thetql
!
! include sobek constants
!
   include '..\include\sobcon.i'

   if ( steady ) then
      dt2    = 1.0E6
   else
      dt2    = sngl(dt1)
   endif
!
   thetql = 1.0
   thetmp = theta
   thetm2 = theta2
   theta  = thetql
   theta2 = thetql

!
   do 10 istat = 1, nqlat
      first = INT(qltpar(5,istat))
      last  = INT(qltpar(6,istat))
      if (first .ge. i1 .and. last .le. i2) then
!
! qlat in this branch
! now check type of lateral discharge
!
         iopt  = INT(qltpar(2,istat))
         if (iopt .eq. cqlret) then
!
! first determine a to e
!
            i      = first
            debi   = sngl( theta2*q(i  ) + (1.0-theta2) * q1(i  ) )
            debi1  = sngl( theta2*q(i+1) + (1.0-theta2) * q1(i+1) )

            dx = x(i+1) - x(i)
! ARS 6341  dtpse = dx**2 / (pseudo * ccc)
            dtpse = dx / cflpsa(i)

            qlat(istat,8) = dtpse
!
!   now calculate structure terms
!
            astr = qlat(istat,4)
            bstr = qlat(istat,5)
            cstr = qlat(istat,6)
            estr = qlat(istat,7)
!
            dtrp = 1. / dt2 + 1. / dtpse
            term = theta * dtrp&
!                                                       S
            &/ ((bstr - 1.) * dtrp - theta * cstr / qltpar(3,istat))

            aterm = dble (term * astr / 2.)
            bterm = 0.0D0
            cterm = aterm
            dterm = 0.0D0
!                         Qr_n
            eterm = dble (qlat(istat,3) + term *&
!                                          Qr_n+1,m-1      Qr_n
            &( estr - (1. - relstr) * qlat(istat,2) + qlat(istat,3)&
!                           S
            &- cstr / (qltpar(3,istat) * dtrp)&
!                     Qr_n            S
            &* (-qlat(istat,3) + qltpar(3,istat)&
!                            Hr_n+1,m          Hr_n
            &/ dtpse * (qltpar(8,istat) - qlat(istat,9)))))

            k = first - i1 + 1
            a1(k) = a1(k) + aterm
            b1(k) = b1(k) + bterm
            c1(k) = c1(k) + cterm
            d1(k) = d1(k) + dterm
!           remove part of lateral discharge due to 'retentie-bekken'
!           that was already added in FLNORM
!                                   Qr_n+1,m
            e1(k) = e1(k) + eterm - qlat (istat,1)

         endif
      endif
10 continue

   theta  = thetmp
   theta2 = thetm2

end
