      subroutine FLQLAB( steady, ngrid , ngridm, i1    , i2    ,
     &                   theta , theta2, relstr, q     ,
     &                   q1    , qltpar,       
     &                   cflpsa, x     , qlat  , a1    ,
     &                   b1    , c1    , d1    , e1    , dt1   ,
     &                   nqlat )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         P.R.Evans
c
c Module:             FLQLAB (FLow QLat ABcde) 
c
c Module description: Subroutine FLQLAB calculates the coefficients
c                     a, b, c, d and e resulting from lateral discharges
c                     which are used in the continuity equation. All      
c                     types of lateral discharge are handled here. 
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c
c
c
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flqlab.pf,v $
c Revision 1.1  1997/11/04  14:17:26  kuipe_j
c Retention basin
c
c
c
c***********************************************************************
c
c declaration of Parameters:
c
      logical steady
      integer ngrid , ngridm, nqlat , i1    , i2 
      real    theta , theta2, relstr
      real    qltpar(9, nqlat),
     &        cflpsa(ngrid),
     &        x(ngrid)      , qlat(nqlat,9)
      
      double precision q(ngrid), q1(ngrid)

      double precision a1(ngridm),
     &                 b1(ngridm),
     &                 c1(ngridm),
     &                 d1(ngridm),
     &                 e1(ngridm),
     &                 dt1
c
c declaration of local variables:
c
      integer istat , first , last  , iopt  ,
     &        i     , k      
      real    dt2   , debi  , dtrp  ,
c     &        debi1 , Pseudo, Uf    , T2ufot,
c     &        Ccc   , dx    , dtpse ,
     &        debi1 ,  dx   , dtpse ,
     &        term  , astr  , bstr  , cstr  ,
     &        estr  
      double precision aterm , bterm , cterm ,
     &                 dterm , eterm  
      real    thetmp, thetm2, thetql
c
c include sobek constants
c
      include '../include/sobcon.i'

      if ( steady ) then
        dt2    = 1.0E6
      else
        dt2    = sngl(dt1)
      endif
c
      thetql = 1.0
      thetmp = theta
      thetm2 = theta2
      theta  = thetql
      theta2 = thetql

c
      do 10 istat = 1, nqlat
        first = INT(qltpar(5,istat)) 
        last  = INT(qltpar(6,istat)) 
        if (first .ge. i1 .and. last .le. i2) then
c
c qlat in this branch
c now check type of lateral discharge
c
          iopt  = INT(qltpar(2,istat))
          if (iopt .eq. cqlret) then
c
c first determine a to e
c
            i      = first
            debi   = sngl( theta2*q(i  ) + (1.0-theta2) * q1(i  ) )
            debi1  = sngl( theta2*q(i+1) + (1.0-theta2) * q1(i+1) )

            dx = x(i+1) - x(i)
c ARS 6341  dtpse = dx**2 / (pseudo * ccc) 
            dtpse = dx / cflpsa(i)
            
            qlat(istat,8) = dtpse
c
c   now calculate structure terms
c
            astr = qlat(istat,4)
            bstr = qlat(istat,5)
            cstr = qlat(istat,6)
            estr = qlat(istat,7)
c
            dtrp = 1. / dt2 + 1. / dtpse
            term = theta * dtrp
c                                                       S
     &           / ((bstr - 1.) * dtrp - theta * cstr / qltpar(3,istat))
  
            aterm = dble (term * astr / 2.)
            bterm = 0.0D0
            cterm = aterm
            dterm = 0.0D0
c                         Qr_n
            eterm = dble (qlat(istat,3) + term *
c                                          Qr_n+1,m-1      Qr_n       
     &            ( estr - (1. - relstr) * qlat(istat,2) + qlat(istat,3)
c                           S            
     &            - cstr / (qltpar(3,istat) * dtrp) 
c                     Qr_n            S
     &            * (-qlat(istat,3) + qltpar(3,istat)
c                            Hr_n+1,m          Hr_n   
     &            / dtpse * (qltpar(8,istat) - qlat(istat,9)))))

            k = first - i1 + 1
            a1(k) = a1(k) + aterm
            b1(k) = b1(k) + bterm
            c1(k) = c1(k) + cterm
            d1(k) = d1(k) + dterm
c           remove part of lateral discharge due to 'retentie-bekken'
c           that was already added in FLNORM
c                                   Qr_n+1,m
            e1(k) = e1(k) + eterm - qlat (istat,1)

          endif
        endif
 10   continue

      theta  = thetmp
      theta2 = thetm2

      end
