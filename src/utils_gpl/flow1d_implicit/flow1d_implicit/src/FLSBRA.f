      subroutine FLSBRA( steady, nqlat , ngrid , qlat  , qlatnm,
     &                   qltpar, strhis, relstr, theta , h1    , 
     &                   h2    , dt1   , nstru ) 

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         P.R.Evans
c
c Module:             FLSBRA (FLow Substitute Back for Retention Areas) 
c
c Module description: Subroutine FLSBRA uses the new hydraulic
c                     parameters calculated in FLBRAN to calculate dQ
c                     and dH at the next time level. This is achieved
c                     by substituting back into the equations for the
c                     structures and the retention area.
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
c $Log: flsbra.pf,v $
c Revision 1.1  1997/11/04  14:17:30  kuipe_j
c Retention basin
c
c
c
c***********************************************************************
c
c declaration of Parameters:
c
      logical steady

      integer nqlat, ngrid, nstru

      real    relstr, theta                
      real    thetmp, thetql
      real    qlat(nqlat,9), qltpar(9,nqlat), strhis(13,nstru)
      double precision h1(ngrid), h2(ngrid), dt1

      character*40 qlatnm(*)

c
c declaration of local variables:
c
      integer iqlat, istat, istru, i, iret1, iret2, iql, type
      real    astr, bstr, cstr, estr, dtpse, term, dq, dh, dt
c
c include sobek constants 
c
      include '../include/sobcon.i'

      if ( steady ) then
        dt = 1.0E6
      else
        dt = sngl(dt1)
      endif

      thetql = 1.0
      thetmp = theta
      theta  = thetql

      do 10 istat = 1, nqlat

        type = INT(qltpar(2,istat))

        if (type .eq. cqlret) then

          istru = MOD(INT(qltpar(9,istat)), 1000)
          i     = INT(qltpar(5,istat))

          astr  = qlat(istat,4)
          bstr  = qlat(istat,5)
          cstr  = qlat(istat,6)
          estr  = qlat(istat,7)
          dtpse = qlat(istat,8)
c
c         For back substitution use Eq 9 of memo of Borsboom Oct. 1997
c
c                  S
          term = qltpar(3,istat) * (1. / dt + 1. / dtpse) 
          dq = -astr / 2. * (h2(i) - h1(i) + h2(i+1) - h1(i+1))  
c                                      Qr_n+1,m-1     Qr_n       
     &       + estr - (1. - relstr) * qlat(istat,2) + qlat(istat,3)
c                               Qr_n             S
     &       - cstr / term * (-qlat(istat,3) + qltpar(3,istat)
c                        Hr_n+1,m          Hr_n   
     &       / dtpse * (qltpar(8,istat) - qlat(istat,9)))
          dq = dq / (bstr - 1. - theta * cstr / term)

c         Shift to next iteration step (M=m+1) 
c         Qr_n+1,M-1      Qr_n+1,m
          qlat(istat,2) = qlat(istat,1)
c         Qr_n+1,M        Qr_n
          qlat(istat,1) = qlat(istat,3) + dq
c
c         For back substitution use Eq 7 of memo of Borsboom Oct. 1997
c
c                Qr_n            S        
          dh = (-qlat(istat,3) + qltpar(3,istat) 
c                        Hr_n+1,m          Hr_n   
     &       / dtpse * (qltpar(8,istat) - qlat(istat,9)) - theta * dq )
     &       / term
c
c         Hr_n+1,m+1          Hr_n
          strhis(13,istru) = qlat(istat,9) + dh 
c
c         Shift to next iteration step (M=m+1)
c         Hr_n+1,M            Hr_n+1,m+1
          qltpar(8,istat)  = strhis(13,istru)
        endif
 10   continue


!     ARS 11484
!     Aanpassingen voor retentie feb, maart 2006
!     Retentiebekkens met toe- en afvoer op aparte locaties
!     Doorloop de namen van de retentiestructures 
!     en bepaal de nummers. Als twee namen "hetzelfde" beginstuk hebben dan
!     moeten de waterstanden gemiddeld worden tot één nieuwe waterstand

!     --- Run through all lateral indicators'

      do iqlat = 1, nqlat

         iret1 = 0
         iret2 = 0

         if ( INT(qltpar(2,iqlat)) == 5 ) then 

!           --- Retention: check name

            if ( qlatnm(iqlat)(1:4) == 'Ret-' ) then

!              --- Check whether there are related names

               iret1 = iqlat
         
!              --- Run through sequence for related name

               do iql = iqlat+1, nqlat

                  if ( qlatnm(iql)(1:7) == qlatnm(iqlat)(1:7) ) then

                     if ( iret2 == 0 ) iret2 = iql

                  endif

               enddo

               if ( iret2 > 0 ) then

                  iret1 = qltpar(9,iret1)
                  iret2 = qltpar(9,iret2)
                  strhis(13,iret1) = ( strhis(13,iret1) + 
     +                                 strhis(13,iret2) ) / 2d0
                  strhis(13,iret2) =   strhis(13,iret1)     

               endif

            endif

         endif

      enddo

      theta = thetmp

      end
