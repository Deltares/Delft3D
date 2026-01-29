subroutine gmpred (ngrid  ,nfrac  ,igp   ,dx     ,dtm    ,alphac ,&
&alphad ,alphae ,sedtr ,source ,cela0  ,cela1a ,&
&cela1b ,cela2  ,cela3 ,dfrac  ,cela1  ,ds     ,&
&celert ,spredc ,jugralg       )
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmpred.F,v $
! Revision 1.5  1996/06/07  11:55:24  kuipe_j
! multi  +  fixed layer
!
! Revision 1.4  1996/01/08  13:29:38  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.3  1996/01/05  15:43:26  kuipe_j
! Lateral sediment and structures
!
! Revision 1.2  1995/09/27  10:11:43  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
! Module:  Graded Morphology; calculate PREDicted sediment transport
!
!          The predicted transports at I+1/2 are calculated for
!          all fractions.
!
!     Declaration of parameters
!
   integer    ngrid  ,nfrac  ,igp   ,jugralg
   real       dx     ,dtm    ,alphac,alphad ,alphae
   real       celert (ngrid)        ,&
   &sedtr  (ngrid,nfrac+2),source (ngrid,nfrac+2) ,&
   &cela0  (ngrid,nfrac)  ,cela1a (ngrid,nfrac)   ,&
   &cela1b (ngrid,nfrac)  ,cela2  (ngrid,nfrac)   ,&
   &cela3  (ngrid,nfrac)  ,&
   &dfrac  (nfrac)        ,spredc (nfrac)         ,&
   &ds     (nfrac)        ,cela1  (nfrac,nfrac)
!
!     Declaration of local variables
!
   integer    i      ,j      ,k      ,igpp1   ,jf
   real       term0  ,term1  ,term2  ,term3   ,dst     ,ddm   ,&
   &cya1as ,cya1bs ,rdx    ,omupw   ,cour    ,acour ,&
   &upwfac ,sum    ,sum1   ,spj     ,smax    ,factor,&
   &factor1
!
   igpp1 = igp + 1
!
!     Fill auxilliary matrix A1 (Dm is used)
!
   do 20  i=1,nfrac
      cya1as = cela1a(igp,i) + cela1a(igpp1,i)
      cya1bs = cela1b(igp,i) + cela1b(igpp1,i)
      do 10  j=1,nfrac
         cela1(i,j) = cya1bs * dfrac(j)
10    continue
      cela1(i,i) = cela1(i,i) + cya1as
20 continue
!
!     Calculate loop constants
!
   rdx = 1./dx
   dst =  (sedtr (igpp1,nfrac+1) - sedtr (igp,nfrac+1)&
   &- source(igp,nfrac+1)) * rdx
   ddm =  (sedtr (igpp1,nfrac+2) - sedtr (igp,nfrac+2)&
   &- source(igp,nfrac+2)) * rdx

   do 30 jf=1,nfrac
      ds(jf) = (sedtr (igpp1,jf) - sedtr (igp,jf)&
      &- source(igp,jf)) * rdx
30 continue
!
!     Calculate prediction per fraction
!     The prediction of the transport s* on time step n+1 can be
!     expressed as:
!     s*(n+1) = s(n) + DELTA-s
!     Calculate DELTA-s per fraction. First according
!     to Lax Wendroff
!
   do 50 jf = 1,nfrac
      term0 = (cela0(igp,jf) + cela0(igpp1,jf)) * dst

      term1 = 0.
      do 40 k=1,nfrac
         term1 = term1 + cela1(jf,k) * ds(k)
40    continue

      term2 = (cela2(igp,jf) + cela2(igpp1,jf)) * ddm

      term3 =  cela3(igp,jf) + cela3(igpp1,jf)
      spredc(jf) = dtm * ( term0 - term1 + term2 + term3)&
      &* alphac * 0.5
50 continue
!
!     Calculate upwind factor
!
   cour   = .5 * dtm * (celert(igp) + celert(igpp1)) * rdx
   acour  = abs(cour)
   if (acour.gt.1.0) then
      omupw = 0.0
   elseif (acour.lt.0.0) then
      omupw = alphad
   else
      omupw  = alphad * (1. - acour)
   endif
   upwfac = sign(1.,cour) * (omupw + (1.-omupw) *&
   &min((alphae-1.)*acour , 1.-acour))
!
!     The predicted transport is a mix of the upwind and the
!     Lax Wendroff scema.
!
   sum = 0.
   do 60 jf = 1,nfrac
      spj = .5 * ( sedtr (igp,jf) + sedtr (igpp1,jf) ) +&
      &(1.-omupw) * spredc(jf) +&
      &upwfac * (sedtr(igp,jf) - sedtr(igpp1,jf))
      spredc(jf) = spj
      sum        = sum + spj
60 continue
!
!     If the predicted sediment transport is far more then
!     the maximum transports, it will be limited.
!
   smax = max(abs(sedtr(igp,nfrac+1)),abs(sedtr(igpp1,nfrac+1)))
   if (abs(sum) .gt. smax * 10. ) then
      if (smax.lt.1e-10) then
         factor = 0.
         do jf = 1,nfrac
            spredc(jf) = .5 * ( sedtr (igp,jf) + sedtr (igpp1,jf))
         enddo
         SUM1=0.
      else
         factor = sum/smax
         factor1 = 10./abs(factor)
         sum1=0.
         do jf = 1,nfrac
            spredc(jf) = factor1 * spredc(jf)
            sum1 = sum1 + spredc(jf)
         enddo
      endif
      WRITE (jugralg,*) 'Predicted sediment LIMITED on grid point',&
      &igp
      WRITE (jugralg,*) '    Ratio to calculated transport =',factor
      WRITE (jugralg,*) '    S-pred was:',sum,' S-pred corrected:',&
      &sum1
   endif
end
