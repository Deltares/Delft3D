subroutine gsdsd  (nbran  ,ngrid  ,nfrac  ,igr    ,ibr   ,g      ,&
&pacfac ,relden ,kinvis ,depth  ,width ,u      ,&
&chezy  ,nvast  ,alffl  ,nchfla ,dfrac ,nucoef ,&
&uscoef ,trform ,rs     ,grsize ,forcon,ptrla2 ,&
&sedtra ,dsdu   ,pdspdd ,zbave  ,zbfl   )
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdsd.F,v $
! Revision 1.3  1996/06/07  11:56:08  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:12:13  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
! Module:             GSDSD (Graded Sediment calculate DS/Du, ds/ddm)
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran ,ngrid   ,nfrac   ,igr    ,ibr   ,nvast  ,&
   &nchfla,nucoef
   real       g     ,pacfac  ,relden  ,kinvis ,depth ,width  ,&
   &u     ,chezy   ,alffl
   real       uscoef(nrcoefs,nucoef) ,trform(3,nbran)        ,&
   &rs    (ngrid,3),&
   &grsize(4,ngrid,nfrac+1),forcon(nfrac+1,4,nbran),&
   &ptrla2(ngrid,nfrac)    ,dfrac (nfrac)          ,&
   &sedtra(nfrac)          ,dsdu(nfrac)            ,&
   &pdspdd(nfrac)          ,&
   &zbave(ngrid)           ,zbfl(ngrid)
!
!     Declaration of local variables
!
   integer    ind   ,jf    ,i
   real       velo  ,velo1 ,dvelo ,dgr ,sedexp
   real       d1(4)
   logical    equal ,incall,snzero
   external   equal
!
!     Constants
!
   real       diff
   parameter  (diff=.001)
   integer    dmed
   parameter  (dmed=4)
!
!     Numerical differentiation of transport to u and Dm
!
   incall = .false.
   ind    = max(int(trform(2,ibr)),1)
!
   snzero = .false.
   do 10 jf=1,nfrac
      snzero = .not.equal(sedtra(jf),0.).or.snzero
10 continue
   if (snzero) then
!
!        Calculate |u+du|
!
      velo  = abs(u)
      dvelo = max(velo * diff,diff)
      velo1 = velo + dvelo
!
!        Calculate sediment transport based on u+du
!
      call gstrfo(incall  ,nfrac ,g      ,pacfac ,relden ,kinvis   ,&
      &grsize(1,igr,1),chezy  ,velo1  ,depth  ,rs(igr,1),&
      &uscoef(1,ind)  ,trform (1,ibr) ,dfrac  ,&
!                                   <s(u+du)>
      &forcon(1,1,ibr),dsdu   ,sedexp ,nvast  ,alffl    ,&
      &zbave(igr), zbfl(igr)  ,nchfla ,ngrid  ,igr      ,&
      &ptrla2    )
!
!        Multiply transport/unit-width with probabilty per
!        fraction and width
!
      do 20 jf=1,nfrac
!                           <s(u+du)>
         dsdu(jf) = sign((dsdu(jf) * ptrla2(igr,jf) * width -&
         &abs(sedtra(jf))) / dvelo , u )
20    continue
!
!        Calculate |Dm+dDm|
!
      do 30 i=1,4
         d1(i) = grsize(i,igr,1)
30    continue
      dgr      = d1(dmed) * diff
      d1(dmed) = d1(dmed) * (1.+diff)
!
!        Calculate sediment transport based on D+dDm
!
      call gstrfo(incall  ,nfrac ,g      ,pacfac ,relden ,kinvis   ,&
      &d1             ,chezy  ,u      ,depth  ,rs(igr,1),&
      &uscoef(1,ind)  ,trform (1,ibr) ,dfrac  ,&
!                                   <s(Dm+dDm)>
      &forcon(1,1,ibr),pdspdd ,sedexp ,nvast  ,alffl    ,&
      &zbave(igr), zbfl(igr)  ,nchfla ,ngrid  ,igr      ,&
      &ptrla2    )
!
!        Multiply transport/unit-width with probabilty per
!        fraction and width
!
      do 40 jf=1,nfrac
!                        <s(Dm+dDm)>
         pdspdd(jf) = (pdspdd(jf) * ptrla2(igr,jf) * width -&
         &sedtra(jf)) / dgr
40    continue
   else
!
      do 50 jf=1,nfrac
         dsdu   (jf) = 0.
         pdspdd (jf) = 0.
50    continue
   endif
!
   return
!
end
