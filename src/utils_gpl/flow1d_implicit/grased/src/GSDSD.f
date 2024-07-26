      subroutine gsdsd  (nbran  ,ngrid  ,nfrac  ,igr    ,ibr   ,g      ,
     &                   pacfac ,relden ,kinvis ,depth  ,width ,u      ,
     &                   chezy  ,nvast  ,alffl  ,nchfla ,dfrac ,nucoef ,
     &                   uscoef ,trform ,rs     ,grsize ,forcon,ptrla2 ,
     &                   sedtra ,dsdu   ,pdspdd ,zbave  ,zbfl   )
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdsd.F,v $
c Revision 1.3  1996/06/07  11:56:08  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:13  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c Module:             GSDSD (Graded Sediment calculate DS/Du, ds/ddm)
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran ,ngrid   ,nfrac   ,igr    ,ibr   ,nvast  ,
     &           nchfla,nucoef
      real       g     ,pacfac  ,relden  ,kinvis ,depth ,width  ,
     &           u     ,chezy   ,alffl
      real       uscoef(nrcoefs,nucoef) ,trform(3,nbran)        ,
     &           rs    (ngrid,3),
     &           grsize(4,ngrid,nfrac+1),forcon(nfrac+1,4,nbran),
     &           ptrla2(ngrid,nfrac)    ,dfrac (nfrac)          ,
     &           sedtra(nfrac)          ,dsdu(nfrac)            ,
     &           pdspdd(nfrac)          ,
     &           zbave(ngrid)           ,zbfl(ngrid)
c
c     Declaration of local variables
c
      integer    ind   ,jf    ,i
      real       velo  ,velo1 ,dvelo ,dgr ,sedexp
      real       d1(4)
      logical    equal ,incall,snzero
      external   equal
c
c     Constants
c
      real       diff
      parameter  (diff=.001)
      integer    dmed
      parameter  (dmed=4)
c
c     Numerical differentiation of transport to u and Dm
c
      incall = .false.
      ind    = max(int(trform(2,ibr)),1)
c
      snzero = .false.
      do 10 jf=1,nfrac
         snzero = .not.equal(sedtra(jf),0.).or.snzero
   10 continue
      if (snzero) then
c
c        Calculate |u+du|
c
         velo  = abs(u)
         dvelo = max(velo * diff,diff)
         velo1 = velo + dvelo
c
c        Calculate sediment transport based on u+du
c
         call gstrfo(incall  ,nfrac ,g      ,pacfac ,relden ,kinvis   ,
     &               grsize(1,igr,1),chezy  ,velo1  ,depth  ,rs(igr,1),
     &               uscoef(1,ind)  ,trform (1,ibr) ,dfrac  ,
c                                   <s(u+du)> 
     &               forcon(1,1,ibr),dsdu   ,sedexp ,nvast  ,alffl    ,
     &               zbave(igr), zbfl(igr)  ,nchfla ,ngrid  ,igr      ,
     &               ptrla2    )
c
c        Multiply transport/unit-width with probabilty per
c        fraction and width
c
         do 20 jf=1,nfrac
c                           <s(u+du)> 
            dsdu(jf) = sign((dsdu(jf) * ptrla2(igr,jf) * width -
     &                       abs(sedtra(jf))) / dvelo , u )
   20    continue
c
c        Calculate |Dm+dDm|
c
         do 30 i=1,4
            d1(i) = grsize(i,igr,1)
   30    continue
         dgr      = d1(dmed) * diff
         d1(dmed) = d1(dmed) * (1.+diff)
c
c        Calculate sediment transport based on D+dDm
c
         call gstrfo(incall  ,nfrac ,g      ,pacfac ,relden ,kinvis   ,
     &               d1             ,chezy  ,u      ,depth  ,rs(igr,1),
     &               uscoef(1,ind)  ,trform (1,ibr) ,dfrac  ,
c                                   <s(Dm+dDm)> 
     &               forcon(1,1,ibr),pdspdd ,sedexp ,nvast  ,alffl    ,
     &               zbave(igr), zbfl(igr)  ,nchfla ,ngrid  ,igr      ,
     &               ptrla2    )
c
c        Multiply transport/unit-width with probabilty per
c        fraction and width
c
         do 40 jf=1,nfrac
c                        <s(Dm+dDm)> 
            pdspdd(jf) = (pdspdd(jf) * ptrla2(igr,jf) * width -
     &                   sedtra(jf)) / dgr 
   40    continue
      else
c
         do 50 jf=1,nfrac
            dsdu   (jf) = 0.
            pdspdd (jf) = 0.
   50    continue
      endif
c
      return
c
      end
