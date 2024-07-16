      subroutine gsacel (nlayer ,nfrac  ,ngrid  ,igr    ,width  ,duda  ,
     &                   delta  ,deltb  ,deltc  ,fdi    ,depos  ,ptrla2,
     &                   pdiacc ,pexla2 ,p0la   ,sedtr  ,dunel  ,deff2 ,
     &                   dfrac  ,pdspdd ,dsdu   ,cela0  ,cela1a ,cela1b,
     &                   cela2  ,cela3  ,cela   ,celb   ,celc   ,
     &                   nunlay ,nrdzdl ,sedtrp )
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsacel.F,v $
c Revision 1.4  1996/06/07  11:55:35  kuipe_j
c multi  +  fixed layer
c
c Revision 1.3  1996/01/08  13:29:42  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:11:46  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c  Module:     Graded Sediment calculate coefficient A for CELerity
c
c     Declaration of parameters
c

      integer    nlayer ,nfrac  ,ngrid  ,igr
      integer    nunlay
      integer    nrdzdl(ngrid)
      real       width  ,duda   ,delta  ,deltb  ,deltc  ,
     &           fdi
      real       ptrla2 (ngrid,nfrac)   ,pexla2 (ngrid,nfrac)  ,
     &           sedtr  (ngrid,nfrac+2) ,cela0  (ngrid,nfrac)  ,
     &           cela1a (ngrid,nfrac)   ,cela1b (ngrid,nfrac)  ,
     &           cela2  (ngrid,nfrac)   ,cela3  (ngrid,nfrac)  ,
     &           pdiacc (nfrac)         ,sedtrp (nfrac)        ,
     &           p0la   (ngrid,nfrac,nunlay)    ,
     &           dunel  (ngrid)         ,deff2  (ngrid)        ,
     &           dfrac  (nfrac)         ,pdspdd (nfrac)        ,
     &           dsdu   (nfrac)         ,cela   (nfrac)        ,
     &           celb   (nfrac)         ,celc   (nfrac)
      logical    depos  (ngrid)
c 
c     Declaration of local variables
c
      integer    jf
      real       radelt ,tmp    ,phi    ,sp     ,sum0   ,sum2  ,sum3
c
c     Declaration of constants
c
      real       gamma      
      parameter (gamma=0.08 )
c
c     Calculate coefficients Aj, Bj, and Cj
c
      do 10 jf=1,nfrac
         if (nlayer .eq. 1) then
            if (depos(igr)) then
               cela(jf) = ptrla2(igr,jf)
               celb(jf) = 0.
               celc(jf) = 0.
            else
               tmp      = (ptrla2(igr,jf) - p0la(igr,jf,nrdzdl(igr))) *
     &                     width
               cela(jf) = p0la(igr,jf,nrdzdl(igr)) - tmp * delta
               celb(jf) = - tmp * deltb
               celc(jf) = - tmp * deltc
            endif
         else if (nlayer .eq. 2) then
            phi = gamma * sedtr(igr,nfrac+1) / dunel(igr) *
     &            (pexla2(igr,jf) - fdi * ptrla2(igr,jf) - pdiacc(jf))
            if (depos(igr)) then
               cela(jf) = ptrla2(igr,jf)
               celb(jf) = 0.
               celc(jf) = phi
            else
               tmp      = (ptrla2(igr,jf) - pexla2(igr,jf)) * width
               cela(jf) = pexla2(igr,jf) - tmp * delta
               celb(jf) = - tmp * deltb
               celc(jf) = phi - tmp * deltc
            endif
         endif
   10 continue
c
c     Calculate sums of Xj * d(Sk)/d(dm) for X=A,B and C.
c
      sum0 = 0.
      sum2 = 0.
      sum3 = 0.
      do 20 jf=1,nfrac
         sum0 = sum0 + cela(jf) * dfrac(jf)
         sum2 = sum2 + celb(jf) * dfrac(jf)
         sum3 = sum3 + celc(jf) * dfrac(jf)
   20 continue
c
c     Calculate coefficients A0ij,A1ikj,A2ij,A3ij to be transferred
c     to the morphology module.
c
      radelt = 1. / (width * deff2(igr))
      do 30 jf=1,nfrac
         sp             = sedtrp(jf)
         cela0 (igr,jf) = (sum0 * pdspdd(jf) + sp * cela(jf)) * radelt
     &                    - dsdu(jf) * duda
         cela1a(igr,jf) = sp * radelt
         cela1b(igr,jf) = pdspdd(jf) * radelt
         cela2 (igr,jf) = (sum2 * pdspdd(jf) + sp * celb(jf)) * radelt
         cela3 (igr,jf) = (sum3 * pdspdd(jf) + sp * celc(jf)) * radelt
   30 continue

      end
