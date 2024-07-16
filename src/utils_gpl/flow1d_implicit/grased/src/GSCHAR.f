      subroutine gschar (ibr    ,nfrac  ,nlayer ,nbran  ,ngrid ,branch ,
     &                   ddis   ,dfrac  ,ptrla2 ,pexla2 ,grsize,dmed0  ,
     &                   p0la   ,nrdzdl ,trform ,sedexp ,nunlay)

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gschar.F,v $
c Revision 1.3  1996/06/07  11:55:39  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:11:54  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment calculate CHARacteristic grain sizes
c
      include '..\include\sobcon.i'
c
c     Declaration of parameters
c
      integer    nfrac  ,nlayer ,nbran ,ngrid  ,ibr   ,nunlay    
      integer    branch (4,nbran)              ,nrdzdl(ngrid)
      real       ddis   (nfrac+1)              ,dfrac(nfrac)  ,
     &           ptrla2 (ngrid  ,nfrac )       ,
     &           pexla2 (ngrid  ,nfrac )       ,
     &           p0la   (ngrid  ,nfrac ,nunlay),
     &           grsize (4      ,ngrid ,nlayer+1),
     &           dmed0  (ngrid ),
     &           sedexp (ngrid ),
     &           trform (3,nbran)
c
c     Declaration of local parameters
c
      integer    igr  ,nml  ,n1  
c
c     Constants
c
c     Store d-10 at d-35
c
      integer    d10  ,d50  ,d90  ,dmed
      parameter  (d10=1 ,d50=2,d90=3,dmed=4)
c
c     Calculate characteristic grain sizes, i.e. d50 ,d90 etc.
c     for transport layer.
c
      do 20 igr = branch(3,ibr),branch(4,ibr)
c
c        Calculate d10
c
         call gschad (ngrid ,nfrac ,igr ,.1 ,ddis ,ptrla2(1,1),
     &                grsize(d10,igr,1))
c
c        Calculate d50
c
         call gschad (ngrid ,nfrac ,igr ,.5 ,ddis ,ptrla2(1,1),
     &                grsize(d50,igr,1))
c
c        Calculate d90
c
         call gschad (ngrid ,nfrac ,igr ,.9 ,ddis ,ptrla2(1,1),
     &                grsize(d90,igr,1))
c
c        Keep old D-med
c
         dmed0 (igr) = grsize(dmed,igr,1)
c
c        Calculate D-med
c
         call gsdmed (ngrid ,nfrac ,igr ,dfrac ,ptrla2(1,1),
     &                grsize(dmed,igr,1))
c    
c        Calculate characteristic grainsizes which are not kept for
c        output but are used in the computation 
c        (for the dune height predictor)
c
         if (nint(trform(1,ibr)) .eq. ctrfaw) then
            call gschad (ngrid ,nfrac ,igr ,.35 ,ddis ,ptrla2(1,1),
c                        D35 (temporarily)
     &                   sedexp(igr))
c                        after call of Gsdhgi the Sediment exponent
         endif
 
  20  continue
      if (nlayer .gt. 1) then

         do 40 igr = branch(3,ibr),branch(4,ibr)
c
c           Calculate d10
c
            call gschad (ngrid ,nfrac ,igr ,.1 ,ddis ,pexla2(1,1),
     &                   grsize(d10,igr,2))
c
c           Calculate d50
c
            call gschad (ngrid ,nfrac ,igr ,.5 ,ddis ,pexla2(1,1),
     &                   grsize(d50,igr,2))
c
c           Calculate d90
c
            call gschad (ngrid ,nfrac ,igr ,.9 ,ddis ,pexla2(1,1),
     &                   grsize(d90,igr,2))
c
c           Calculate d-med
c
             call gsdmed (ngrid ,nfrac ,igr ,dfrac ,pexla2(1,1),
     &                    grsize(dmed,igr,2))
  40     continue
      endif
c 
      if (nunlay .gt. 1) then
         nml = nlayer+1
         do 60 igr = branch(3,ibr),branch(4,ibr)
            n1 = nrdzdl(igr)
c
c           Calculate d10
c
            call gschad (ngrid ,nfrac ,igr ,.1 ,ddis ,p0la(1,1,n1),
     &                   grsize(d10,igr,nml))
c
c           Calculate d50
c
            call gschad (ngrid ,nfrac ,igr ,.5 ,ddis ,p0la(1,1,n1),
     &                   grsize(d50,igr,nml))
c
c           Calculate d90
c
            call gschad (ngrid ,nfrac ,igr ,.9 ,ddis ,p0la(1,1,n1),
     &                   grsize(d90,igr,nml))
c
c           Calculate d-med
c
            call gsdmed (ngrid ,nfrac ,igr ,dfrac ,p0la(1,1,n1),
     &                   grsize(dmed,igr,nml))
  60     continue
      endif

      end
