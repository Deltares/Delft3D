      subroutine gsnoml (ibr    ,nfrac  ,nlayer ,nbran  ,ngrid ,branch ,
     &                   ptrla2 ,pexla2 ,p0la   ,grsize ,nunlay,nrdzdl )

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsnoml.F,v $
c Revision 1.3  1996/01/08  13:30:00  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:12:42  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment adapt NOn Moving Layer

c
c     Declaration of parameters
c
      integer    nfrac  ,nlayer ,nbran ,ngrid  ,ibr
      integer    nunlay
      integer    branch (4,nbran)
      integer    nrdzdl (ngrid  )
      real       ptrla2 (ngrid  ,nfrac )       ,
     &           pexla2 (ngrid  ,nfrac )       ,
     &           p0la   (ngrid  ,nfrac ,nunlay),
     &           grsize (4,ngrid,nlayer+1)
c
c     Declaration of local parameters
c
      integer    i   ,igr  ,jf  ,nml
c
      if (nlayer .eq. 1) then
         do 20 igr = branch(3,ibr),branch(4,ibr)
           do 10 jf=1,nfrac
              p0la(igr,jf,nrdzdl(igr)) = ptrla2(igr,jf)
  10        continue
  20     continue
      else
         do 40 igr = branch(3,ibr),branch(4,ibr)
           do 30 jf=1,nfrac
              p0la(igr,jf,nrdzdl(igr)) = pexla2(igr,jf)
  30        continue
  40     continue
      endif

      nml = nlayer + 1
      do 60 igr = branch(3,ibr),branch(4,ibr)
         do 50 i=1,4
            grsize(i,igr,nml) = grsize(i,igr,nlayer)
  50     continue
  60  continue

      end
