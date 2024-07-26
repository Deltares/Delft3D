      subroutine gsdune (ngrid  ,nbran  ,ibr    ,initra ,heiopt ,lenopt,
     &                   g      ,relden ,kinvis ,branch ,grsize ,cs    ,
     &                   qs     ,afs    ,wfs    ,duncof ,trform ,duncon,
     &                   sedexp ,duneh  ,dunel  )

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdune.F,v $
c Revision 1.3  1996/06/07  11:56:15  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:21  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment Calculation of DUNE height and length

c
c     Declaration of parameters
c
      integer    ngrid  ,nbran  ,heiopt ,lenopt ,ibr
      integer    branch (4,nbran)
      real       g      ,relden ,kinvis ,sedexp(*)
      real       trform(3,nbran)        ,duncof(*)        ,duncon(*)   ,
     &           grsize(4,ngrid,*)      ,cs    (ngrid,3)  ,
     &           qs    (ngrid,2)        ,afs   (ngrid,2)  ,
     &           wfs   (ngrid,2)        ,duneh (ngrid)    ,
     &           dunel (ngrid)
      logical    initra
c
c     Declaration of variables
c
      integer    igr
      real       u     ,depth  ,frou2
c
c     Declaration of constants
c
      integer    dmed
      parameter (dmed=4)
c                Dune height option
c                Gill      Van Rijn
      integer    dhgill   ,dhvryn
      parameter (dhgill=1 ,dhvryn=2)
      do 10 igr = branch(3,ibr),branch(4,ibr)
         u     = qs(igr,1) / afs(igr,1)
         depth = afs(igr,1) / wfs(igr,1)
         frou2 = u**2 / (g*depth)
         if (heiopt .eq. dhgill) then
            call gsdhgi(initra   ,g   ,relden,kinvis,grsize(dmed,igr,1),
     &                  cs(igr,1),u   ,depth ,frou2 ,duncof(1)   ,
     &                  duncof(2)     ,trform(1,ibr),duncon(1)   ,
     &                  sedexp(igr)  ,duneh(igr))
         else if (heiopt .eq. dhvryn) then
c           Van Rijn
         endif

         call gsdunl (lenopt ,depth ,duncof ,dunel(igr))
   10 continue

      end
