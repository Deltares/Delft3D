      subroutine gsdula (ngrid  ,nbran  ,g      ,gsopts ,branch ,
     &                   sedpar ,cp     ,afwfqs ,trform ,duncon ,
     &                   grsize ,sedexp ,duneh  ,dunel  ,deff   )
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdula.F,v $
c Revision 1.4  1996/06/07  11:56:12  kuipe_j
c multi  +  fixed layer
c
c Revision 1.3  1996/01/05  15:43:28  kuipe_j
c Lateral sediment and structures
c
c Revision 1.2  1995/09/27  10:12:19  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment calculate DUne height and LAyer thickness

c     Declaration of parameters
c
      integer    ngrid ,nbran   
      integer    branch(4,nbran)         ,gsopts(*)  
      real       g    
      real       grsize(4,ngrid)         ,cp    (ngrid,4)       ,
     &           afwfqs(ngrid,8)         ,deff  (ngrid,2)       ,
     &           dunel (ngrid)           ,duneh (ngrid)         ,
     &           trform(3,nbran)         ,sedexp(ngrid)         ,
     &           sedpar(*)               ,duncon(*)             
c
c     Declaration of local parameters
c
      integer    ibr    ,igr    ,lathic ,heiopt ,lenopt
      real       redfac ,relden ,kinvis 
      real       duncof (3)
      logical    initra
c
      integer    dmed
      parameter (dmed=4)

      kinvis    = sedpar(1) 
      relden    = sedpar(2) 
      duncof(1) = sedpar(6)
      duncof(2) = sedpar(7)
      duncof(3) = sedpar(8)
      redfac    = sedpar(12)
      heiopt    = gsopts(1) 
      lenopt    = gsopts(2)
      lathic    = gsopts(4) 
c
      initra    = .false.
c
      do 20 ibr=1,nbran

c        Calculate dune height and length

         call gsdune (ngrid  ,nbran  ,ibr    ,initra ,heiopt ,lenopt ,
c                                                            <cs>
     &                g      ,relden ,kinvis ,branch ,grsize ,cp(1,2),
c                    <qs>            <afs>           <wfs>
     &                afwfqs(1,7)    ,afwfqs(1,1)    ,afwfqs(1,3)    ,
     &                duncof ,trform ,duncon ,sedexp ,duneh  ,dunel  )

c        Calculate layer thickness

         do 10 igr = branch(3,ibr),branch(4,ibr)
c                                          <deff2(igr)>
            call gslati(lathic ,duneh(igr) ,deff(igr,2) ,redfac ,
     &                  grsize(dmed,igr)   )
   10    continue
c
   20 continue

      end
