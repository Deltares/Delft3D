subroutine gsdula (ngrid  ,nbran  ,g      ,gsopts ,branch ,&
&sedpar ,cp     ,afwfqs ,trform ,duncon ,&
&grsize ,sedexp ,duneh  ,dunel  ,deff   )
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdula.F,v $
! Revision 1.4  1996/06/07  11:56:12  kuipe_j
! multi  +  fixed layer
!
! Revision 1.3  1996/01/05  15:43:28  kuipe_j
! Lateral sediment and structures
!
! Revision 1.2  1995/09/27  10:12:19  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment calculate DUne height and LAyer thickness

!     Declaration of parameters
!
   integer    ngrid ,nbran
   integer    branch(4,nbran)         ,gsopts(*)
   real       g
   real       grsize(4,ngrid)         ,cp    (ngrid,4)       ,&
   &afwfqs(ngrid,8)         ,deff  (ngrid,2)       ,&
   &dunel (ngrid)           ,duneh (ngrid)         ,&
   &trform(3,nbran)         ,sedexp(ngrid)         ,&
   &sedpar(*)               ,duncon(*)
!
!     Declaration of local parameters
!
   integer    ibr    ,igr    ,lathic ,heiopt ,lenopt
   real       redfac ,relden ,kinvis
   real       duncof (3)
   logical    initra
!
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
!
   initra    = .false.
!
   do 20 ibr=1,nbran

!        Calculate dune height and length

      call gsdune (ngrid  ,nbran  ,ibr    ,initra ,heiopt ,lenopt ,&
!                                                            <cs>
      &g      ,relden ,kinvis ,branch ,grsize ,cp(1,2),&
!                    <qs>            <afs>           <wfs>
      &afwfqs(1,7)    ,afwfqs(1,1)    ,afwfqs(1,3)    ,&
      &duncof ,trform ,duncon ,sedexp ,duneh  ,dunel  )

!        Calculate layer thickness

      do 10 igr = branch(3,ibr),branch(4,ibr)
!                                          <deff2(igr)>
         call gslati(lathic ,duneh(igr) ,deff(igr,2) ,redfac ,&
         &grsize(dmed,igr)   )
10    continue
!
20 continue

end
