subroutine gstout (ngrid,nfrac,jugraut,time,headtx,array)
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gstout.F,v $
! Revision 1.3  1996/06/07  11:56:41  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:13:00  kuipe_j
! Maintenance
!
!
!***********************************************************************
   integer            ngrid,nfrac,jugraut
   double precision   time
   real               array(ngrid,nfrac)
   character*(*)      headtx
   integer            i,j

   write (jugraut,100) time
   write (jugraut,101) headtx
   do 10 i=1,ngrid
      write (jugraut,102) i,(array(i,j),j=1,nfrac)
10 continue
100 format (' TIME=',f12.0,' sec')
101 format (' ARRAY',a)
102 format (i4,10e15.8)
end
