subroutine getstr (istr ,strnam, lstnam)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Kuipers
!
! Module:             GETSTR (GET STRucture name)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 istr              I  structure number
!  2 strnam            O  structure name
!  3 lstnam            O  number of characters of name
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getst1  GET STructure name
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: getstr.pf,v $
! Revision 1.1  1999/06/01  13:42:27  kuipe_j
! names in messages substituted + message template
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    istr , lstnam
   character*(*)     strnam
!
!     Declaration of local variables
!
   integer    strunm
!
!     External functions
!
   integer    gtcpnt
   external   gtcpnt
!
!     Include memory pool
!
   include '..\include\mempool.i'

   strunm =    gtcpnt('STRUNM')

   call getst1 (istr ,cp(strunm), strnam, lstnam)

end

subroutine getst1 (istr ,strunm, strnam, lstnam)
!
!     Declaration of parameters
!
   integer       istr   ,lstnam
   character*40  strunm(*)
   character*(*) strnam
!
!     Declaration of local variables
!
   integer    l  ,i2,  i

   l  = len (strunm(istr))
   i2 = l
   do i=l,1,-1
      if (strunm(istr)(i:i).ne.' ') exit
      i2 = i2-1
   enddo

   if (i2 .le. 0) then
      write (strnam(1:2),'(i2)') istr
      lstnam = 2
   else
      strnam (:i2) = strunm(istr)(:i2)
      lstnam       = i2
   endif
   strnam (lstnam+1:) = ' '

end
