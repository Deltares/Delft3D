subroutine getbrn (ibr ,branam, lbrnam)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Kuipers
!
! Module:             GETBRN (GET BRanch Name)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 ibr               I  branch number
!  2 branam            O  branch name
!  3 lbrnam            O  number of characters of name
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getbr1  GET Branch name
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: getbrn.pf,v $
! Revision 1.1  1999/06/01  13:42:26  kuipe_j
! names in messages substituted + message template
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    ibr , lbrnam
   character*(*)    branam
!
!     Declaration of local variables
!
   integer    branch ,gridnm
!
!     External functions
!
   integer    gtcpnt, gtipnt
   external   gtcpnt, gtipnt
!
!     Include memory pool
!
   include '..\include\mempool.i'

   branch =    gtipnt('BRANCH')
   gridnm =    gtcpnt('GRIDNM')

   call getbr1 (ibr ,ip(branch) ,cp(gridnm), branam, lbrnam)

end

subroutine getbr1 (ibr    ,branch ,gridnm, branam, lbrnam)
!
!     Declaration of parameters
!
   integer       ibr    ,lbrnam
   integer       branch (4,*)
   character*40  gridnm(*)
   character*(*) branam
!
!     Declaration of local variables
!
   integer    igr  ,i2 ,i

   igr = branch(3,ibr)
   i2 = 40
   do i=40,1,-1
      i2 = i2 -1
      if (gridnm (igr)(i:i).eq.'_') exit
   enddo
   if (i2 .le. 0) then
      write (branam(1:5),'(i5)') ibr
      lbrnam = 5
   else
      branam (:i2) = gridnm(igr)(:i2)
      lbrnam       = i2
   endif
   branam (lbrnam+1:) = ' '

end
