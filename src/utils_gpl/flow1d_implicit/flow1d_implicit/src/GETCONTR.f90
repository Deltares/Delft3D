subroutine getcontr (icon ,cntrnm, lcntrnm)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Kuipers
!
! Module:             GETCONTR (GET CONTRoller name)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 icon              I  controller number
!  2 cntrnm            O  controller name
!  3 lcntrnm           O  number of characters of name
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME       DESCRIPTION
! getcontr1  GET CONTRoller name
!=======================================================================
!

!
!     Declaration of parameters
!
   integer    icon , lcntrnm
   character(len=*)     cntrnm
!
!     Declaration of local variables
!
   integer    contrnam
!
!     External functions
!
   integer    gtcpnt
   external   gtcpnt
!
!     Include memory pool
!
   include '../include/mempool.i'

   contrnam =    gtcpnt('CONTRNAM')

   call getcontr1 (icon, cp(contrnam), cntrnm, lcntrnm)

end

subroutine getcontr1 (icon ,contrnam, cntrnm, lcntrnm)
!
!     Declaration of parameters
!
   integer       icon   ,lcntrnm
   character(len=40) contrnam(*)
   character(len=*) cntrnm
!
!     Declaration of local variables
!
   integer    l  ,i2,  i

   l  = len (contrnam(icon))
   i2 = l
   do i=l,1,-1
      if (contrnam(icon)(i:i).ne.' ') exit
      i2 = i2-1
   enddo

   if (i2 .le. 0) then
      write (cntrnm(1:2),'(i2)') icon
      lcntrnm = 2
   else
      cntrnm (:i2)  = contrnam(icon)(:i2)
      lcntrnm       = i2
   endif
   cntrnm (lcntrnm+1:) = ' '

end
