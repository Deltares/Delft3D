subroutine SOCFIL ( lflow, lwqin ,newres)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOCFIL (SObek Close FILes)
!
! Module description: Close files used.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 lflow             I  Switch to enable flow module
!  2 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
!  3 newres            I  true, if a new restart file will be made
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! clsdat  CLoSe DATa file
! clsdef  CLoSe DEFinition file
! gtipnt  GeT Integer PoiNTer
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: socfil.pf,v $
! Revision 1.7  1999/03/15  15:19:35  kuipe_j
! tabs removed
!
! Revision 1.6  1996/01/17  14:47:32  kuipe_j
! header update
!
! Revision 1.5  1996/01/16  15:01:51  kuipe_j
! Restart improvements
!
! Revision 1.4  1995/09/22  10:03:26  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:41  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:33  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:59  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:30  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   logical       lflow ,lwqin, newres
!
!     Variables
!
   integer       errcod
   integer       fd_nefis
!
!     External functions
!
   integer, external     :: clsnef, gtipnt
!
!     Include memory pool
!
   include '..\include\mempool.i'

   if (lflow) then
!
!        Close result file
!
      fd_nefis = gtipnt ('FD_NEFIS_RES')

      errcod = clsnef(ip(fd_nefis))
!
!        Close restart file
!
      fd_nefis = gtipnt ('FD_NEFIS_RST')

      errcod = clsnef(ip(fd_nefis))
!
!        Close new restart file
!
      if (newres) then
         fd_nefis = gtipnt ('FD_NEFIS_NEW')

         errcod = clsnef(ip(fd_nefis))
      endif
!
!        Close water quality interface file
!
      if (lwqin) then
         fd_nefis = gtipnt ('FD_NEFIS_WAQ')

         errcod = clsnef(ip(fd_nefis))
      endif
   endif

end
