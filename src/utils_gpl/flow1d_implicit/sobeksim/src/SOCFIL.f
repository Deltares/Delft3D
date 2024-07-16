      subroutine SOCFIL ( lflow, lwqin ,newres)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOCFIL (SObek Close FILes)
c
c Module description: Close files used.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 lflow             I  Switch to enable flow module
c  2 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c  3 newres            I  true, if a new restart file will be made
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c clsdat  CLoSe DATa file
c clsdef  CLoSe DEFinition file
c gtipnt  GeT Integer PoiNTer
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: socfil.pf,v $
c Revision 1.7  1999/03/15  15:19:35  kuipe_j
c tabs removed
c
c Revision 1.6  1996/01/17  14:47:32  kuipe_j
c header update
c
c Revision 1.5  1996/01/16  15:01:51  kuipe_j
c Restart improvements
c
c Revision 1.4  1995/09/22  10:03:26  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:41  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:33  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:11:59  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:30  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      logical       lflow ,lwqin, newres
c
c     Variables
c
      integer       errcod
      integer       fd_nefis
c
c     External functions
c
      integer, external     :: clsnef, gtipnt
c
c     Include memory pool
c
      include '..\include\mempool.i'

      if (lflow) then
c
c        Close result file
c
         fd_nefis = gtipnt ('FD_NEFIS_RES')

         errcod = clsnef(ip(fd_nefis))
c
c        Close restart file
c
         fd_nefis = gtipnt ('FD_NEFIS_RST')

         errcod = clsnef(ip(fd_nefis))
c
c        Close new restart file
c
         if (newres) then
            fd_nefis = gtipnt ('FD_NEFIS_NEW')

            errcod = clsnef(ip(fd_nefis))
         endif
c
c        Close water quality interface file
c
         if (lwqin) then
            fd_nefis = gtipnt ('FD_NEFIS_WAQ')

            errcod = clsnef(ip(fd_nefis))
         endif
      endif

      end
