      subroutine restim (fd_nefis_res, grpnam ,kotim ,itim  ,ncel  ,
     &                   nameac ,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             RESTIM (RESults; writing of current TIMe)
c
c Module description: The time step number at which map or history re-
c                     sults of a module are written to the result file,
c                     will also be written.
c
c                     Initial call: The map or history group will be
c                     positioned according to the starting time step
c                     number of the simulation, i.e. the current cell
c                     number will be returned.
c
c                     Next calls: The time step number will be updated
c                     and the time step number will be written.
c
c Postcondition:      Number of cell at which results must be written.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdrs            P  -
c  1 defdrs            P  -
c  3 grpnam            P  -
c  5 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c  4 kotim             I  Control code of routine restim:
c                         0 =     Initial call; set actual cell number
c                         1 =     Next calls; update of time step and
c                                 cell number
c  7 nameac(nentri)    O  All element names of a data group.
c  6 ncel              IO Actual cell number of a group.
c  8 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c defelm  DEFinition of an ELeMent
c getiel  GET Integer ELement from nefis file
c inqmxi  INQuire for MaXimum Index of data group
c putiel  PUT Integer ELement to nefis file
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
c $Log: restim.pf,v $
c Revision 1.3  1999/03/15  15:49:16  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  06:57:17  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:17  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       kotim     ,ncel      ,neferr
      integer       fd_nefis_res, itim(2)
      character*(*) grpnam
      character*(*) nameac(*)
c
c     Declaration of local variables
c
      integer       error     ,icel
      integer       dimn(1)   ,uindex(3) ,usrord(1) ,itimf(2)
      character*16  nameti    ,quanti    ,unitti
      character*64  descti
c
c     Declaration of external functions
c
      integer       defelm    ,inqmxi    ,putiel    ,getiel
      external      defelm    ,inqmxi    ,putiel    ,getiel
c
      data          usrord    ,dimn      /1,2/
c
c     Element definition
c
      data          descti    ,nameti    ,quanti     ,unitti  /
c
     &'Time step identification' ,'report_time_step' ,'t' ,'-' /
c
      if (kotim .eq. 0) then
c
c        Create element definition if not already existing.
c
         error  = defelm (fd_nefis_res  ,nameti  ,'INTEGER'  ,4  ,
     &                    quanti  ,unitti  ,descti     ,1  ,dimn)
         if (error .ne. 0 .and. error .ne. 5007) goto 1000
         nameac(1) = nameti
c
c        Search for starting cel number in case of an existing group.
c        (Function inqmxi must return no error code in that case).
c
         error = inqmxi (fd_nefis_res ,grpnam ,icel )
         if (error.eq.0) then
            icel = min(icel,ncel) + 1
         else
            icel  = 0
            error = 0
         endif
c
c        Search for cel with a time equal or lower than the starting
c        time.
c
         ncel      = 0
         uindex(3) = 1
c
c ------ Backwards loop over time steps on file -------->
   10    continue
         if (icel.gt.1) then
            icel      = icel - 1
            uindex(1) = icel
            uindex(2) = icel
            error = getiel (fd_nefis_res ,grpnam ,nameti ,
     &                      uindex  ,usrord ,8      ,itimf  )
            if (error.ne.0) goto 1000
c           Check if current time on file is less then starting time.
            if (itimf(1).lt.itim(1) .or.
     &         (itimf(1).eq.itim(1) .and. itimf(2).lt.itim(2))) then
               ncel = icel
               icel = 1
            endif
            goto 10
         endif
c <----- End loop ------------------------------------
c
c        Remark: Looping back is faster because in most cases the file
c                will just be extended after the last cell.
c
      else if (kotim .eq. 1) then
c
c        Update cel number and write time step.
c
         ncel      = ncel + 1
         uindex(1) = ncel
         uindex(2) = ncel
         uindex(3) = 1
c
         error = putiel (fd_nefis_res, grpnam ,nameti ,
     &                   uindex  ,usrord ,itim   )
         if (error.ne.0) goto 1000
c
      endif
c
 1000 continue
      neferr = error
c
      end
