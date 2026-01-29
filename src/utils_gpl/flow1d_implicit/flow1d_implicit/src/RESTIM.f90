subroutine restim (fd_nefis_res, grpnam ,kotim ,itim  ,ncel  ,&
&nameac ,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             RESTIM (RESults; writing of current TIMe)
!
! Module description: The time step number at which map or history re-
!                     sults of a module are written to the result file,
!                     will also be written.
!
!                     Initial call: The map or history group will be
!                     positioned according to the starting time step
!                     number of the simulation, i.e. the current cell
!                     number will be returned.
!
!                     Next calls: The time step number will be updated
!                     and the time step number will be written.
!
! Postcondition:      Number of cell at which results must be written.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdrs            P  -
!  1 defdrs            P  -
!  3 grpnam            P  -
!  5 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
!  4 kotim             I  Control code of routine restim:
!                         0 =     Initial call; set actual cell number
!                         1 =     Next calls; update of time step and
!                                 cell number
!  7 nameac(nentri)    O  All element names of a data group.
!  6 ncel              IO Actual cell number of a group.
!  8 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! defelm  DEFinition of an ELeMent
! getiel  GET Integer ELement from nefis file
! inqmxi  INQuire for MaXimum Index of data group
! putiel  PUT Integer ELement to nefis file
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
! $Log: restim.pf,v $
! Revision 1.3  1999/03/15  15:49:16  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  06:57:17  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:17  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       kotim     ,ncel      ,neferr
   integer       fd_nefis_res, itim(2)
   character(len=*) grpnam
   character(len=*) nameac(*)
!
!     Declaration of local variables
!
   integer       error     ,icel
   integer       dimn(1)   ,uindex(3) ,usrord(1) ,itimf(2)
   character(len=16)  nameti    ,quanti    ,unitti
   character(len=64)  descti
!
!     Declaration of external functions
!
   integer       defelm    ,inqmxi    ,putiel    ,getiel
   external      defelm    ,inqmxi    ,putiel    ,getiel
!
   data          usrord    ,dimn      /1,2/
!
!     Element definition
!
   data          descti    ,nameti    ,quanti     ,unitti  /&
!
   &'Time step identification' ,'report_time_step' ,'t' ,'-' /
!
   if (kotim .eq. 0) then
!
!        Create element definition if not already existing.
!
      error  = defelm (fd_nefis_res  ,nameti  ,'INTEGER'  ,4  ,&
      &quanti  ,unitti  ,descti     ,1  ,dimn)
      if (error .ne. 0 .and. error .ne. 5007) goto 1000
      nameac(1) = nameti
!
!        Search for starting cel number in case of an existing group.
!        (Function inqmxi must return no error code in that case).
!
      error = inqmxi (fd_nefis_res ,grpnam ,icel )
      if (error.eq.0) then
         icel = min(icel,ncel) + 1
      else
         icel  = 0
         error = 0
      endif
!
!        Search for cel with a time equal or lower than the starting
!        time.
!
      ncel      = 0
      uindex(3) = 1
!
! ------ Backwards loop over time steps on file -------->
10    continue
      if (icel.gt.1) then
         icel      = icel - 1
         uindex(1) = icel
         uindex(2) = icel
         error = getiel (fd_nefis_res ,grpnam ,nameti ,&
         &uindex  ,usrord ,8      ,itimf  )
         if (error.ne.0) goto 1000
!           Check if current time on file is less then starting time.
         if (itimf(1).lt.itim(1) .or.&
         &(itimf(1).eq.itim(1) .and. itimf(2).lt.itim(2))) then
            ncel = icel
            icel = 1
         endif
         goto 10
      endif
! <----- End loop ------------------------------------
!
!        Remark: Looping back is faster because in most cases the file
!                will just be extended after the last cell.
!
   else if (kotim .eq. 1) then
!
!        Update cel number and write time step.
!
      ncel      = ncel + 1
      uindex(1) = ncel
      uindex(2) = ncel
      uindex(3) = 1
!
      error = putiel (fd_nefis_res, grpnam ,nameti ,&
      &uindex  ,usrord ,itim   )
      if (error.ne.0) goto 1000
!
   endif
!
1000 continue
   neferr = error
!
end
