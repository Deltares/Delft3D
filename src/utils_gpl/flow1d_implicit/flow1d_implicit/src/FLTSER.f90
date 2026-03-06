subroutine fltser (kode   ,nstru  ,ngrid  ,strpar ,strtyp ,h     ,&
&ker    ,juer   )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             FLTSER (FLow Tabulated Structure ERror messages)
!
! Module description: Make warning if the domain of the data base of a
!                     structure is too small.
!
!-----------------------------------------------------------------------
! Parameters:
! NAME             IO DESCRIPTION
! kode             I  = 0 call at every completed flow time step
!                       1 call at end of simulation
! nstru               number of structures
! strpar(21,nstru) IO Parameters for data base structure:
!                     (1,i) interpolation:
!                           1 = linear
!                           2 = spline
!                     (2,i) sequence number of data base structure
!                           (1,2,…), i.e. data base structure number
!                     (3,i) number of dimensions of data base (2 or 3)
!                     (4,i) number of H1-values nh1
!                     (5,i) number of H2-values nh2
!                     (6,i) number of values of gate height, crest level
!                           or width
!                           (extension in future; 1 in Sobek 2.51)
!                     (7,i) dummy
!                     (8,i) dummy
!                     (9,i) index of first element in array with
!                           discharges stdbq for this structure
!                     (10,i) type of values of first row in stdbq for
!                           this structure:
!                           1 : H2-values
!                           2 : dH-values (dH=H2-H1)
!                     (11,i) number of structure where the data base
!                           of this structure is defined. This is
!                           the structure with the first occurrence
!                           of a link to this data base table.
!                     (12,i) indicator that identifies if this data
!                           base table is initialised:
!                           0 : no
!                           1 : yes
!                     (13,i) level of sill of structure or reference
!                           level
!                     (14,i) indicator that defines if discharge is zero
!                           if sill is dry (0), otherwise 1.
!                     (15,i) minimum H1 (wrt sill) in case of underflow
!                           (otherwise 1E20)
!                     (16,i) minimum H2/Dh in case of underflow
!                           (otherwise 1E20)
!                     (17,i) maximum H1 (wrt sill) in case of overflow
!                           (otherwise -1E20)
!                     (18,i) maximum H2/DH in case of overflow
!                           (otherwise -1E20)
!                     (19,i) 4-digit indicater for under or overflow in
!                           an iteration step.
!                           From right to left a digit corresponds for:
!                           H1-underflow, H2/DH-underflow,
!                           H1-overflow, H2/DH-overflow.
!                           A digit can be 0 (inside) or 1 (outside
!                           data base).
!
!-----------------------------------------------------------------------
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer    kode  ,nstru   ,ngrid ,juer   ,ker
   integer    strtyp(10,nstru)
   real       strpar(dmstrpar,nstru)
   double precision h(ngrid)
!
!     Declaration of local variables:
!
   integer    il    ,ir     ,limit   ,lstnam, istru
   real       h1    ,h2     ,zs
   logical    deltah
   character  strnam*40
   character  h2txt*3
   character  txt*13
!
!     Include sobek constants and error codes
!
   include '../include/errcod.i'
   include '../include/sobcon.i'

   do istru=1,nstru
      if ( strtyp(1,istru) .eq. cdtbst ) then
         if (kode.eq.0) then
!              Time step completed
!              Update arguments that exceed data base
            il      = strtyp(3,istru)
            ir      = strtyp(4,istru)
            deltah  = nint(strpar(10,istru)).eq.2
            zs      =      strpar(13,istru)
            limit   = nint(strpar(19,istru))
!
            if (limit.gt.0) then
               h1 = real( h(il), kind=kind(h1) ) - zs
               if (deltah) then
                  h2 = real( h(il) - h(ir), kind=kind(h2) )
               else
                  h2 = real( h(ir), kind=kind(h2) ) - zs
               endif
               if (mod(limit,10).eq.1)&
               &strpar(15,istru) = min(strpar(15,istru),h1)
               if (mod(limit/10,10).eq.1)&
               &strpar(16,istru) = min(strpar(16,istru),h2)
               if (mod(limit/100,10).eq.1)&
               &strpar(17,istru) = max(strpar(17,istru),h1)
               if (mod(limit/1000,10).eq.1)&
               &strpar(18,istru) = max(strpar(18,istru),h2)
            endif
         else
!              Simulation completed
!              Make messages if any
            call getstr(istru,strnam,lstnam)
            if (deltah) then
               h2txt = 'DH='
            else
               h2txt = 'H2='
            endif
            if (strpar(15,istru) .lt. 1.e19) then
               call getstr(istru,strnam,lstnam)
               ker = warnng
               write (txt,'(a,f7.2,a)') ' (H1=',strpar(15,istru),')'
               call sre_error (juer,&
               &'FLTSER @Underflow@ in db structure @'&
               &//strnam(:lstnam)//txt//'@', efllim, ker)
            endif
            if (strpar(16,istru) .lt. 1.e19) then
               call getstr(istru,strnam,lstnam)
               ker = warnng
               write (txt,'(2a,f7.2,a)')&
               &' (',h2txt,strpar(16,istru),')'
               call sre_error (juer,&
               &'FLTSER @Underflow@ in db structure @'&
               &//strnam(:lstnam)//txt//'@', efllim, ker)
            endif
            if (strpar(17,istru) .gt. -1.e19) then
               call getstr(istru,strnam,lstnam)
               ker = warnng
               write (txt,'(a,f7.2,a)') ' (H1=',strpar(17,istru),')'
               call sre_error (juer,&
               &'FLTSER @Overflow@ in db structure @'&
               &//strnam(:lstnam)//txt//'@', efllim, ker)
            endif
            if (strpar(18,istru) .gt. -1.e19) then
               call getstr(istru,strnam,lstnam)
               ker = warnng
               write (txt,'(2a,f7.2,a)')&
               &' (',h2txt,strpar(18,istru),')'
               call sre_error (juer,&
               &'FLTSER @Overflow@ in db structure @'&
               &//strnam(:lstnam)//txt//'@', efllim, ker)
            endif
         endif
      endif
   enddo
!
end
