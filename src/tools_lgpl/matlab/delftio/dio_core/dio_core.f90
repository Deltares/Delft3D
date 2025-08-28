!------- LGPL --------------------------------------------------------------------
!
!     Copyright (C) 2011-2025 Stichting Deltares.
!
!     This library is free software; you can redistribute it and/or
!     modify it under the terms of the GNU Lesser General Public
!     License as published by the Free Software Foundation version 2.1.
!
!     This library is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!     Lesser General Public License for more details.
!
!     You should have received a copy of the GNU Lesser General Public
!     License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!     contact: delft3d.support@deltares.nl
!     Stichting Deltares
!     P.O. Box 177
!     2600 MH Delft, The Netherlands
!
!     All indications and logos of, and references to, "Delft3D" and "Deltares"
!     are registered trademarks of Stichting Deltares, and remain the property of
!     Stichting Deltares. All rights reserved.
!
!---------------------------------------------------------------------------------
!     http://www.deltaressystems.com
!
!

!********************************************************************
!*      Check and get string
!********************************************************************/
subroutine chkGetString(prhs, name, result)
!
!---- Global variables
!
   integer prhs
   character*(*) name, result
!
!---- Local variables
!
   integer ndims, strlen
   integer dimarray(2)
   character*128 message
!
!---- Executable statements
!
   ndims = mxGetNumberOfDimensions(prhs)
   strlen = -1
   if (mxIsChar(prhs) .and. (ndims == 2)) then
      call mxCopyPtrToInteger4(mxGetDimensions(prhs), dimarray, 2)
      if (dimarray(1)==1) strlen = dimarray(2)
   endif
   if (strlen < 0) call mexErrMsgTxt('Invalid '//name//'.')
   if (strlen > len(result)) call mexErrMsgTxt(name//' too long.')
   status = mxGetString(prhs, result, len(result))
   if (status /= 0)&
   &call mexErrMsgTxt('mxGetString error for '//name//'.')
end subroutine chkGetString


!********************************************************************
!*      Check and get positive integer
!********************************************************************/
subroutine chkGetPosInt(prhs,Value)
!
!---- Global variables
!
   integer prhs
   integer Value
!
!---- Local variables
!
   integer ndims, strlen
   integer rValue_pr
   integer dimarray(2)
   real*8 rValue
!
!---- Executable statements
!
   Value = -1
   ndims = mxGetNumberOfDimensions(prhs)
   if (mxIsDouble(prhs) .and. .not.mxIsComplex(prhs)&
   &.and. (ndims == 2)) then
      call mxCopyPtrToInteger4(mxGetDimensions(prhs), dimarray, 2)
      if ((dimarray(1)==1) .and. dimarray(2)==1) then
         strlen = dimarray(2)
         rValue_pr = mxGetPr(prhs)
         call mxCopyPtrToReal8(rValue_pr, rValue, 1)
         Value = int(rValue)
      endif
   endif
end subroutine chkGetPosInt


!********************************************************************
!*      Check and get DioShmPart
!********************************************************************/
subroutine GetDioPart(prhs, diopart)
   use dio_shm
!
!---- Global variables
!
   integer prhs
   integer diopart
!
!---- Local variables
!
   character*10 diopartstr
!
!---- Executable statements
!
   call chkGetString(prhs, 'DelftIO Shared Memory Part Identifier',&
   &diopartstr)
   if (diopartstr == 'header') then
      diopart = DioShmHeaderPart
   elseif (diopartstr == 'data') then
      diopart = DioShmDataPart
   else
      call mexErrMsgTxt('Invalid DelftIO Shared Memory Part '//&
      &'Indentification String')
   endif
end subroutine GetDioPart


!********************************************************************
!*      Get data share pointer
!********************************************************************/
subroutine GetDs(prhs,ds)
   use dio_shm
!
!---- Global variables
!
   integer prhs
   type(DsShmHandle) :: ds
!
!---- Local variables
!
   integer prhs_r
!
!---- Executable statements
!
   prhs_r = mxGetPr(prhs)
   call mxCopyPtrToInteger4(prhs_r, ds%cppPntr, 1)
end subroutine GetDs


!********************************************************************
!*      Put data share pointer
!********************************************************************/
subroutine PutDs(ds,plhs)
   use dio_shm
!
!---- Global variables
!
   type(DsShmHandle) :: ds
   integer plhs
!
!---- Local variables
!
   integer plhs_r
!
!---- Executable statements
!
   plhs = mxCreateNumericMatrix(1, 1,&
   &mxClassIDFromClassName('int32'), 0)
   plhs_r = mxGetPr(plhs)
   call mxCopyInteger4ToPtr(ds%cppPntr, plhs_r, 1)
end subroutine PutDs


!********************************************************************
!*      Display input
!********************************************************************/
subroutine DisplayInput(nlhs, plhs, nrhs, prhs)
!
!---- Global variables
!
   integer plhs(*), prhs(*)
   integer nlhs, nrhs
!
!---- Local variables
!
   integer i, d
   integer dimarray(100), ndims
   character*128 message
!
!---- Functions
!
   external mxGetNumberOfDimensions
   integer mxGetNumberOfDimensions
   external  mxGetDimensions
   integer mxGetDimensions
   external mxGetClassName
   character*128 mxGetClassName
!
!---- Executable statements
!
   write(message,'(I0)') nrhs
   call mexPrintf(trim(message))
   call mexPrintf(' input argument(s) and ')
   write(message,'(I0)') nlhs
   call mexPrintf(trim(message))
   call mexPrintf(' output argument(s).')
   call mexPrintf(achar(10))

   do i=1,nrhs
      call mexPrintf('arg')
      write(message,'(I2.2)') i
      call mexPrintf(trim(message))
      call mexPrintf(': ')

      ndims = mxGetNumberOfDimensions(prhs(i));
      call mxCopyPtrToInteger4(mxGetDimensions(prhs(i)), dimarray,&
      &ndims)
      write(message,'(I0)') dimarray(1)
      call mexPrintf(trim(message))
      do d=2,ndims
         call mexPrintf('x')
         write(message,'(I0)') dimarray(d)
         call mexPrintf(trim(message))
      end do
      call mexPrintf(' : ')
      message = mxGetClassName(prhs(i))
      call mexPrintf(message)

      call mexPrintf(achar(10))
   end do
end subroutine DisplayInput


!********************************************************************
!*      Main Program
!********************************************************************/
subroutine mexFunction(nlhs, plhs, nrhs, prhs)
   use dio_shm
!
!---- Global variables
!
   integer plhs(*), prhs(*)
   integer nlhs, nrhs
!
!---- Local variables
!
   character*10 cmdstr,dFormat
   character*100 DioName
   integer diopart, mxA_r
   integer nBytesHdr, nBytesData
   character*128 message
   type(DsShmHandle), pointer :: ds
   logical logok
   integer intok
   real*8 rintok
   integer dimarray(100), ndims
   character*1, allocatable :: Strings(:)
   integer*1, allocatable :: bData(:)
   integer, allocatable :: iData(:)
   real*4, allocatable :: rData(:)
!
!---- Executable statements
!
!      call DisplayInput(nlhs, plhs, nrhs, prhs)
   diopart = DioShmDataPart
   if (nrhs<1) then
      call mexErrMsgTxt('Missing DelftIO command.')
   elseif ( mxIsChar(prhs(1)) /= 1) then
      call mexErrMsgTxt('Missing DelftIO command.')
   endif
   call chkGetString(prhs(1), 'DelftIO command', cmdstr)

   call mexPrintf('DelftIO command: '//cmdstr//achar(10))

   if (cmdstr=='newput') then
!  /*****************************************************************
!   *   NEWPUT
!   *****************************************************************/
      if (nlhs /= 1) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif ((nrhs /= 3) .and. (nrhs /= 4)) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         nnBytesHdr = 0
         nBytesData = 0
         idx = 1
         if (nrhs == 3) then
!             /*
!              * Header size = 0
!              * Data size   = user specified
!              */

            call chkGetPosInt(prhs(2),nBytesData)
            if (nBytesData < 0)&
            &call mexErrMsgTxt('Invalid data size.')

            idx = 2
         else
!             /*
!              * Header size = user specified
!              * Data size   = user specified
!              */

            call chkGetPosInt(prhs(2),nBytesHdr)

            if (nBytesHdr < 0)&
            &call mexErrMsgTxt('Invalid header size.')

            call chkGetPosInt(prhs(3),nBytesData)
            if (nBytesData < 0)&
            &call mexErrMsgTxt('Invalid data size.')

            idx = 3
         endif
         call chkGetString(prhs(idx+1),'DelftIO share name',DioName)
         intok = DioShmDsCreateWithSize(ds,nBytesHdr,nBytesData,&
         &DioShmSharedMem,DioName)

         call PutDs(ds,plhs(1))
      endif
   elseif (cmdstr=='newget') then
!  /*****************************************************************
!   *   NEWGET
!   *****************************************************************/
      if (nlhs /= 1) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif (nrhs /= 2) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         call chkGetString(prhs(2),'DelftIO share name',DioName)
         intok = DioShmDsCreateNoSize(ds,DioShmSharedMem,DioName)

         call PutDs(ds,plhs(1))
      endif
   elseif (cmdstr=='getname') then
!  /*****************************************************************
!   *   GETNAME
!   *****************************************************************/
      if (nlhs /= 1) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif (nrhs /= 2) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         call mexErrMsgTxt('GETNAME command not available.')
      endif
   elseif (cmdstr=='startwrite') then
!  /*****************************************************************
!   *   STARTWRITE
!   *****************************************************************/
      if (nlhs /= 1) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif ((nrhs /= 2) .and. (nrhs /= 3)) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         allocate(ds)
         call GetDs(prhs(2),ds)

         if (nrhs == 3) call GetDioPart(prhs(3),diopart)

         if (diopart==DioShmHeaderPart) then
            logok = DioShmDsStartWriteHdr(ds)
         else
            logok = DioShmDsStartWrite(ds)
         endif

         plhs(1) = mxCreateDoubleMatrix(1, 1, 0)
         mxA_r = mxGetPr(plhs(1))
         rintok = logok
         call mxCopyReal8ToPtr(rintok, mxA_r, 1)
         deallocate(ds)
      endif
   elseif (cmdstr=='startread') then
!  /*****************************************************************
!   *   STARTREAD
!   *****************************************************************/
      if (nlhs /= 1) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif ((nrhs /= 2) .and. (nrhs /= 3)) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         allocate(ds)
         call GetDs(prhs(2),ds)
         if (nrhs == 3) call GetDioPart(prhs(3),diopart)
         if (diopart==DioShmHeaderPart) then
            logok = DioShmDsStartReadHdr(ds)
         else
            logok = DioShmDsStartRead(ds)
         endif
         plhs(1) = mxCreateDoubleMatrix(1, 1, 0)
         mxA_r = mxGetPr(plhs(1))
         rintok = logok
         call mxCopyReal8ToPtr(rintok, mxA_r, 1)
         deallocate(ds)
      endif
   elseif (cmdstr=='write') then
!  /*****************************************************************
!   *   WRITE
!   *****************************************************************/
      if (nlhs /= 0) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif (nrhs /= 3) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         allocate(ds)
         call GetDs(prhs(2),ds)

         ndims = mxGetNumberOfDimensions(prhs(3))
         call mxCopyPtrToInteger4(mxGetDimensions(prhs(3)), dimarray,&
         &ndims)
         numItems = 1
         do i=1,ndims
            numItems = numItems * dimarray(i)
         enddo

!          /*
!           * Write Data
!           */
         if (mxIsUint8(prhs(3))) then
            mxA_r = mxGetPr(prhs(3))
            allocate(bData(numItems))
            allocate(Strings(numItems))
            call mxCopyPtrToInteger1(mxA_r, bData, numItems)
            do i=1,numItems
               Strings(i)(1:1) = achar(bData(i))
            enddo
            call DioShmDsWriteStrings(ds,numItems,1,Strings)
            deallocate(bData)
            deallocate(Strings)
         elseif (mxIsInt32(prhs(3))) then
            mxA_r = mxGetPr(prhs(3))
            allocate(iData(numItems))
            call mxCopyPtrToInteger4(mxA_r, iData, numItems)
            call DioShmDsWriteInts(ds,numItems,iData)
            deallocate(iData)
         elseif (mxIsSingle(prhs(3))) then
            mxA_r = mxGetPr(prhs(3))
            allocate(rData(numItems))
            call mxCopyPtrToReal4(mxA_r, rData, numItems)
            call DioShmDsWriteReals(ds,numItems,rData)
            deallocate(rData)
         else
            call mexErrMsgTxt('Transfer of data other than uint8'//&
            &',int32,single not yet implemented.')
         endif
      endif
   elseif (cmdstr=='read') then
!  /*****************************************************************
!   *   READ
!   *****************************************************************/
      if (nlhs /= 2) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif (nrhs /= 4) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         allocate(ds)
         call GetDs(prhs(2),ds)

         call chkGetPosInt(prhs(3),numItems)
         if (numItems < 0)&
         &call mexErrMsgTxt('Invalid number of items.')

         call chkGetString(prhs(4),'data format argument',dFormat)

!          /*
!           * Read and Return Data
!           */
         intok = 0
         if (dFormat=='uint8') then
            allocate(bData(numItems))
            allocate(Strings(numItems))
            call DioShmDsReadStrings(ds,numItems,1,Strings)
            plhs(1) = mxCreateNumericMatrix(1, numItems,&
            &mxClassIDFromClassName('int8'), 0)
            mxA_r = mxGetPr(plhs(1))
            do i=1,numItems
               bData(i) = iachar(Strings(i)(1:1))
            enddo
            call mxCopyInteger1ToPtr(bData, mxA_r, numItems)
            deallocate(bData)
            deallocate(Strings)
         elseif ((dFormat=='int') .or. (dFormat=='int32')) then
            allocate(iData(numItems))
            call DioShmDsReadInts(ds,numItems,iData)
            plhs(1) = mxCreateNumericMatrix(1, numItems,&
            &mxClassIDFromClassName('int32'), 0)
            mxA_r = mxGetPr(plhs(1))
            call mxCopyInteger4ToPtr(iData, mxA_r, numItems)
            deallocate(iData)
         elseif ((dFormat=='single') .or. (dFormat=='float32')) then
            allocate(rData(numItems))
            call DioShmDsReadReals(ds,numItems,rData)
            plhs(1) = mxCreateNumericMatrix(1, numItems,&
            &mxClassIDFromClassName('single'), 0)
            mxA_r = mxGetPr(plhs(1))
            call mxCopyReal4ToPtr(rData, mxA_r, numItems)
            deallocate(rData)
         else
            call mexErrMsgTxt('Transfer of data other than uint8'//&
            &',int32,single not yet implemented.')
         endif
!          /*
!           * Return flag of correct reading
!           */
         plhs(2) = mxCreateDoubleMatrix(1, 1, 0)
         mxA_r = mxGetPr(plhs(2))
         rintok = intok
         call mxCopyReal8ToPtr(rintok, mxA_r, 1)
         deallocate(ds)
      endif
   elseif (cmdstr=='endwrite') then
!  /*****************************************************************
!   *   ENDWRITE
!   *****************************************************************/
      if (nlhs /= 0) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif ((nrhs /= 2) .and. (nrhs /= 3)) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         allocate(ds)
         call GetDs(prhs(2),ds)

         if (nrhs == 3) call GetDioPart(prhs(3),diopart)

         if (diopart==DioShmHeaderPart) then
            call DioShmDsEndWriteHdr(ds)
         else
            call DioShmDsEndWrite(ds)
         endif
         deallocate(ds)
      endif
   elseif (cmdstr=='endread') then
!  /*****************************************************************
!   *   ENDREAD
!   *****************************************************************/
      if (nlhs /= 0) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif ((nrhs /= 2) .and. (nrhs /= 3)) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
         allocate(ds)
         call GetDs(prhs(2),ds)

         if (nrhs == 3) call GetDioPart(prhs(3),diopart)

         if (diopart==DioShmHeaderPart) then
            call DioShmDsEndReadHdr(ds)
         else
            call DioShmDsEndRead(ds)
         endif
         deallocate(ds)
      endif
   elseif (cmdstr=='delete') then
!  /*****************************************************************
!   *   DELETE
!   *****************************************************************/
      if (nlhs /= 0) then
         call mexErrMsgTxt('Invalid number of output arguments.')
      elseif (nrhs /= 2) then
         call mexErrMsgTxt('Invalid number of input arguments.')
      else
      endif
   else
!  /*****************************************************************
!   *   Unrecognized command.
!   *****************************************************************/
      call mexErrMsgTxt('Unrecognized DelftIO command: '//cmdstr)
   endif
end subroutine mexFunction
