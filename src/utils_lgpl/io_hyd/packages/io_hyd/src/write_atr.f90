!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  

      subroutine write_atr(hyd)

      ! function : write the attributes file

      ! global declarations

      use m_extract_waq_attribute
      use m_hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: lunatr                ! unit number sources file
      integer                                :: iseg                  ! loop counter
      integer                                :: i_atr                 ! loop counter
!
!           Local variables
!
      integer :: il, is, ikmrk1, ikmrk2
      character( 2 ), allocatable :: kenout(:)          !!  this is now allocated on the stack !!!

      call hyd%file_atr%open()
      lunatr = hyd%file_atr%unit

      if ( hyd%atr_type .EQ. ATR_COMPLETE ) then
         write(lunatr,'(a)') '         ; DELWAQ_COMPLETE_ATTRIBUTES'
         write(lunatr,'(a)') '    1    ; one block with input'
         write(lunatr,'(i6,a)') hyd%no_atr, ' ; number of attributes, they are :'
         write(lunatr,'(i6)') (i_atr,i_atr=1,hyd%no_atr)
         write(lunatr,'(a)') '    1    ; file option in this file'
         write(lunatr,'(a)') '    1    ; option without defaults'
         do iseg = 1 , hyd%noseg
            write(lunatr,'(i10)') hyd%attributes(iseg)
         enddo
         write(lunatr,'(a)') '    0    ; no time dependent attributes'
      else if ( hyd%atr_type .EQ. ATR_FM ) then
         allocate(kenout(hyd%nosegl))
         write ( lunatr , '(a)' )  '         ; DELWAQ_COMPLETE_ATTRIBUTES'
         write ( lunatr , '(a)' )  '    2    ; two blocks with input     '
         write ( lunatr , '(a)' )  '    1    ; number of attributes, they are :'
         write ( lunatr , '(a)' )  '    1    ;  ''1'' is active ''0'' is not'
         write ( lunatr , '(a)' )  '    1    ; data follows in this file '
         write ( lunatr , '(a)' )  '    1    ; all data is given without defaults'
         do il = 1,hyd%nolay
             write ( lunatr , * ) '  ;    layer: ',il
             do is = 1, hyd%nosegl
                 kenout(is) = '  '
                 call extract_waq_attribute( 1, hyd%attributes(is + (il - 1) * hyd%nosegl), ikmrk1 )
                 if (ikmrk1 == 0) then
                    kenout(is) = ' 0'
                 else if (ikmrk1 == 1) then
                    kenout(is) = ' 1'
                 else
                    write (*,*) 'Invalid attribute for segment: ', is
                    write (*,*) 'ikmrk1 is: ', ikmrk1
                    stop (1)
                 endif
             enddo
             write ( lunatr, '(500a2)' ) kenout
         enddo
         write ( lunatr , '(a)' )  '    1    ; number of attributes, they are :'
         write ( lunatr , '(a)' )  '    2    ;  ''1'' has surface ''3'' has bottom'
         write ( lunatr , '(a)' )  '         ;  ''0'' has both    ''2'' has none  '
         write ( lunatr , '(a)' )  '    1    ; data follows in this file '
         write ( lunatr , '(a)' )  '    1    ; all data is given without defaults'
         do il = 1,hyd%nolay
             write ( lunatr , * ) '  ;    layer: ',il
             do is = 1, hyd%nosegl
                 kenout(is) = '  '
                 call extract_waq_attribute( 2, hyd%attributes(is + (il - 1) * hyd%nosegl), ikmrk2 )
                 if (ikmrk2 == 0) then
                    kenout(is) = ' 0'
                 else if (ikmrk2 == 1) then
                    kenout(is) = ' 1'
                 else if (ikmrk2 == 2) then
                    kenout(is) = ' 2'
                 else if (ikmrk2 == 3) then
                    kenout(is) = ' 3'
                 else
                    write (*,*) 'Invalid attribute for segment: ', is
                    write (*,*) 'ikmrk2 is: ', ikmrk2
                    stop (1)
                 endif
             enddo
             write ( lunatr, '(500a2)' ) kenout
         enddo
         write ( lunatr , '(a)' )  '    0    ; no time dependent attributes'

!         call extract_waq_attribute( 2, iknmrk(i1), ikmrk2 )
      else
         write(lunatr,'(a)') '    1    ; Input option without defaults'
         if ( hyd%nolay .gt. 1 ) then
            write(lunatr,'(i12,a)') hyd%nosegl,'*1   ; top layer segments with water surface'
            if ( hyd%nolay .gt. 2 ) then
               write(lunatr,'(i12,a)') hyd%nosegl*(hyd%nolay-2),'*2   ; intermediate segments without surface or bottom'
            endif
            write(lunatr,'(i12,a)') hyd%nosegl,'*3   ; segments with bottom attached'
         else
            write(lunatr,'(i12,a)') hyd%noseg,'*0   ; all depth integrated segments'
         endif
      endif
      close(lunatr)
      hyd%file_atr%status = FILE_STAT_UNOPENED

      return
      end
