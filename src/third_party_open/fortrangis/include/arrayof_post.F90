#ifndef ARRAYOF_TYPE
#define ARRAYOF_TYPE arrayof_/**/ARRAYOF_ORIGTYPE
#endif


!> Method for inserting a number of elements of the array at a desired position.
!! If necessary, the array is reallocated to accomodate the new elements.
SUBROUTINE ARRAYOF_TYPE/**/_insert_array(this, content, nelem, pos)
TYPE(ARRAYOF_TYPE) :: this !< array object to extend
ARRAYOF_ORIGTYPE, INTENT(in), OPTIONAL :: content(:) !< object of \a TYPE ARRAYOF_ORIGTYPE to insert, if not provided, space is reserved but not initialized
INTEGER, INTENT(in), OPTIONAL :: nelem !< number of elements to add, mutually exclusive with the previous parameter, if both are not provided, a single element is added without initialization
INTEGER, INTENT(in), OPTIONAL :: pos !< position where to insert, if it is out of range, it is clipped, if it is not provided, the object is appended

INTEGER :: i, n, p

IF (PRESENT(content)) THEN ! size of data
  n = SIZE(content)
ELSE IF (PRESENT(nelem)) THEN ! explicit size
  n = nelem
ELSE ! default add one element
  n = 1
ENDIF
IF (n <= 0) RETURN ! nothing to do

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize+1))
ELSE ! pos not provided, append
  p = this%arraysize + 1
ENDIF
this%arraysize = this%arraysize + n
#ifdef DEBUG
!PRINT*,'ARRAYOF: inserting ',n,' elements at position ',p
#endif

CALL ARRAYOF_TYPE/**/_alloc(this) ! ensure to have space
DO i = this%arraysize, p+n, -1 ! push the elements forward starting from p
  this%array(i) = this%array(i-n)
ENDDO
IF (PRESENT(content)) THEN
  this%array(p:p+n-1) = content(:)
ENDIF

END SUBROUTINE ARRAYOF_TYPE/**/_insert_array


!> Method for inserting an element of the array at a desired position.
!! If necessary, the array is reallocated to accomodate the new element.
SUBROUTINE ARRAYOF_TYPE/**/_insert(this, content, pos)
TYPE(ARRAYOF_TYPE) :: this !< array object to extend
ARRAYOF_ORIGTYPE, INTENT(in) :: content !< object of \a TYPE ARRAYOF_ORIGTYPE to insert
INTEGER, INTENT(in), OPTIONAL :: pos !< position where to insert, if it is out of range, it is clipped, if it is not provided, the object is appended

CALL insert(this, (/content/), pos=pos)

END SUBROUTINE ARRAYOF_TYPE/**/_insert


!> Quick method to append an element to the array.
!! The return value is the position at which the element has been
!! appended.
FUNCTION ARRAYOF_TYPE/**/_append(this, content) RESULT(pos)
TYPE(ARRAYOF_TYPE) :: this !< array object to extend
ARRAYOF_ORIGTYPE, INTENT(in) :: content !< object of \a TYPE ARRAYOF_ORIGTYPE to append
INTEGER :: pos

this%arraysize = this%arraysize + 1
pos = this%arraysize + 1
CALL ARRAYOF_TYPE/**/_alloc(this)
this%array(this%arraysize) = content

END FUNCTION ARRAYOF_TYPE/**/_append


#ifdef ARRAYOF_ORIGEQ
!> Method for inserting an element of the array at a desired position
!! only if it is not present in the array yet.
!! If necessary, the array is reallocated to accomodate the new element.
SUBROUTINE ARRAYOF_TYPE/**/_insert_unique(this, content, pos)
TYPE(ARRAYOF_TYPE) :: this !< array object to extend
ARRAYOF_ORIGTYPE, INTENT(in) :: content !< object of \a TYPE ARRAYOF_ORIGTYPE to insert
INTEGER, INTENT(in), OPTIONAL :: pos !< position where to insert, if it is out of range, it is clipped, if it is not provided, the object is appended

INTEGER :: i

DO i = 1, this%arraysize
  IF (this%array(i) == content) RETURN
ENDDO

CALL insert(this, (/content/), pos=pos)

END SUBROUTINE ARRAYOF_TYPE/**/_insert_unique


!> Quick function to append an element to the array
!! only if it is not present in the array yet.  The return value is
!! the position at which the element has been appended or at which it
!! has been found.
FUNCTION ARRAYOF_TYPE/**/_append_unique(this, content) RESULT(pos)
TYPE(ARRAYOF_TYPE) :: this !< array object to extend
ARRAYOF_ORIGTYPE, INTENT(in) :: content !< object of \a TYPE ARRAYOF_ORIGTYPE to append
INTEGER :: pos

DO pos = 1, this%arraysize
  IF (this%array(pos) == content) RETURN
ENDDO

this%arraysize = this%arraysize + 1
pos = this%arraysize
CALL ARRAYOF_TYPE/**/_alloc(this)
this%array(this%arraysize) = content

END FUNCTION ARRAYOF_TYPE/**/_append_unique
#endif


!> Method for removing elements of the array at a desired position.
!! If necessary, the array is reallocated to reduce space.
SUBROUTINE ARRAYOF_TYPE/**/_remove(this, nelem, pos &
#ifdef ARRAYOF_ORIGDESTRUCTOR
 , nodestroy &
#endif
)
TYPE(ARRAYOF_TYPE) :: this !< array object in which an element has to be removed
INTEGER, INTENT(in), OPTIONAL :: nelem !< number of elements to remove, if not provided, a single element is removed
INTEGER, INTENT(in), OPTIONAL :: pos !< position of the element to be removed, if it is out of range, it is clipped, if it is not provided, objects are removed at the end
#ifdef ARRAYOF_ORIGDESTRUCTOR
LOGICAL, INTENT(in), OPTIONAL :: nodestroy !< if provided and \c .TRUE. , the destructor possibily defined for the ARRAYOF_ORIGTYPE is not called for every deleted object, may be useful if the objects to be deleted have been copied to another instance of ARRAYOF_TYPE and continue their life there
#endif

INTEGER :: i, n, p
#ifdef ARRAYOF_ORIGDESTRUCTOR
LOGICAL :: destroy
#endif

IF (this%arraysize <= 0) RETURN ! nothing to do
IF (PRESENT(nelem)) THEN ! explicit size
  n = nelem
  IF (n <= 0) RETURN ! nothing to do
ELSE ! default remove one element
  n = 1
ENDIF

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize-n+1))
ELSE ! pos not provided, cut at the end
  p = this%arraysize - n + 1
ENDIF
#ifdef DEBUG
!PRINT*,'ARRAYOF: removing ',n,' elements at position ',p
#endif

! destroy the elements if needed
#ifdef ARRAYOF_ORIGDESTRUCTOR
destroy = .TRUE.
IF (PRESENT(nodestroy)) THEN
  destroy = .NOT.nodestroy
ENDIF
IF (destroy) THEN
  DO i = p, p+n-1
    ARRAYOF_ORIGDESTRUCTOR(this%array(i))
  ENDDO
ENDIF
#endif

this%arraysize = this%arraysize - n
DO i = p, this%arraysize ! push the elements backward starting from p
  this%array(i) = this%array(i+n)
ENDDO
CALL ARRAYOF_TYPE/**/_alloc(this) ! release space if possible

END SUBROUTINE ARRAYOF_TYPE/**/_remove


!> Destructor for finalizing an array object.  If defined, calls the
!! destructor for every element of the array object;
!! finally it deallocates all the space occupied.
SUBROUTINE ARRAYOF_TYPE/**/_delete(this, &
#ifdef ARRAYOF_ORIGDESTRUCTOR
 nodestroy, &
#endif
 nodealloc)
TYPE(ARRAYOF_TYPE) :: this !< array object to be destroyed
#ifdef ARRAYOF_ORIGDESTRUCTOR
LOGICAL, INTENT(in), OPTIONAL :: nodestroy !< if provided and \c .TRUE. , the destructor possibily defined for the ARRAYOF_ORIGTYPE is not called for every deleted object, may be useful if the objects to be deleted have been copied to another instance of ARRAYOF_TYPE and continue their life there
#endif
LOGICAL, INTENT(in), OPTIONAL :: nodealloc !< if provided and \c .TRUE. , the space reserved for the array is not deallocated, thus the values are retained, while the array pointer is nullified, this means that the caller must have previously assigned the pointer contents this%array to another pointer to prevent memory leaks

TYPE(ARRAYOF_TYPE) :: empty

#ifdef ARRAYOF_ORIGDESTRUCTOR
INTEGER :: i
LOGICAL :: destroy
#endif
LOGICAL :: dealloc

#ifdef DEBUG
!PRINT*,'ARRAYOF: destroying ',this%arraysize
#endif
IF (ASSOCIATED(this%array)) THEN
! destroy the elements if needed
#ifdef ARRAYOF_ORIGDESTRUCTOR
  destroy = .TRUE.
  IF (PRESENT(nodestroy)) THEN
    destroy = .NOT.nodestroy
  ENDIF
  IF (destroy) THEN
    DO i = 1, this%arraysize
      ARRAYOF_ORIGDESTRUCTOR(this%array(i))
    ENDDO
  ENDIF
#endif
! free the space
  dealloc = .TRUE.
  IF (PRESENT(nodealloc)) THEN
    dealloc = .NOT.nodealloc
  ENDIF
  IF (dealloc) THEN
    DEALLOCATE(this%array)
  ENDIF
ENDIF
! give empty values
this=empty

END SUBROUTINE ARRAYOF_TYPE/**/_delete


!> Method for packing the array object reducing at a minimum
!! the memory occupation, without destroying its contents.
!! The value of this::overalloc remains unchanged.
!! After the call to the method, the object can continue to be used,
!! extended and shortened as before. If the object is empty the array
!! is allocated to zero length.
SUBROUTINE ARRAYOF_TYPE/**/_packarray(this)
TYPE(ARRAYOF_TYPE) :: this !< object to be packed

DOUBLE PRECISION :: tmpoveralloc

#ifdef DEBUG
!PRINT*,'ARRAYOF: packing ',this%arraysize
#endif
tmpoveralloc = this%overalloc ! save value
this%overalloc = 1.0D0
CALL ARRAYOF_TYPE/**/_alloc(this) ! reallocate exact size
this%overalloc = tmpoveralloc

END SUBROUTINE ARRAYOF_TYPE/**/_packarray


SUBROUTINE ARRAYOF_TYPE/**/_alloc(this)
TYPE(ARRAYOF_TYPE) :: this

ARRAYOF_ORIGTYPE, POINTER :: tmpptr(:)
INTEGER :: newsize, copysize

newsize = MAX(INT(this%arraysize*this%overalloc), this%arraysize)

IF (ASSOCIATED(this%array)) THEN ! array already allocated
! space is neither too small nor too big, nothing to do
  IF (SIZE(this%array) >= this%arraysize .AND. SIZE(this%array) <= newsize) RETURN
! if too big, reduce
  IF (SIZE(this%array) > newsize) newsize = this%arraysize
#ifdef DEBUG
!  PRINT*,'ARRAYOF: requested ',this%arraysize,' elements, allocating ',newsize
#endif
  tmpptr => this%array ! keep a pointer to the old data
  ALLOCATE(this%array(newsize))
  copysize = MIN(this%arraysize, SIZE(tmpptr)) ! restrict to valid intervals
  this%array(1:copysize) = tmpptr(1:copysize) ! copy the old data
  DEALLOCATE(tmpptr) ! and destroy them
ELSE ! need to allocate from scratch
#ifdef DEBUG
!  PRINT*,'ARRAYOF: first time requested ',this%arraysize,' elements, allocating ',newsize
#endif
  ALLOCATE(this%array(newsize))
ENDIF

END SUBROUTINE ARRAYOF_TYPE/**/_alloc
