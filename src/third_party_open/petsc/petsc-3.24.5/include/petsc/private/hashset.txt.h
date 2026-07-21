/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetI - Hash set with a key of PetscInt

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetICreate()`, `PetscHSetIDestroy()`, `PetscHSetIQueryAdd()`, `PetscHSetIDel()`,
          `PetscHSetIAdd()`, `PetscHSetIReset()`, `PETSC_HASH_MAP()`, `PetscHMapICreate()`,  `PetscHSetI`
S*/
typedef struct _PetscHashI PetscHSetI;

/*MC
  PetscHSetICreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetICreate(PetscHSetI *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIDestroy()`
M*/

/*MC
  PetscHSetIDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIDestroy(PetscHSetI *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetICreate()`
M*/

/*MC
  PetscHSetIReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIReset(PetscHSetI ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIClear()`
M*/

/*MC
  PetscHSetIDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIDuplicate(PetscHSetI ht,PetscHSetI *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetICreate()`
M*/

/*MC
  PetscHSetIUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIUpdate(PetscHSetI ht,PetscHSetI hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetICreate()`, `PetscHSetIDuplicate()`
M*/

/*MC
  PetscHSetIClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIClear(PetscHSetI ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIReset()`
M*/

/*MC
  PetscHSetIResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIResize(PetscHSetI ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetICreate()`
M*/

/*MC
  PetscHSetIGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIGetSize(PetscHSetI ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetIResize()`
M*/

/*MC
  PetscHSetIGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIGetCapacity(PetscHSetI ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetIResize()`, `PetscHSetIGetSize()`
M*/

/*MC
  PetscHSetIHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIHas(PetscHSetI ht, PetscInt key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetIAdd()`, `PetscHSetIDel()`, `PetscHSetIQueryAdd()`
M*/

/*MC
  PetscHSetIAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIAdd(PetscHSetI ht, PetscInt key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIDel()`, `PetscHSetIHas()`, `PetscHSetIQueryAdd()`
M*/

/*MC
  PetscHSetIDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIDel(PetscHSetI ht, PetscInt key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIAdd()`, `PetscHSetIHas()`
M*/

/*MC
  PetscHSetIQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIQueryAdd(PetscHSetI ht, PetscInt key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetIQueryDel()`, `PetscHSetIAdd()`, `PetscHSetIHas()`
M*/

/*MC
  PetscHSetIQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIQueryDel(PetscHSetI ht, PetscInt key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetIQueryAdd()`, `PetscHSetIDel()`
M*/

/*MC
  PetscHSetIGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIGetElems(PetscHSetI ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIGetSize()`
M*/
/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetIJ - Hash set with a key of struct {PetscInt i, j;}

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetIJCreate()`, `PetscHSetIJDestroy()`, `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`,
          `PetscHSetIJAdd()`, `PetscHSetIJReset()`, `PETSC_HASH_MAP()`, `PetscHMapIJCreate()`,  `PetscHSetIJ`
S*/
typedef struct _PetscHashIJ PetscHSetIJ;

/*MC
  PetscHSetIJCreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJCreate(PetscHSetIJ *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJDestroy()`
M*/

/*MC
  PetscHSetIJDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDestroy(PetscHSetIJ *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJReset(PetscHSetIJ ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJClear()`
M*/

/*MC
  PetscHSetIJDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDuplicate(PetscHSetIJ ht,PetscHSetIJ *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJUpdate(PetscHSetIJ ht,PetscHSetIJ hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`, `PetscHSetIJDuplicate()`
M*/

/*MC
  PetscHSetIJClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJClear(PetscHSetIJ ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJReset()`
M*/

/*MC
  PetscHSetIJResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJResize(PetscHSetIJ ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetSize(PetscHSetIJ ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetIJResize()`
M*/

/*MC
  PetscHSetIJGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetCapacity(PetscHSetIJ ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetIJResize()`, `PetscHSetIJGetSize()`
M*/

/*MC
  PetscHSetIJHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJHas(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetIJAdd()`, `PetscHSetIJDel()`, `PetscHSetIJQueryAdd()`
M*/

/*MC
  PetscHSetIJAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJAdd(PetscHSetIJ ht, struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIJDel()`, `PetscHSetIJHas()`, `PetscHSetIJQueryAdd()`
M*/

/*MC
  PetscHSetIJDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDel(PetscHSetIJ ht, struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIJAdd()`, `PetscHSetIJHas()`
M*/

/*MC
  PetscHSetIJQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJQueryAdd(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetIJQueryDel()`, `PetscHSetIJAdd()`, `PetscHSetIJHas()`
M*/

/*MC
  PetscHSetIJQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJQueryDel(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`
M*/

/*MC
  PetscHSetIJGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetElems(PetscHSetIJ ht, PetscInt *off, struct {PetscInt i, j;} array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIJGetSize()`
M*/
/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetI - Hash set with a key of PetscInt

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetICreate()`, `PetscHSetIDestroy()`, `PetscHSetIQueryAdd()`, `PetscHSetIDel()`,
          `PetscHSetIAdd()`, `PetscHSetIReset()`, `PETSC_HASH_MAP()`, `PetscHMapICreate()`,  `PetscHSetI`
S*/
typedef struct _PetscHashI PetscHSetI;

/*MC
  PetscHSetICreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetICreate(PetscHSetI *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIDestroy()`
M*/

/*MC
  PetscHSetIDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIDestroy(PetscHSetI *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetICreate()`
M*/

/*MC
  PetscHSetIReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIReset(PetscHSetI ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIClear()`
M*/

/*MC
  PetscHSetIDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIDuplicate(PetscHSetI ht,PetscHSetI *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetICreate()`
M*/

/*MC
  PetscHSetIUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIUpdate(PetscHSetI ht,PetscHSetI hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetICreate()`, `PetscHSetIDuplicate()`
M*/

/*MC
  PetscHSetIClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIClear(PetscHSetI ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIReset()`
M*/

/*MC
  PetscHSetIResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIResize(PetscHSetI ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetICreate()`
M*/

/*MC
  PetscHSetIGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIGetSize(PetscHSetI ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetIResize()`
M*/

/*MC
  PetscHSetIGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIGetCapacity(PetscHSetI ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetIResize()`, `PetscHSetIGetSize()`
M*/

/*MC
  PetscHSetIHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIHas(PetscHSetI ht, PetscInt key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetIAdd()`, `PetscHSetIDel()`, `PetscHSetIQueryAdd()`
M*/

/*MC
  PetscHSetIAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIAdd(PetscHSetI ht, PetscInt key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIDel()`, `PetscHSetIHas()`, `PetscHSetIQueryAdd()`
M*/

/*MC
  PetscHSetIDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIDel(PetscHSetI ht, PetscInt key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIAdd()`, `PetscHSetIHas()`
M*/

/*MC
  PetscHSetIQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIQueryAdd(PetscHSetI ht, PetscInt key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetIQueryDel()`, `PetscHSetIAdd()`, `PetscHSetIHas()`
M*/

/*MC
  PetscHSetIQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIQueryDel(PetscHSetI ht, PetscInt key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetIQueryAdd()`, `PetscHSetIDel()`
M*/

/*MC
  PetscHSetIGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashseti.h>
  PetscErrorCode PetscHSetIGetElems(PetscHSetI ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIGetSize()`
M*/
/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetIJ - Hash set with a key of struct {PetscInt i, j;}

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetIJCreate()`, `PetscHSetIJDestroy()`, `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`,
          `PetscHSetIJAdd()`, `PetscHSetIJReset()`, `PETSC_HASH_MAP()`, `PetscHMapIJCreate()`,  `PetscHSetIJ`
S*/
typedef struct _PetscHashIJ PetscHSetIJ;

/*MC
  PetscHSetIJCreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJCreate(PetscHSetIJ *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJDestroy()`
M*/

/*MC
  PetscHSetIJDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDestroy(PetscHSetIJ *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJReset(PetscHSetIJ ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJClear()`
M*/

/*MC
  PetscHSetIJDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDuplicate(PetscHSetIJ ht,PetscHSetIJ *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJUpdate(PetscHSetIJ ht,PetscHSetIJ hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`, `PetscHSetIJDuplicate()`
M*/

/*MC
  PetscHSetIJClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJClear(PetscHSetIJ ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJReset()`
M*/

/*MC
  PetscHSetIJResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJResize(PetscHSetIJ ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetSize(PetscHSetIJ ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetIJResize()`
M*/

/*MC
  PetscHSetIJGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetCapacity(PetscHSetIJ ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetIJResize()`, `PetscHSetIJGetSize()`
M*/

/*MC
  PetscHSetIJHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJHas(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetIJAdd()`, `PetscHSetIJDel()`, `PetscHSetIJQueryAdd()`
M*/

/*MC
  PetscHSetIJAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJAdd(PetscHSetIJ ht, struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIJDel()`, `PetscHSetIJHas()`, `PetscHSetIJQueryAdd()`
M*/

/*MC
  PetscHSetIJDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDel(PetscHSetIJ ht, struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIJAdd()`, `PetscHSetIJHas()`
M*/

/*MC
  PetscHSetIJQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJQueryAdd(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetIJQueryDel()`, `PetscHSetIJAdd()`, `PetscHSetIJHas()`
M*/

/*MC
  PetscHSetIJQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJQueryDel(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`
M*/

/*MC
  PetscHSetIJGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetElems(PetscHSetIJ ht, PetscInt *off, struct {PetscInt i, j;} array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIJGetSize()`
M*/
/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetIJ - Hash set with a key of struct {PetscInt i, j;}

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetIJCreate()`, `PetscHSetIJDestroy()`, `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`,
          `PetscHSetIJAdd()`, `PetscHSetIJReset()`, `PETSC_HASH_MAP()`, `PetscHMapIJCreate()`,  `PetscHSetIJ`
S*/
typedef struct _PetscHashIJ PetscHSetIJ;

/*MC
  PetscHSetIJCreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJCreate(PetscHSetIJ *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJDestroy()`
M*/

/*MC
  PetscHSetIJDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDestroy(PetscHSetIJ *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJReset(PetscHSetIJ ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJClear()`
M*/

/*MC
  PetscHSetIJDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDuplicate(PetscHSetIJ ht,PetscHSetIJ *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJUpdate(PetscHSetIJ ht,PetscHSetIJ hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetIJCreate()`, `PetscHSetIJDuplicate()`
M*/

/*MC
  PetscHSetIJClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJClear(PetscHSetIJ ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIJReset()`
M*/

/*MC
  PetscHSetIJResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJResize(PetscHSetIJ ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetIJCreate()`
M*/

/*MC
  PetscHSetIJGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetSize(PetscHSetIJ ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetIJResize()`
M*/

/*MC
  PetscHSetIJGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetCapacity(PetscHSetIJ ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetIJResize()`, `PetscHSetIJGetSize()`
M*/

/*MC
  PetscHSetIJHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJHas(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetIJAdd()`, `PetscHSetIJDel()`, `PetscHSetIJQueryAdd()`
M*/

/*MC
  PetscHSetIJAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJAdd(PetscHSetIJ ht, struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIJDel()`, `PetscHSetIJHas()`, `PetscHSetIJQueryAdd()`
M*/

/*MC
  PetscHSetIJDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJDel(PetscHSetIJ ht, struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIJAdd()`, `PetscHSetIJHas()`
M*/

/*MC
  PetscHSetIJQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJQueryAdd(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetIJQueryDel()`, `PetscHSetIJAdd()`, `PetscHSetIJHas()`
M*/

/*MC
  PetscHSetIJQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJQueryDel(PetscHSetIJ ht, struct {PetscInt i, j;} key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`
M*/

/*MC
  PetscHSetIJGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashsetij.h>
  PetscErrorCode PetscHSetIJGetElems(PetscHSetIJ ht, PetscInt *off, struct {PetscInt i, j;} array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIJGetSize()`
M*/
/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetIV - Hash set with a key of PetscInt

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetIVCreate()`, `PetscHSetIVDestroy()`, `PetscHSetIVQueryAdd()`, `PetscHSetIVDel()`,
          `PetscHSetIVAdd()`, `PetscHSetIVReset()`, `PETSC_HASH_MAP()`, `PetscHMapIVCreate()`,  `PetscHSetIV`
S*/
typedef struct _PetscHashIV PetscHSetIV;

/*MC
  PetscHSetIVCreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVCreate(PetscHSetIV *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIVDestroy()`
M*/

/*MC
  PetscHSetIVDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVDestroy(PetscHSetIV *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIVCreate()`
M*/

/*MC
  PetscHSetIVReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVReset(PetscHSetIV ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIVClear()`
M*/

/*MC
  PetscHSetIVDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVDuplicate(PetscHSetIV ht,PetscHSetIV *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetIVCreate()`
M*/

/*MC
  PetscHSetIVUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVUpdate(PetscHSetIV ht,PetscHSetIV hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetIVCreate()`, `PetscHSetIVDuplicate()`
M*/

/*MC
  PetscHSetIVClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVClear(PetscHSetIV ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetIVReset()`
M*/

/*MC
  PetscHSetIVResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVResize(PetscHSetIV ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetIVCreate()`
M*/

/*MC
  PetscHSetIVGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVGetSize(PetscHSetIV ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetIVResize()`
M*/

/*MC
  PetscHSetIVGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVGetCapacity(PetscHSetIV ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetIVResize()`, `PetscHSetIVGetSize()`
M*/

/*MC
  PetscHSetIVHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVHas(PetscHSetIV ht, PetscInt key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetIVAdd()`, `PetscHSetIVDel()`, `PetscHSetIVQueryAdd()`
M*/

/*MC
  PetscHSetIVAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVAdd(PetscHSetIV ht, PetscInt key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIVDel()`, `PetscHSetIVHas()`, `PetscHSetIVQueryAdd()`
M*/

/*MC
  PetscHSetIVDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVDel(PetscHSetIV ht, PetscInt key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetIVAdd()`, `PetscHSetIVHas()`
M*/

/*MC
  PetscHSetIVQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVQueryAdd(PetscHSetIV ht, PetscInt key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetIVQueryDel()`, `PetscHSetIVAdd()`, `PetscHSetIVHas()`
M*/

/*MC
  PetscHSetIVQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVQueryDel(PetscHSetIV ht, PetscInt key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetIVQueryAdd()`, `PetscHSetIVDel()`
M*/

/*MC
  PetscHSetIVGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashsetiv.h>
  PetscErrorCode PetscHSetIVGetElems(PetscHSetIV ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIVGetSize()`
M*/
/* MANSEC = Sys */
/* SUBMANSEC = PetscH */

This file is processed by doc/build_man_pages.py to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHHashIJ.

/*S
  PetscHSetObj - Hash set with a key of PetscInt64

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetObjCreate()`, `PetscHSetObjDestroy()`, `PetscHSetObjQueryAdd()`, `PetscHSetObjDel()`,
          `PetscHSetObjAdd()`, `PetscHSetObjReset()`, `PETSC_HASH_MAP()`, `PetscHMapObjCreate()`,  `PetscHSetObj`
S*/
typedef struct _PetscHashObj PetscHSetObj;

/*MC
  PetscHSetObjCreate - Create a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjCreate(PetscHSetObj *ht)

  Output Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetObjDestroy()`
M*/

/*MC
  PetscHSetObjDestroy - Destroy a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjDestroy(PetscHSetObj *ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetObjCreate()`
M*/

/*MC
  PetscHSetObjReset - Reset a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjReset(PetscHSetObj ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetObjClear()`
M*/

/*MC
  PetscHSetObjDuplicate - Duplicate a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjDuplicate(PetscHSetObj ht,PetscHSetObj *hd)

  Input Parameter:
. ht - The source hash set

  Output Parameter:
. ht - The duplicated hash set

  Level: developer

.seealso: `PetscHSetObjCreate()`
M*/

/*MC
  PetscHSetObjUpdate - Add entries from a has set to another

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjUpdate(PetscHSetObj ht,PetscHSetObj hda)

  Input Parameters:
+ ht - The hash set to which elements are added
- hta - The hash set from which the elements are retrieved

  Output Parameter:
. ht - The hash set filled with the elements from the other hash set

  Level: developer

.seealso: `PetscHSetObjCreate()`, `PetscHSetObjDuplicate()`
M*/

/*MC
  PetscHSetObjClear - Clear a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjClear(PetscHSetObj ht)

  Input Parameter:
. ht - The hash set

  Level: developer

.seealso: `PetscHSetObjReset()`
M*/

/*MC
  PetscHSetObjResize - Set the number of buckets in a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjResize(PetscHSetObj ht,PetscInt nb)

  Input Parameters:
+ ht - The hash set
- nb - The number of buckets

  Level: developer

.seealso: `PetscHSetObjCreate()`
M*/

/*MC
  PetscHSetObjGetSize - Get the number of entries in a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjGetSize(PetscHSetObj ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHSetObjResize()`
M*/

/*MC
  PetscHSetObjGetCapacity - Get the current size of the array in the hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjGetCapacity(PetscHSetObj ht,PetscInt *n)

  Input Parameter:
. ht - The hash set

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHSetObjResize()`, `PetscHSetObjGetSize()`
M*/

/*MC
  PetscHSetObjHas - Query for an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjHas(PetscHSetObj ht, PetscInt64 key, PetscBool *has)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. has - Boolean indicating whether the entry is in the hash set

  Level: developer

.seealso: `PetscHSetObjAdd()`, `PetscHSetObjDel()`, `PetscHSetObjQueryAdd()`
M*/

/*MC
  PetscHSetObjAdd - Set an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjAdd(PetscHSetObj ht, PetscInt64 key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetObjDel()`, `PetscHSetObjHas()`, `PetscHSetObjQueryAdd()`
M*/

/*MC
  PetscHSetObjDel - Remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjDel(PetscHSetObj ht, PetscInt64 key)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Level: developer

.seealso: `PetscHSetObjAdd()`, `PetscHSetObjHas()`
M*/

/*MC
  PetscHSetObjQueryAdd - Query and add an entry in the hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjQueryAdd(PetscHSetObj ht, PetscInt64 key, PetscBool *missing)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. missing - Boolean indicating whether the entry was missing

  Level: developer

.seealso: `PetscHSetObjQueryDel()`, `PetscHSetObjAdd()`, `PetscHSetObjHas()`
M*/

/*MC
  PetscHSetObjQueryDel - Query and remove an entry from the hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjQueryDel(PetscHSetObj ht, PetscInt64 key, PetscBool *present)

  Input Parameters:
+ ht  - The hash set
- key - The entry

  Output Parameter:
. present - Boolean indicating whether the entry was present

  Level: developer

.seealso: `PetscHSetObjQueryAdd()`, `PetscHSetObjDel()`
M*/

/*MC
  PetscHSetObjGetElems - Get all entries from a hash set

  Synopsis:
  #include <petsc/private/hashsetobj.h>
  PetscErrorCode PetscHSetObjGetElems(PetscHSetObj ht, PetscInt *off, PetscInt64 array[])

  Input Parameters:
+ ht    - The hash set
. off   - Input offset in array (usually zero)
- array - Array to put hash set entries in

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetObjGetSize()`
M*/
