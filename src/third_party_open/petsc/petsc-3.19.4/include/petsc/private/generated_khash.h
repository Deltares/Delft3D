 /* SUBMANSEC = PetscH */ 

This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHSetIJ.

/*S
  PetscHSetI - Hash set with a key of <Type>

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetICreate()`, `PetscHSetIDestroy()`, `PetscHSetIQueryAdd()`, `PetscHSetIDel()`,
          `PetscHSetIAdd()`, PetscHSetIReset()`
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
- array - Array where to put hash set entries into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIGetSize()`
M*/

This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_SET(). For example, PetscHSetIJ.

/*S
  PetscHSetIJ - Hash set with a key of <Type>

  Level: developer

.seealso: `PETSC_HASH_SET()`, `PetscHSetIJCreate()`, `PetscHSetIJDestroy()`, `PetscHSetIJQueryAdd()`, `PetscHSetIJDel()`,
          `PetscHSetIJAdd()`, PetscHSetIJReset()`
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
- array - Array where to put hash set entries into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash set size)
- array - Array filled with the hash set entries

  Level: developer

.seealso: `PetscHSetIJGetSize()`
M*/
This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_MAP(). For example, PetscHMAPIJ.

/*S
  PetscHMapI - Hash table map

  Synopsis:
  typedef khash_t(HMapI) *PetscHMapI;

  Level: developer

.seealso: `PETSC_HASH_MAP()`, `PetscHMapICreate()`
S*/
typedef struct _PetscHashI PetscHMapI;

/*MC
  PetscHMapICreate - Create a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapICreate(PetscHMapI *ht)

  Output Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapICreateWithSize()`, `PetscHMapIDestroy()`
M*/

/*MC
  PetscHMapICreateWithSize - Create a hash table with a given initial size

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapICreateWithSize(PetscInt n, PetscHMapI *ht)

  Input Parameter:
. n - The size of the hash table

  Output Parameter:
. ht - The hash table

  Level: developer

  Note:
  `n` must be non-negative.

.seealso: `PetscHMapICreate()`, `PetscHMapIDestroy()`
M*/

/*MC
  PetscHMapIDestroy - Destroy a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIDestroy(PetscHMapI *ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapICreate()`, `PetscHMapICreateWithSize()`
M*/

/*MC
  PetscHMapIReset - Reset a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIReset(PetscHMapI ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIClear()`
M*/

/*MC
  PetscHMapIDuplicate - Duplicate a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIDuplicate(PetscHMapI ht, PetscHMapI *hd)

  Input Parameter:
. ht - The source hash table

  Output Parameter:
. ht - The duplicated hash table

  Level: developer

.seealso: `PetscHMapICreate()`
M*/

/*MC
  PetscHMapIClear - Clear a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIClear(PetscHMapI ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIReset()`
M*/

/*MC
  PetscHMapIResize - Set the number of buckets in a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIResize(PetscHMapI ht, PetscInt nb)

  Input Parameters:
+ ht - The hash table
- nb - The number of buckets

  Level: developer

.seealso: `PetscHMapICreate()`
M*/

/*MC
  PetscHMapIGetSize - Get the number of entries in a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGetSize(PetscHMapI ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHMapIResize()`
M*/

/*MC
  PetscHMapIGetCapacity - Get the current size of the array in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGetCapacity(PetscHMapI ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHMapIResize()`, `PetscHMapIGetSize()`
M*/

/*MC
  PetscHMapIHas - Query for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIHas(PetscHMapI ht, PetscInt key, PetscBool *has)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. has - Boolean indicating whether key is in the hash table

  Level: developer

.seealso: `PetscHMapIGet()`, `PetscHMapIGetWithDefault()`, `PetscHMapISet()`,
`PetscHMapISetWithMode()`, `PetscHMapIFind()`
M*/

/*MC
  PetscHMapIGet - Get the value for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGet(PetscHMapI ht, PetscInt key, PetscInt *val)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapISet()`, `PetscHMapISetWithMode()`, `PetscHMapIIterGet()`,
`PetscHMapIGetWithDefault()`
M*/

/*MC
  PetscHMapIGetWithDefault - Get the value for a key in the hash table but override the default
  value returned if the key was not found

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGetWithDefault(PetscHMapI ht, PetscInt key, PetscInt default_val, PetscInt *val)

  Input Parameters:
+ ht          - The hash table
. key         - The key
- default_val - The default value to set `val` to if `key` was not found

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIGet()`, `PetscHMapISet()`, `PetscHMapISetWithMode()`, `PetscHMapIIterGet()`
M*/


/*MC
  PetscHMapISet - Set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapISet(PetscHMapI ht, PetscInt key, PetscInt val)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Level: developer

.seealso: `PetscHMapIGet()`, `PetscHMapISetWithMode()`, `PetscHMapIGetWithDefault()`,
`PetscHMapIIterSet()`
M*/

/*MC
  PetscHMapISetWithMode - Set a (key,value) entry in the hash table according to an `InsertMode`

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapISetWithMode(PetscHMapI ht, PetscInt key, PetscInt val, InsertMode mode)

  Input Parameters:
+ ht   - The hash table
. key  - The key
. val  - The value
- mode - The insertion mode

  Level: developer

  Notes:
  `mode` may be any of the following\:
  - `INSERT_VALUES`\: this routine behaves identically to `PetscHMapISet()`.
  - `ADD_VALUES`\: if `key` is found `val` is added to the current entry, otherwise (`key`, `value`)
                   is inserted into `ht` as-if-by `INSERT_VALUES`.
  - `MAX_VALUES`\: if `key` is found the current value is replaced by the maximum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.
  - `MIN_VALUES`\: if `key` is found the current value is replaced by the minimum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.

   All other `InsertMode`s will raise an error if used.

   Since this routine relies on `+`, `<`, and `>` being well-formed for a particular type
   it is not available by default for all PETSc hash table instantiations. If a particular
   instantiation supports this routine it must define `PETSC_HMAPI_HAVE_EXTENDED_API` to
   `1`.

.seealso: `PetscHMapISet()`, `PetscHMapIGet()`, `PetscHMapIGetWithDefault()`,
`PetscHMapIIterSet()`
M*/

/*MC
  PetscHMapIDel - Remove a key and its value from the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIDel(PetscHMapI ht,PetscInt key)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Level: developer

.seealso: `PetscHMapIHas()`, `PetscHMapIIterDel()`
M*/

/*MC
  PetscHMapIQuerySet - Query and set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIQuerySet(PetscHMapI ht, PetscInt key, PetscInt val, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Output Parameter:
. missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIQueryDel()`, `PetscHMapISet()`, `PetscHMapISetWithMode()`
M*/

/*MC
  PetscHMapIQueryDel - Query and remove a (key,value) entry from the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIQueryDel(PetscHMapI ht, PetscInt key, PetscBool *present)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. present - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIQuerySet()`, `PetscHMapIDel()`
M*/

/*MC
  PetscHMapIFind - Query for key in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIFind(PetscHMapI ht, PetscInt key, PetscHashIter *iter, PetscBool *found)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- found - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIIterGet()`, `PetscHMapIIterDel()`
M*/

/*MC
  PetscHMapIPut - Set a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIPut(PetscHMapI ht, PetscInt key, PetscHashIter *iter, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIIterSet()`, `PetscHMapIQuerySet()`, `PetscHMapISet()`,
`PetscHMapISetWithMode()`
M*/

/*MC
  PetscHMapIIterGet - Get the value referenced by an iterator in the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIIterGet(PetscHMapI ht, PetscHashIter iter, PetscInt *val)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Output Parameter:
. val  - The value

  Level: developer

.seealso: `PetscHMapIFind()`, `PetscHMapIGet()`, `PetscHMapIGetWithDefault()`
M*/

/*MC
  PetscHMapIIterSet - Set the value referenced by an iterator in the hash

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIIterSet(PetscHMapI ht, PetscHashIter iter, PetscInt val)

  Input Parameters:
+ ht   - The hash table
. iter - The iterator
- val  - The value

  Level: developer

.seealso: `PetscHMapIPut()`, `PetscHMapIQuerySet()`, `PetscHMapISet()`,
`PetscHMapISetWithMode()`
M*/

/*MC
  PetscHMapIIterDel - Remove the (key,value) referenced by an iterator from the hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIIterDel(PetscHMapI ht, PetscHashIter iter)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Level: developer

.seealso: `PetscHMapIFind()`, `PetscHMapIQueryDel()`, `PetscHMapIDel()`
M*/

/*MC
  PetscHMapIGetKeys - Get all keys from a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGetKeys(PetscHMapI ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table keys into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table keys

  Level: developer

.seealso: `PetscHSetIGetSize()`, `PetscHMapIGetVals()`
M*/

/*MC
  PetscHMapIGetVals - Get all values from a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGetVals(PetscHMapI ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIGetSize()`, `PetscHMapIGetKeys()`
M*/

/*MC
  PetscHMapIGetPairs - Get all (key,value) pairs from a hash table

  Synopsis:
  #include <petsc/private/hashmapi.h>
  PetscErrorCode PetscHMapIGetPairs(PetscHMapI ht, PetscInt *off, PetscInt karray[], PetscInt varray[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
. karray - Array where to put hash table keys into
- varray - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
. karray - Array filled with the hash table keys
- varray - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIGetSize()`, `PetscHMapIGetKeys()`, `PetscHMapIGetVals()`
M*/
This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_MAP(). For example, PetscHMAPIJ.

/*S
  PetscHMapIJ - Hash table map

  Synopsis:
  typedef khash_t(HMapIJ) *PetscHMapIJ;

  Level: developer

.seealso: `PETSC_HASH_MAP()`, `PetscHMapIJCreate()`
S*/
typedef struct _PetscHashIJ PetscHMapIJ;

/*MC
  PetscHMapIJCreate - Create a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJCreate(PetscHMapIJ *ht)

  Output Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJCreateWithSize()`, `PetscHMapIJDestroy()`
M*/

/*MC
  PetscHMapIJCreateWithSize - Create a hash table with a given initial size

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJCreateWithSize(PetscInt n, PetscHMapIJ *ht)

  Input Parameter:
. n - The size of the hash table

  Output Parameter:
. ht - The hash table

  Level: developer

  Note:
  `n` must be non-negative.

.seealso: `PetscHMapIJCreate()`, `PetscHMapIJDestroy()`
M*/

/*MC
  PetscHMapIJDestroy - Destroy a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJDestroy(PetscHMapIJ *ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJCreate()`, `PetscHMapIJCreateWithSize()`
M*/

/*MC
  PetscHMapIJReset - Reset a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJReset(PetscHMapIJ ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJClear()`
M*/

/*MC
  PetscHMapIJDuplicate - Duplicate a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJDuplicate(PetscHMapIJ ht, PetscHMapIJ *hd)

  Input Parameter:
. ht - The source hash table

  Output Parameter:
. ht - The duplicated hash table

  Level: developer

.seealso: `PetscHMapIJCreate()`
M*/

/*MC
  PetscHMapIJClear - Clear a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJClear(PetscHMapIJ ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJReset()`
M*/

/*MC
  PetscHMapIJResize - Set the number of buckets in a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJResize(PetscHMapIJ ht, PetscInt nb)

  Input Parameters:
+ ht - The hash table
- nb - The number of buckets

  Level: developer

.seealso: `PetscHMapIJCreate()`
M*/

/*MC
  PetscHMapIJGetSize - Get the number of entries in a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetSize(PetscHMapIJ ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHMapIJResize()`
M*/

/*MC
  PetscHMapIJGetCapacity - Get the current size of the array in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetCapacity(PetscHMapIJ ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHMapIJResize()`, `PetscHMapIJGetSize()`
M*/

/*MC
  PetscHMapIJHas - Query for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJHas(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscBool *has)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. has - Boolean indicating whether key is in the hash table

  Level: developer

.seealso: `PetscHMapIJGet()`, `PetscHMapIJGetWithDefault()`, `PetscHMapIJSet()`,
`PetscHMapIJSetWithMode()`, `PetscHMapIJFind()`
M*/

/*MC
  PetscHMapIJGet - Get the value for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGet(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscInt *val)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIJSet()`, `PetscHMapIJSetWithMode()`, `PetscHMapIJIterGet()`,
`PetscHMapIJGetWithDefault()`
M*/

/*MC
  PetscHMapIJGetWithDefault - Get the value for a key in the hash table but override the default
  value returned if the key was not found

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetWithDefault(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscInt default_val, PetscInt *val)

  Input Parameters:
+ ht          - The hash table
. key         - The key
- default_val - The default value to set `val` to if `key` was not found

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIJGet()`, `PetscHMapIJSet()`, `PetscHMapIJSetWithMode()`, `PetscHMapIJIterGet()`
M*/


/*MC
  PetscHMapIJSet - Set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJSet(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscInt val)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Level: developer

.seealso: `PetscHMapIJGet()`, `PetscHMapIJSetWithMode()`, `PetscHMapIJGetWithDefault()`,
`PetscHMapIJIterSet()`
M*/

/*MC
  PetscHMapIJSetWithMode - Set a (key,value) entry in the hash table according to an `InsertMode`

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJSetWithMode(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscInt val, InsertMode mode)

  Input Parameters:
+ ht   - The hash table
. key  - The key
. val  - The value
- mode - The insertion mode

  Level: developer

  Notes:
  `mode` may be any of the following\:
  - `INSERT_VALUES`\: this routine behaves identically to `PetscHMapIJSet()`.
  - `ADD_VALUES`\: if `key` is found `val` is added to the current entry, otherwise (`key`, `value`)
                   is inserted into `ht` as-if-by `INSERT_VALUES`.
  - `MAX_VALUES`\: if `key` is found the current value is replaced by the maximum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.
  - `MIN_VALUES`\: if `key` is found the current value is replaced by the minimum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.

   All other `InsertMode`s will raise an error if used.

   Since this routine relies on `+`, `<`, and `>` being well-formed for a particular type
   it is not available by default for all PETSc hash table instantiations. If a particular
   instantiation supports this routine it must define `PETSC_HMAPIJ_HAVE_EXTENDED_API` to
   `1`.

.seealso: `PetscHMapIJSet()`, `PetscHMapIJGet()`, `PetscHMapIJGetWithDefault()`,
`PetscHMapIJIterSet()`
M*/

/*MC
  PetscHMapIJDel - Remove a key and its value from the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJDel(PetscHMapIJ ht,struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Level: developer

.seealso: `PetscHMapIJHas()`, `PetscHMapIJIterDel()`
M*/

/*MC
  PetscHMapIJQuerySet - Query and set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJQuerySet(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscInt val, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Output Parameter:
. missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIJQueryDel()`, `PetscHMapIJSet()`, `PetscHMapIJSetWithMode()`
M*/

/*MC
  PetscHMapIJQueryDel - Query and remove a (key,value) entry from the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJQueryDel(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscBool *present)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. present - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIJQuerySet()`, `PetscHMapIJDel()`
M*/

/*MC
  PetscHMapIJFind - Query for key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJFind(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscHashIter *iter, PetscBool *found)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- found - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIJIterGet()`, `PetscHMapIJIterDel()`
M*/

/*MC
  PetscHMapIJPut - Set a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJPut(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscHashIter *iter, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIJIterSet()`, `PetscHMapIJQuerySet()`, `PetscHMapIJSet()`,
`PetscHMapIJSetWithMode()`
M*/

/*MC
  PetscHMapIJIterGet - Get the value referenced by an iterator in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJIterGet(PetscHMapIJ ht, PetscHashIter iter, PetscInt *val)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Output Parameter:
. val  - The value

  Level: developer

.seealso: `PetscHMapIJFind()`, `PetscHMapIJGet()`, `PetscHMapIJGetWithDefault()`
M*/

/*MC
  PetscHMapIJIterSet - Set the value referenced by an iterator in the hash

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJIterSet(PetscHMapIJ ht, PetscHashIter iter, PetscInt val)

  Input Parameters:
+ ht   - The hash table
. iter - The iterator
- val  - The value

  Level: developer

.seealso: `PetscHMapIJPut()`, `PetscHMapIJQuerySet()`, `PetscHMapIJSet()`,
`PetscHMapIJSetWithMode()`
M*/

/*MC
  PetscHMapIJIterDel - Remove the (key,value) referenced by an iterator from the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJIterDel(PetscHMapIJ ht, PetscHashIter iter)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Level: developer

.seealso: `PetscHMapIJFind()`, `PetscHMapIJQueryDel()`, `PetscHMapIJDel()`
M*/

/*MC
  PetscHMapIJGetKeys - Get all keys from a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetKeys(PetscHMapIJ ht, PetscInt *off, struct {PetscInt i, j;} array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table keys into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table keys

  Level: developer

.seealso: `PetscHSetIJGetSize()`, `PetscHMapIJGetVals()`
M*/

/*MC
  PetscHMapIJGetVals - Get all values from a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetVals(PetscHMapIJ ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIJGetSize()`, `PetscHMapIJGetKeys()`
M*/

/*MC
  PetscHMapIJGetPairs - Get all (key,value) pairs from a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetPairs(PetscHMapIJ ht, PetscInt *off, struct {PetscInt i, j;} karray[], PetscInt varray[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
. karray - Array where to put hash table keys into
- varray - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
. karray - Array filled with the hash table keys
- varray - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIJGetSize()`, `PetscHMapIJGetKeys()`, `PetscHMapIJGetVals()`
M*/
This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_MAP(). For example, PetscHMAPIJ.

/*S
  PetscHMapIJ - Hash table map

  Synopsis:
  typedef khash_t(HMapIJ) *PetscHMapIJ;

  Level: developer

.seealso: `PETSC_HASH_MAP()`, `PetscHMapIJCreate()`
S*/
typedef struct _PetscHashIJ PetscHMapIJ;

/*MC
  PetscHMapIJCreate - Create a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJCreate(PetscHMapIJ *ht)

  Output Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJCreateWithSize()`, `PetscHMapIJDestroy()`
M*/

/*MC
  PetscHMapIJCreateWithSize - Create a hash table with a given initial size

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJCreateWithSize(PetscInt n, PetscHMapIJ *ht)

  Input Parameter:
. n - The size of the hash table

  Output Parameter:
. ht - The hash table

  Level: developer

  Note:
  `n` must be non-negative.

.seealso: `PetscHMapIJCreate()`, `PetscHMapIJDestroy()`
M*/

/*MC
  PetscHMapIJDestroy - Destroy a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJDestroy(PetscHMapIJ *ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJCreate()`, `PetscHMapIJCreateWithSize()`
M*/

/*MC
  PetscHMapIJReset - Reset a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJReset(PetscHMapIJ ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJClear()`
M*/

/*MC
  PetscHMapIJDuplicate - Duplicate a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJDuplicate(PetscHMapIJ ht, PetscHMapIJ *hd)

  Input Parameter:
. ht - The source hash table

  Output Parameter:
. ht - The duplicated hash table

  Level: developer

.seealso: `PetscHMapIJCreate()`
M*/

/*MC
  PetscHMapIJClear - Clear a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJClear(PetscHMapIJ ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIJReset()`
M*/

/*MC
  PetscHMapIJResize - Set the number of buckets in a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJResize(PetscHMapIJ ht, PetscInt nb)

  Input Parameters:
+ ht - The hash table
- nb - The number of buckets

  Level: developer

.seealso: `PetscHMapIJCreate()`
M*/

/*MC
  PetscHMapIJGetSize - Get the number of entries in a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetSize(PetscHMapIJ ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHMapIJResize()`
M*/

/*MC
  PetscHMapIJGetCapacity - Get the current size of the array in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetCapacity(PetscHMapIJ ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHMapIJResize()`, `PetscHMapIJGetSize()`
M*/

/*MC
  PetscHMapIJHas - Query for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJHas(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscBool *has)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. has - Boolean indicating whether key is in the hash table

  Level: developer

.seealso: `PetscHMapIJGet()`, `PetscHMapIJGetWithDefault()`, `PetscHMapIJSet()`,
`PetscHMapIJSetWithMode()`, `PetscHMapIJFind()`
M*/

/*MC
  PetscHMapIJGet - Get the value for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGet(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscScalar *val)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIJSet()`, `PetscHMapIJSetWithMode()`, `PetscHMapIJIterGet()`,
`PetscHMapIJGetWithDefault()`
M*/

/*MC
  PetscHMapIJGetWithDefault - Get the value for a key in the hash table but override the default
  value returned if the key was not found

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetWithDefault(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscScalar default_val, PetscScalar *val)

  Input Parameters:
+ ht          - The hash table
. key         - The key
- default_val - The default value to set `val` to if `key` was not found

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIJGet()`, `PetscHMapIJSet()`, `PetscHMapIJSetWithMode()`, `PetscHMapIJIterGet()`
M*/


/*MC
  PetscHMapIJSet - Set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJSet(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscScalar val)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Level: developer

.seealso: `PetscHMapIJGet()`, `PetscHMapIJSetWithMode()`, `PetscHMapIJGetWithDefault()`,
`PetscHMapIJIterSet()`
M*/

/*MC
  PetscHMapIJSetWithMode - Set a (key,value) entry in the hash table according to an `InsertMode`

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJSetWithMode(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscScalar val, InsertMode mode)

  Input Parameters:
+ ht   - The hash table
. key  - The key
. val  - The value
- mode - The insertion mode

  Level: developer

  Notes:
  `mode` may be any of the following\:
  - `INSERT_VALUES`\: this routine behaves identically to `PetscHMapIJSet()`.
  - `ADD_VALUES`\: if `key` is found `val` is added to the current entry, otherwise (`key`, `value`)
                   is inserted into `ht` as-if-by `INSERT_VALUES`.
  - `MAX_VALUES`\: if `key` is found the current value is replaced by the maximum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.
  - `MIN_VALUES`\: if `key` is found the current value is replaced by the minimum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.

   All other `InsertMode`s will raise an error if used.

   Since this routine relies on `+`, `<`, and `>` being well-formed for a particular type
   it is not available by default for all PETSc hash table instantiations. If a particular
   instantiation supports this routine it must define `PETSC_HMAPIJ_HAVE_EXTENDED_API` to
   `1`.

.seealso: `PetscHMapIJSet()`, `PetscHMapIJGet()`, `PetscHMapIJGetWithDefault()`,
`PetscHMapIJIterSet()`
M*/

/*MC
  PetscHMapIJDel - Remove a key and its value from the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJDel(PetscHMapIJ ht,struct {PetscInt i, j;} key)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Level: developer

.seealso: `PetscHMapIJHas()`, `PetscHMapIJIterDel()`
M*/

/*MC
  PetscHMapIJQuerySet - Query and set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJQuerySet(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscScalar val, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Output Parameter:
. missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIJQueryDel()`, `PetscHMapIJSet()`, `PetscHMapIJSetWithMode()`
M*/

/*MC
  PetscHMapIJQueryDel - Query and remove a (key,value) entry from the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJQueryDel(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscBool *present)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. present - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIJQuerySet()`, `PetscHMapIJDel()`
M*/

/*MC
  PetscHMapIJFind - Query for key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJFind(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscHashIter *iter, PetscBool *found)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- found - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIJIterGet()`, `PetscHMapIJIterDel()`
M*/

/*MC
  PetscHMapIJPut - Set a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJPut(PetscHMapIJ ht, struct {PetscInt i, j;} key, PetscHashIter *iter, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIJIterSet()`, `PetscHMapIJQuerySet()`, `PetscHMapIJSet()`,
`PetscHMapIJSetWithMode()`
M*/

/*MC
  PetscHMapIJIterGet - Get the value referenced by an iterator in the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJIterGet(PetscHMapIJ ht, PetscHashIter iter, PetscScalar *val)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Output Parameter:
. val  - The value

  Level: developer

.seealso: `PetscHMapIJFind()`, `PetscHMapIJGet()`, `PetscHMapIJGetWithDefault()`
M*/

/*MC
  PetscHMapIJIterSet - Set the value referenced by an iterator in the hash

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJIterSet(PetscHMapIJ ht, PetscHashIter iter, PetscScalar val)

  Input Parameters:
+ ht   - The hash table
. iter - The iterator
- val  - The value

  Level: developer

.seealso: `PetscHMapIJPut()`, `PetscHMapIJQuerySet()`, `PetscHMapIJSet()`,
`PetscHMapIJSetWithMode()`
M*/

/*MC
  PetscHMapIJIterDel - Remove the (key,value) referenced by an iterator from the hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJIterDel(PetscHMapIJ ht, PetscHashIter iter)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Level: developer

.seealso: `PetscHMapIJFind()`, `PetscHMapIJQueryDel()`, `PetscHMapIJDel()`
M*/

/*MC
  PetscHMapIJGetKeys - Get all keys from a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetKeys(PetscHMapIJ ht, PetscInt *off, struct {PetscInt i, j;} array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table keys into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table keys

  Level: developer

.seealso: `PetscHSetIJGetSize()`, `PetscHMapIJGetVals()`
M*/

/*MC
  PetscHMapIJGetVals - Get all values from a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetVals(PetscHMapIJ ht, PetscInt *off, PetscScalar array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIJGetSize()`, `PetscHMapIJGetKeys()`
M*/

/*MC
  PetscHMapIJGetPairs - Get all (key,value) pairs from a hash table

  Synopsis:
  #include <petsc/private/hashmapij.h>
  PetscErrorCode PetscHMapIJGetPairs(PetscHMapIJ ht, PetscInt *off, struct {PetscInt i, j;} karray[], PetscScalar varray[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
. karray - Array where to put hash table keys into
- varray - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
. karray - Array filled with the hash table keys
- varray - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIJGetSize()`, `PetscHMapIJGetKeys()`, `PetscHMapIJGetVals()`
M*/
This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_MAP(). For example, PetscHMAPIJ.

/*S
  PetscHMapIV - Hash table map

  Synopsis:
  typedef khash_t(HMapIV) *PetscHMapIV;

  Level: developer

.seealso: `PETSC_HASH_MAP()`, `PetscHMapIVCreate()`
S*/
typedef struct _PetscHashIV PetscHMapIV;

/*MC
  PetscHMapIVCreate - Create a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVCreate(PetscHMapIV *ht)

  Output Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIVCreateWithSize()`, `PetscHMapIVDestroy()`
M*/

/*MC
  PetscHMapIVCreateWithSize - Create a hash table with a given initial size

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVCreateWithSize(PetscInt n, PetscHMapIV *ht)

  Input Parameter:
. n - The size of the hash table

  Output Parameter:
. ht - The hash table

  Level: developer

  Note:
  `n` must be non-negative.

.seealso: `PetscHMapIVCreate()`, `PetscHMapIVDestroy()`
M*/

/*MC
  PetscHMapIVDestroy - Destroy a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVDestroy(PetscHMapIV *ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIVCreate()`, `PetscHMapIVCreateWithSize()`
M*/

/*MC
  PetscHMapIVReset - Reset a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVReset(PetscHMapIV ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIVClear()`
M*/

/*MC
  PetscHMapIVDuplicate - Duplicate a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVDuplicate(PetscHMapIV ht, PetscHMapIV *hd)

  Input Parameter:
. ht - The source hash table

  Output Parameter:
. ht - The duplicated hash table

  Level: developer

.seealso: `PetscHMapIVCreate()`
M*/

/*MC
  PetscHMapIVClear - Clear a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVClear(PetscHMapIV ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapIVReset()`
M*/

/*MC
  PetscHMapIVResize - Set the number of buckets in a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVResize(PetscHMapIV ht, PetscInt nb)

  Input Parameters:
+ ht - The hash table
- nb - The number of buckets

  Level: developer

.seealso: `PetscHMapIVCreate()`
M*/

/*MC
  PetscHMapIVGetSize - Get the number of entries in a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGetSize(PetscHMapIV ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHMapIVResize()`
M*/

/*MC
  PetscHMapIVGetCapacity - Get the current size of the array in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGetCapacity(PetscHMapIV ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHMapIVResize()`, `PetscHMapIVGetSize()`
M*/

/*MC
  PetscHMapIVHas - Query for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVHas(PetscHMapIV ht, PetscInt key, PetscBool *has)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. has - Boolean indicating whether key is in the hash table

  Level: developer

.seealso: `PetscHMapIVGet()`, `PetscHMapIVGetWithDefault()`, `PetscHMapIVSet()`,
`PetscHMapIVSetWithMode()`, `PetscHMapIVFind()`
M*/

/*MC
  PetscHMapIVGet - Get the value for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGet(PetscHMapIV ht, PetscInt key, PetscScalar *val)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIVSet()`, `PetscHMapIVSetWithMode()`, `PetscHMapIVIterGet()`,
`PetscHMapIVGetWithDefault()`
M*/

/*MC
  PetscHMapIVGetWithDefault - Get the value for a key in the hash table but override the default
  value returned if the key was not found

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGetWithDefault(PetscHMapIV ht, PetscInt key, PetscScalar default_val, PetscScalar *val)

  Input Parameters:
+ ht          - The hash table
. key         - The key
- default_val - The default value to set `val` to if `key` was not found

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapIVGet()`, `PetscHMapIVSet()`, `PetscHMapIVSetWithMode()`, `PetscHMapIVIterGet()`
M*/


/*MC
  PetscHMapIVSet - Set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVSet(PetscHMapIV ht, PetscInt key, PetscScalar val)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Level: developer

.seealso: `PetscHMapIVGet()`, `PetscHMapIVSetWithMode()`, `PetscHMapIVGetWithDefault()`,
`PetscHMapIVIterSet()`
M*/

/*MC
  PetscHMapIVSetWithMode - Set a (key,value) entry in the hash table according to an `InsertMode`

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVSetWithMode(PetscHMapIV ht, PetscInt key, PetscScalar val, InsertMode mode)

  Input Parameters:
+ ht   - The hash table
. key  - The key
. val  - The value
- mode - The insertion mode

  Level: developer

  Notes:
  `mode` may be any of the following\:
  - `INSERT_VALUES`\: this routine behaves identically to `PetscHMapIVSet()`.
  - `ADD_VALUES`\: if `key` is found `val` is added to the current entry, otherwise (`key`, `value`)
                   is inserted into `ht` as-if-by `INSERT_VALUES`.
  - `MAX_VALUES`\: if `key` is found the current value is replaced by the maximum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.
  - `MIN_VALUES`\: if `key` is found the current value is replaced by the minimum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.

   All other `InsertMode`s will raise an error if used.

   Since this routine relies on `+`, `<`, and `>` being well-formed for a particular type
   it is not available by default for all PETSc hash table instantiations. If a particular
   instantiation supports this routine it must define `PETSC_HMAPIV_HAVE_EXTENDED_API` to
   `1`.

.seealso: `PetscHMapIVSet()`, `PetscHMapIVGet()`, `PetscHMapIVGetWithDefault()`,
`PetscHMapIVIterSet()`
M*/

/*MC
  PetscHMapIVDel - Remove a key and its value from the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVDel(PetscHMapIV ht,PetscInt key)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Level: developer

.seealso: `PetscHMapIVHas()`, `PetscHMapIVIterDel()`
M*/

/*MC
  PetscHMapIVQuerySet - Query and set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVQuerySet(PetscHMapIV ht, PetscInt key, PetscScalar val, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Output Parameter:
. missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIVQueryDel()`, `PetscHMapIVSet()`, `PetscHMapIVSetWithMode()`
M*/

/*MC
  PetscHMapIVQueryDel - Query and remove a (key,value) entry from the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVQueryDel(PetscHMapIV ht, PetscInt key, PetscBool *present)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. present - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIVQuerySet()`, `PetscHMapIVDel()`
M*/

/*MC
  PetscHMapIVFind - Query for key in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVFind(PetscHMapIV ht, PetscInt key, PetscHashIter *iter, PetscBool *found)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- found - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapIVIterGet()`, `PetscHMapIVIterDel()`
M*/

/*MC
  PetscHMapIVPut - Set a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVPut(PetscHMapIV ht, PetscInt key, PetscHashIter *iter, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapIVIterSet()`, `PetscHMapIVQuerySet()`, `PetscHMapIVSet()`,
`PetscHMapIVSetWithMode()`
M*/

/*MC
  PetscHMapIVIterGet - Get the value referenced by an iterator in the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVIterGet(PetscHMapIV ht, PetscHashIter iter, PetscScalar *val)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Output Parameter:
. val  - The value

  Level: developer

.seealso: `PetscHMapIVFind()`, `PetscHMapIVGet()`, `PetscHMapIVGetWithDefault()`
M*/

/*MC
  PetscHMapIVIterSet - Set the value referenced by an iterator in the hash

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVIterSet(PetscHMapIV ht, PetscHashIter iter, PetscScalar val)

  Input Parameters:
+ ht   - The hash table
. iter - The iterator
- val  - The value

  Level: developer

.seealso: `PetscHMapIVPut()`, `PetscHMapIVQuerySet()`, `PetscHMapIVSet()`,
`PetscHMapIVSetWithMode()`
M*/

/*MC
  PetscHMapIVIterDel - Remove the (key,value) referenced by an iterator from the hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVIterDel(PetscHMapIV ht, PetscHashIter iter)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Level: developer

.seealso: `PetscHMapIVFind()`, `PetscHMapIVQueryDel()`, `PetscHMapIVDel()`
M*/

/*MC
  PetscHMapIVGetKeys - Get all keys from a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGetKeys(PetscHMapIV ht, PetscInt *off, PetscInt array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table keys into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table keys

  Level: developer

.seealso: `PetscHSetIVGetSize()`, `PetscHMapIVGetVals()`
M*/

/*MC
  PetscHMapIVGetVals - Get all values from a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGetVals(PetscHMapIV ht, PetscInt *off, PetscScalar array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIVGetSize()`, `PetscHMapIVGetKeys()`
M*/

/*MC
  PetscHMapIVGetPairs - Get all (key,value) pairs from a hash table

  Synopsis:
  #include <petsc/private/hashmapiv.h>
  PetscErrorCode PetscHMapIVGetPairs(PetscHMapIV ht, PetscInt *off, PetscInt karray[], PetscScalar varray[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
. karray - Array where to put hash table keys into
- varray - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
. karray - Array filled with the hash table keys
- varray - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetIVGetSize()`, `PetscHMapIVGetKeys()`, `PetscHMapIVGetVals()`
M*/
This file is processed by make allmanpages in $PETSC_DIR/makefile to create manual pages
for the types and macros created by PETSC_HASH_MAP(). For example, PetscHMAPIJ.

/*S
  PetscHMapObj - Hash table map

  Synopsis:
  typedef khash_t(HMapObj) *PetscHMapObj;

  Level: developer

.seealso: `PETSC_HASH_MAP()`, `PetscHMapObjCreate()`
S*/
typedef struct _PetscHashObj PetscHMapObj;

/*MC
  PetscHMapObjCreate - Create a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjCreate(PetscHMapObj *ht)

  Output Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapObjCreateWithSize()`, `PetscHMapObjDestroy()`
M*/

/*MC
  PetscHMapObjCreateWithSize - Create a hash table with a given initial size

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjCreateWithSize(PetscInt n, PetscHMapObj *ht)

  Input Parameter:
. n - The size of the hash table

  Output Parameter:
. ht - The hash table

  Level: developer

  Note:
  `n` must be non-negative.

.seealso: `PetscHMapObjCreate()`, `PetscHMapObjDestroy()`
M*/

/*MC
  PetscHMapObjDestroy - Destroy a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjDestroy(PetscHMapObj *ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapObjCreate()`, `PetscHMapObjCreateWithSize()`
M*/

/*MC
  PetscHMapObjReset - Reset a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjReset(PetscHMapObj ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapObjClear()`
M*/

/*MC
  PetscHMapObjDuplicate - Duplicate a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjDuplicate(PetscHMapObj ht, PetscHMapObj *hd)

  Input Parameter:
. ht - The source hash table

  Output Parameter:
. ht - The duplicated hash table

  Level: developer

.seealso: `PetscHMapObjCreate()`
M*/

/*MC
  PetscHMapObjClear - Clear a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjClear(PetscHMapObj ht)

  Input Parameter:
. ht - The hash table

  Level: developer

.seealso: `PetscHMapObjReset()`
M*/

/*MC
  PetscHMapObjResize - Set the number of buckets in a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjResize(PetscHMapObj ht, PetscInt nb)

  Input Parameters:
+ ht - The hash table
- nb - The number of buckets

  Level: developer

.seealso: `PetscHMapObjCreate()`
M*/

/*MC
  PetscHMapObjGetSize - Get the number of entries in a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGetSize(PetscHMapObj ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The number of entries

  Level: developer

.seealso: `PetscHMapObjResize()`
M*/

/*MC
  PetscHMapObjGetCapacity - Get the current size of the array in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGetCapacity(PetscHMapObj ht, PetscInt *n)

  Input Parameter:
. ht - The hash table

  Output Parameter:
. n - The capacity

  Level: developer

.seealso: `PetscHMapObjResize()`, `PetscHMapObjGetSize()`
M*/

/*MC
  PetscHMapObjHas - Query for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjHas(PetscHMapObj ht, PetscInt64 key, PetscBool *has)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. has - Boolean indicating whether key is in the hash table

  Level: developer

.seealso: `PetscHMapObjGet()`, `PetscHMapObjGetWithDefault()`, `PetscHMapObjSet()`,
`PetscHMapObjSetWithMode()`, `PetscHMapObjFind()`
M*/

/*MC
  PetscHMapObjGet - Get the value for a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGet(PetscHMapObj ht, PetscInt64 key, PetscObject *val)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapObjSet()`, `PetscHMapObjSetWithMode()`, `PetscHMapObjIterGet()`,
`PetscHMapObjGetWithDefault()`
M*/

/*MC
  PetscHMapObjGetWithDefault - Get the value for a key in the hash table but override the default
  value returned if the key was not found

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGetWithDefault(PetscHMapObj ht, PetscInt64 key, PetscObject default_val, PetscObject *val)

  Input Parameters:
+ ht          - The hash table
. key         - The key
- default_val - The default value to set `val` to if `key` was not found

  Output Parameter:
. val - The value

  Level: developer

.seealso: `PetscHMapObjGet()`, `PetscHMapObjSet()`, `PetscHMapObjSetWithMode()`, `PetscHMapObjIterGet()`
M*/


/*MC
  PetscHMapObjSet - Set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjSet(PetscHMapObj ht, PetscInt64 key, PetscObject val)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Level: developer

.seealso: `PetscHMapObjGet()`, `PetscHMapObjSetWithMode()`, `PetscHMapObjGetWithDefault()`,
`PetscHMapObjIterSet()`
M*/

/*MC
  PetscHMapObjSetWithMode - Set a (key,value) entry in the hash table according to an `InsertMode`

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjSetWithMode(PetscHMapObj ht, PetscInt64 key, PetscObject val, InsertMode mode)

  Input Parameters:
+ ht   - The hash table
. key  - The key
. val  - The value
- mode - The insertion mode

  Level: developer

  Notes:
  `mode` may be any of the following\:
  - `INSERT_VALUES`\: this routine behaves identically to `PetscHMapObjSet()`.
  - `ADD_VALUES`\: if `key` is found `val` is added to the current entry, otherwise (`key`, `value`)
                   is inserted into `ht` as-if-by `INSERT_VALUES`.
  - `MAX_VALUES`\: if `key` is found the current value is replaced by the maximum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.
  - `MIN_VALUES`\: if `key` is found the current value is replaced by the minimum of `val` and the
                   current entry, otherwise (`key`, `value`) is inserted into `ht` as-if-by
                   `INSERT_VALUES`.

   All other `InsertMode`s will raise an error if used.

   Since this routine relies on `+`, `<`, and `>` being well-formed for a particular type
   it is not available by default for all PETSc hash table instantiations. If a particular
   instantiation supports this routine it must define `PETSC_HMAPObj_HAVE_EXTENDED_API` to
   `1`.

.seealso: `PetscHMapObjSet()`, `PetscHMapObjGet()`, `PetscHMapObjGetWithDefault()`,
`PetscHMapObjIterSet()`
M*/

/*MC
  PetscHMapObjDel - Remove a key and its value from the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjDel(PetscHMapObj ht,PetscInt64 key)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Level: developer

.seealso: `PetscHMapObjHas()`, `PetscHMapObjIterDel()`
M*/

/*MC
  PetscHMapObjQuerySet - Query and set a (key,value) entry in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjQuerySet(PetscHMapObj ht, PetscInt64 key, PetscObject val, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
. key - The key
- val - The value

  Output Parameter:
. missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapObjQueryDel()`, `PetscHMapObjSet()`, `PetscHMapObjSetWithMode()`
M*/

/*MC
  PetscHMapObjQueryDel - Query and remove a (key,value) entry from the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjQueryDel(PetscHMapObj ht, PetscInt64 key, PetscBool *present)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameter:
. present - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapObjQuerySet()`, `PetscHMapObjDel()`
M*/

/*MC
  PetscHMapObjFind - Query for key in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjFind(PetscHMapObj ht, PetscInt64 key, PetscHashIter *iter, PetscBool *found)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- found - Boolean indicating whether the key was present

  Level: developer

.seealso: `PetscHMapObjIterGet()`, `PetscHMapObjIterDel()`
M*/

/*MC
  PetscHMapObjPut - Set a key in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjPut(PetscHMapObj ht, PetscInt64 key, PetscHashIter *iter, PetscBool *missing)

  Input Parameters:
+ ht  - The hash table
- key - The key

  Output Parameters:
+ iter - Iterator referencing the value for key
- missing - Boolean indicating whether the key was missing

  Level: developer

.seealso: `PetscHMapObjIterSet()`, `PetscHMapObjQuerySet()`, `PetscHMapObjSet()`,
`PetscHMapObjSetWithMode()`
M*/

/*MC
  PetscHMapObjIterGet - Get the value referenced by an iterator in the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjIterGet(PetscHMapObj ht, PetscHashIter iter, PetscObject *val)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Output Parameter:
. val  - The value

  Level: developer

.seealso: `PetscHMapObjFind()`, `PetscHMapObjGet()`, `PetscHMapObjGetWithDefault()`
M*/

/*MC
  PetscHMapObjIterSet - Set the value referenced by an iterator in the hash

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjIterSet(PetscHMapObj ht, PetscHashIter iter, PetscObject val)

  Input Parameters:
+ ht   - The hash table
. iter - The iterator
- val  - The value

  Level: developer

.seealso: `PetscHMapObjPut()`, `PetscHMapObjQuerySet()`, `PetscHMapObjSet()`,
`PetscHMapObjSetWithMode()`
M*/

/*MC
  PetscHMapObjIterDel - Remove the (key,value) referenced by an iterator from the hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjIterDel(PetscHMapObj ht, PetscHashIter iter)

  Input Parameters:
+ ht   - The hash table
- iter - The iterator

  Level: developer

.seealso: `PetscHMapObjFind()`, `PetscHMapObjQueryDel()`, `PetscHMapObjDel()`
M*/

/*MC
  PetscHMapObjGetKeys - Get all keys from a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGetKeys(PetscHMapObj ht, PetscInt *off, PetscInt64 array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table keys into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table keys

  Level: developer

.seealso: `PetscHSetObjGetSize()`, `PetscHMapObjGetVals()`
M*/

/*MC
  PetscHMapObjGetVals - Get all values from a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGetVals(PetscHMapObj ht, PetscInt *off, PetscObject array[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
- array - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
- array - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetObjGetSize()`, `PetscHMapObjGetKeys()`
M*/

/*MC
  PetscHMapObjGetPairs - Get all (key,value) pairs from a hash table

  Synopsis:
  #include <petsc/private/hashmapobj.h>
  PetscErrorCode PetscHMapObjGetPairs(PetscHMapObj ht, PetscInt *off, PetscInt64 karray[], PetscObject varray[])

  Input Parameters:
+ ht    - The hash table
. off   - Input offset in array (usually zero)
. karray - Array where to put hash table keys into
- varray - Array where to put hash table values into

  Output Parameters:
+ off   - Output offset in array (output offset = input offset + hash table size)
. karray - Array filled with the hash table keys
- varray - Array filled with the hash table values

  Level: developer

.seealso: `PetscHSetObjGetSize()`, `PetscHMapObjGetKeys()`, `PetscHMapObjGetVals()`
M*/
