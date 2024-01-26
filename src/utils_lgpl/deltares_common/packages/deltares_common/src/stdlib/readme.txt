This is a small subset of functions taken from the Fortran stdlib project.
See the Fortran stdlib project: https://github.com/fortran-lang/stdlib

---HOW TO CHOOSE SORTING ALGORITHM---
The library offers four different sorting algorithms.
The fastest is usually 'radix_sort', which is only available for certain data types.
In general, use 'sort' for randomly ordered data and
'ord_sort' if you know that the data is partially sorted already.
Finally, 'sort_index' is needed when you do not only need to sort the array,
but also need the permutation of the indices that sorts the array
(algorithmically based on on ord_sort, so also efficient for partially sorted data).

---HOW LIBRARY WAS RETRIEVED---
(2023-11-22) We have decided to only take the sorting algorithms from stdlib, since:
* There is no stable release yet (version 0.3.0)
* The library is currently not tested for the platform/compiler combinations that we use
* The library cannot easily build with VSBuild + ifort + MSVC (C/C++), because it depends on the advanced dependency resolution of Ninja
* The library requires the fypp preprocessor, which would add an additional requirement for our builds

To still take advantage of the sorting algorithms, which have been ported from Rust to Fortran,
we have decided to strip down this part of the library and include it:
* Based on commit b8fbb3ce1aaf2fd1729897739fcdae8f6b3c4ef4
* Build the library on windows with Ninja (without the tests)
* Copy the preprocessed files from:
    - <repository_folder>/src/stdlib_sorting_radix_sort.f90
    - <build_folder>/src/stdlib_kinds.f90
    - <build_folder>/src/stdlib_optval.f90
    - <build_folder>/src/stdlib_sorting_sort.f90
    - <build_folder>/src/stdlib_sorting_sort_index.f90
    - <build_folder>/src/stdlib_sorting_ord_sort.f90
    - <build_folder>/src/stdlib_sorting.f90
* Remove all references to the stdlib bitset and string types from these sources to reduce dependencies
* Remove all references to the stdlib xp and qp floating point types, since they are not portable
* Remove the stdlib specific references to documentation
* Change int_size (stdlib_sorting.f90) from int64 to int32, such that the interface of sort_index is similar the old sort algorithms

---HOW TO UPDATE LIBRARY---
Retrieve the latest Fortran stdlib sources from https://github.com/fortran-lang/stdlib,
and follow the steps above.
Optionally, more parts of the stdlib project can be added if there is a need.
