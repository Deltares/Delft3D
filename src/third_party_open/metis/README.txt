These are METIS libraries for Windows.
METIS is a set of serial programs for partitioning graphs, partitioning
finite element meshes, and producing fill reducing orderings for sparse
matrices.

Official METIS information:
"A Fast and Highly Quality Multilevel Scheme for Partitioning Irregular
Graphs". George Karypis and Vipin Kumar. SIAM Journal on Scientific
Computing, Vol. 20, No. 1, pp. 359--392, 1999.

VERSION:
5.1.0, official source release from:
http://glaros.dtc.umn.edu/gkhome/metis/metis/download

Setup for solution:
1. Unpack metis-5.1.0.tar.gz
2. If not installed yet, install latest CMake
3. Generate Visual Studio solution with vsgen.bat (in directory metis-5.1.0)
4. Adapt metis.vcxproj into your solution.
5. Remove CMAKE stuff from this project
6. Configure Platforms and Configurations

It is a little bit of a hassle, but metis does not change to often anymore.
From now one no problems with newer IDE's anymore.