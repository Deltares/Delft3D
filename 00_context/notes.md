The objective is to implement a solver that allows proper bank erosion modelling. To this end, first we need a solver that captures velocity near the banks correctly. The solver must be implemented in Delft3D Flexible Mesh (FM). We are in this repository. 

The current solution I am considering is an Immersed Boundary Method. The paper `Canestrelli16.md`> explains an implementation of such a method in a branch of Delft3D 4 (not accessible). Delft3D 4 is similar to Delft3D FM (e.g., staggered scheme), but it has the fundamental difference of using a structured grid. 

I am considering that the method to be applied only makes sense to apply it in Cartesian grids. I think that, for now, it should be restricted to Cartesian grids. Nevertheless, in a first proof of concepth, we will use the existing general unstructured solver of Delft3D FM. In the future we may implement a specific Cartesian solver. 

The implementation must have testing included. One test is the one shown in `Canestrelli16.md`. See where tests are located in `tests/`.

The objective now is to create a good prompt to request for a plan for implementing an Immersed Boundary Method in Delft3D Flexible Mesh. What does it need to be included in that promp to improve the creation of a plan? 

Write this prompt in a new markdown file.