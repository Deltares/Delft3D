from conan import ConanFile
from conan.tools.cmake import CMakeDeps, CMakeToolchain


class PetscSolverReplayRecipe(ConanFile):
    name = "petsc-solver-replay"
    version = "0.1.0"
    settings = "os", "compiler", "build_type", "arch", "fortran_compiler"

    def requirements(self):
        self.requires("petsc/3.25.3")

    def generate(self):
        CMakeDeps(self).generate()
        CMakeToolchain(self).generate()

    def layout(self):
        self.folders.generators = "conan"
