from conan import ConanFile
from conan.tools.cmake import cmake_layout


class Delft3DRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"

    def requirements(self):
      self.requires("netcdf-fortran/4.6.2")
   
    def layout(self):
        cmake_layout(self)
        
    #def configure(self):
      #self.options['example'].example =False
    