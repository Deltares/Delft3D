#ifndef SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_PRECICE_LIB_HPP
#define SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_PRECICE_LIB_HPP

#include <print>
#include <string>

namespace csumo_precice
{
	/** 
	* @brief Run the preCICE solver dummy coupling
	* 
	* @param configFileName Path to the preCICE configuration file
	* @param solverName Name of the solver participant
	* @return int Return 0 on success, non-zero on failure
	*/
	int csumo_precice(const std::string& configFileName, const std::string& solverName);
	
	/** 
	* @brief Basic function for testing (legacy)
	* 
	* @return int Return 0 on success
	*/
	int csumo_precice();
}

#endif // SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_PRECICE_LIB_HPP
