#!/usr/bin/env python3
"""
Generate preCICE configuration XML for dummy scalar latency testing.
This creates a configuration file with N scalar fields exchanged between FM and Wave.
"""

def generate_precice_config(num_scalars=100, output_file="precice-config.xml"):
    """
    Generate a preCICE configuration file for scalar latency testing.
    
    Args:
        num_scalars: Number of scalar fields to exchange (default: 100)
        output_file: Output XML filename (default: precice-config.xml)
    """
    
    with open(output_file, 'w') as f:
        # Header
        f.write('<?xml version="1.0" encoding="UTF-8" ?>\n')
        f.write('<precice-configuration>\n')
        f.write('  <log>\n')
        f.write('    <sink\n')
        f.write('      filter="%Severity% > debug"\n')
        f.write('      format="---[precice] %ColorizedSeverity% %Message%"\n')
        f.write('      enabled="true" />\n')
        f.write('  </log>\n\n')
        
        # Data declarations
        f.write('  <!-- Scalar data field declarations -->\n')
        for i in range(1, num_scalars + 1):
            scalar_name = f'fm_scalar_{i:07d}'
            f.write(f'  <data:scalar name="{scalar_name}" />\n')
        f.write('\n')
        
        # FM dummy mesh
        f.write('  <!-- FM dummy mesh with single point at (0,0) -->\n')
        f.write('  <mesh name="fm_dummy_mesh" dimensions="2">\n')
        for i in range(1, num_scalars + 1):
            scalar_name = f'fm_scalar_{i:07d}'
            f.write(f'    <use-data name="{scalar_name}" />\n')
        f.write('  </mesh>\n\n')
        
        # Wave dummy mesh
        f.write('  <!-- Wave dummy mesh with single point at (0,0) -->\n')
        f.write('  <mesh name="wave_dummy_mesh" dimensions="2">\n')
        for i in range(1, num_scalars + 1):
            scalar_name = f'fm_scalar_{i:07d}'
            f.write(f'    <use-data name="{scalar_name}" />\n')
        f.write('  </mesh>\n\n')
        
        # FM participant
        f.write('  <!-- FM participant (data provider) -->\n')
        f.write('  <participant name="fm">\n')
        f.write('    <provide-mesh name="fm_dummy_mesh" />\n')
        for i in range(1, num_scalars + 1):
            scalar_name = f'fm_scalar_{i:07d}'
            f.write(f'    <write-data name="{scalar_name}" mesh="fm_dummy_mesh" />\n')
        f.write('  </participant>\n\n')
        
        # Wave participant
        f.write('  <!-- Wave participant (data receiver) -->\n')
        f.write('  <participant name="wave">\n')
        f.write('    <receive-mesh name="fm_dummy_mesh" from="fm" />\n')
        for i in range(1, num_scalars + 1):
            scalar_name = f'fm_scalar_{i:07d}'
            f.write(f'    <read-data name="{scalar_name}" mesh="fm_dummy_mesh" />\n')
        f.write('\n')
        f.write('    <!-- Mapping from FM mesh to Wave mesh -->\n')
        f.write('    <mapping:nearest-neighbor\n')
        f.write('      direction="read"\n')
        f.write('      from="fm_dummy_mesh"\n')
        f.write('      to="wave_dummy_mesh"\n')
        f.write('      constraint="consistent" />\n')
        f.write('  </participant>\n\n')
        
        # Communication method
        f.write('  <!-- Communication method: TCP sockets -->\n')
        f.write('  <m2n:sockets acceptor="fm" connector="wave" exchange-directory=".." />\n\n')
        
        # Coupling scheme
        f.write('  <!-- Serial explicit coupling scheme -->\n')
        f.write('  <coupling-scheme:serial-explicit>\n')
        f.write('    <time-window-size value="1.0" />\n')
        f.write('    <max-time value="100.0" />\n')
        f.write('    <participants first="fm" second="wave" />\n')
        for i in range(1, num_scalars + 1):
            scalar_name = f'fm_scalar_{i:07d}'
            f.write(f'    <exchange data="{scalar_name}" mesh="fm_dummy_mesh" from="fm" to="wave" />\n')
        f.write('  </coupling-scheme:serial-explicit>\n')
        
        # Footer
        f.write('</precice-configuration>\n')
    
    print(f"Generated preCICE configuration with {num_scalars} scalars: {output_file}")
    print(f"Configuration summary:")
    print(f"  - FM provides fm_dummy_mesh and writes {num_scalars} scalars")
    print(f"  - Wave receives fm_dummy_mesh and reads {num_scalars} scalars")
    print(f"  - Using nearest-neighbor mapping (single point, so exact match)")
    print(f"  - TCP sockets communication")
    print(f"  - Serial explicit coupling")

if __name__ == "__main__":
    import sys
    
    # Get number of scalars from command line, default to 100
    num_scalars = 100
    if len(sys.argv) > 1:
        try:
            num_scalars = int(sys.argv[1])
            print(f"Generating configuration for {num_scalars} scalars...")
        except ValueError:
            print(f"Invalid number of scalars: {sys.argv[1]}, using default: 100")
    
    generate_precice_config(num_scalars)
