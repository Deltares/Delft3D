#!/usr/bin/env python3
"""
Add dummy scalar fields to existing preCICE configuration for latency testing.
This script modifies the precice_config.xml from 08_dflowfm_sequential_dwaves
to include N dummy scalar fields exchanged between FM and Wave.

The script uses text manipulation to preserve the exact XML format including
pseudo-namespaces like data:scalar and mapping:nearest-neighbor.
"""

import os
import sys
import re

def add_dummy_scalars_to_config(input_file, output_file, num_scalars):
    """
    Add dummy scalar fields to an existing preCICE configuration.
    
    Args:
        input_file: Path to original precice_config.xml
        output_file: Path to output precice_config.xml (can be same as input)
        num_scalars: Number of dummy scalar fields to add
    """
    
    with open(input_file, 'r') as f:
        content = f.read()
    
    # Remove VTK export lines to reduce I/O overhead
    content = re.sub(r'\s*<export:vtu[^>]*/>.*\n?', '', content)
    
    # If num_scalars is 0, just copy the original file unchanged (but with VTK removed)
    if num_scalars == 0:
        with open(output_file, 'w') as f:
            f.write(content)
        print("\nNo dummy scalars requested (num_scalars=0)")
        print(f"Modified configuration written to: {output_file}")
        print("  - Removed VTK export declarations")
        return
    
    # Generate scalar names
    scalar_names = [f'fm_scalar_{i:07d}' for i in range(1, num_scalars + 1)]
    
    print(f"Adding {num_scalars} dummy scalar declarations...")
    
    # 1. Add data declarations after the last existing data declaration
    last_data_pattern = r'(<data:(scalar|vector)[^>]*/>)'
    last_data_match = None
    for match in re.finditer(last_data_pattern, content):
        last_data_match = match
    
    if last_data_match:
        insert_pos = last_data_match.end()
        scalar_declarations = '\n\n  <!-- Dummy scalars for latency testing -->'
        for name in scalar_names:
            scalar_declarations += f'\n  <data:scalar name="{name}" />'
        content = content[:insert_pos] + scalar_declarations + content[insert_pos:]
    
    # 2. Add or update fm_dummy_mesh
    fm_mesh_pattern = r'<mesh name="fm_dummy_mesh"[^>]*>.*?</mesh>'
    if re.search(fm_mesh_pattern, content, re.DOTALL):
        # Remove existing fm_dummy_mesh
        content = re.sub(fm_mesh_pattern, '', content, flags=re.DOTALL)
    
    # Find where to insert fm_dummy_mesh (after fm_flow_nodes mesh)
    fm_flow_mesh_pattern = r'(</mesh>)(\s*)(<!-- .*? mesh -->|\s*<mesh name="swan_)'
    fm_flow_match = re.search(r'<mesh name="fm_flow_nodes".*?</mesh>', content, re.DOTALL)
    
    if fm_flow_match:
        insert_pos = fm_flow_match.end()
        fm_dummy_mesh = '\n\n  <!-- FM dummy mesh for latency testing -->'
        fm_dummy_mesh += '\n  <mesh name="fm_dummy_mesh" dimensions="2">'
        for name in scalar_names:
            fm_dummy_mesh += f'\n    <use-data name="{name}" />'
        fm_dummy_mesh += '\n  </mesh>'
        content = content[:insert_pos] + fm_dummy_mesh + content[insert_pos:]
    
    # 3. Add or update wave_dummy_mesh
    wave_mesh_pattern = r'<mesh name="wave_dummy_mesh"[^>]*>.*?</mesh>'
    if re.search(wave_mesh_pattern, content, re.DOTALL):
        # Remove existing wave_dummy_mesh
        content = re.sub(wave_mesh_pattern, '', content, flags=re.DOTALL)
    
    # Find where to insert wave_dummy_mesh (after swan_mesh or fm_dummy_mesh)
    swan_mesh_match = re.search(r'<mesh name="swan_mesh".*?</mesh>', content, re.DOTALL)
    if not swan_mesh_match:
        # Try fm_dummy_mesh if swan_mesh not found
        swan_mesh_match = re.search(r'<mesh name="fm_dummy_mesh".*?</mesh>', content, re.DOTALL)
    
    if swan_mesh_match:
        insert_pos = swan_mesh_match.end()
        wave_dummy_mesh = '\n\n  <!-- Wave dummy mesh for latency testing -->'
        wave_dummy_mesh += '\n  <mesh name="wave_dummy_mesh" dimensions="2">'
        for name in scalar_names:
            wave_dummy_mesh += f'\n    <use-data name="{name}" />'
        wave_dummy_mesh += '\n  </mesh>'
        content = content[:insert_pos] + wave_dummy_mesh + content[insert_pos:]
    
    # 4. Update FM participant
    # Remove existing dummy scalar write-data from FM
    fm_participant_pattern = r'(<participant name="fm">)(.*?)(</participant>)'
    fm_match = re.search(fm_participant_pattern, content, re.DOTALL)
    
    if fm_match:
        fm_content = fm_match.group(2)
        # Remove existing dummy scalars
        fm_content = re.sub(r'\s*<provide-mesh name="fm_dummy_mesh"\s*/>', '', fm_content)
        fm_content = re.sub(r'\s*<write-data name="fm_scalar_\d+"\s+mesh="fm_dummy_mesh"\s*/>', '', fm_content)
        
        # Add provide-mesh after other provide-mesh declarations
        provide_pattern = r'(<provide-mesh[^>]*/>)'
        provide_matches = list(re.finditer(provide_pattern, fm_content))
        if provide_matches:
            insert_pos = provide_matches[-1].end()
            provide_dummy = '\n    <provide-mesh name="fm_dummy_mesh" />'
            fm_content = fm_content[:insert_pos] + provide_dummy + fm_content[insert_pos:]
        
        # Add write-data declarations at the end (before closing tag)
        write_data_additions = '\n\n    <!-- Dummy scalar writes for latency testing -->'
        for name in scalar_names:
            write_data_additions += f'\n    <write-data name="{name}" mesh="fm_dummy_mesh" />'
        
        # Insert before last line of participant content
        fm_content = fm_content.rstrip() + write_data_additions + '\n  '
        
        # Replace the participant content
        content = content[:fm_match.start(2)] + fm_content + content[fm_match.end(2):]
    
    # 5. Update Wave participant
    wave_participant_pattern = r'(<participant name="wave">)(.*?)(</participant>)'
    wave_match = re.search(wave_participant_pattern, content, re.DOTALL)
    
    if wave_match:
        wave_content = wave_match.group(2)
        # Remove existing dummy scalars
        wave_content = re.sub(r'\s*<provide-mesh name="wave_dummy_mesh"\s*/>', '', wave_content)
        wave_content = re.sub(r'\s*<receive-mesh name="fm_dummy_mesh"\s+from="fm"\s*/>', '', wave_content)
        wave_content = re.sub(r'\s*<read-data name="fm_scalar_\d+"\s+mesh="fm_dummy_mesh"\s*/>', '', wave_content)
        wave_content = re.sub(r'\s*<mapping:nearest-neighbor[^>]*from="fm_dummy_mesh"[^>]*to="wave_dummy_mesh"[^>]*/>', '', wave_content)
        
        # Add provide-mesh for wave_dummy_mesh after the first provide-mesh
        provide_pattern = r'(<provide-mesh[^>]*/>)'
        provide_matches = list(re.finditer(provide_pattern, wave_content))
        if provide_matches:
            insert_pos = provide_matches[0].end()
            provide_dummy = '\n    <provide-mesh name="wave_dummy_mesh" />'
            wave_content = wave_content[:insert_pos] + provide_dummy + wave_content[insert_pos:]
        
        # Add receive-mesh after other receive-mesh declarations
        receive_pattern = r'(<receive-mesh[^>]*/>)'
        receive_matches = list(re.finditer(receive_pattern, wave_content))
        if receive_matches:
            insert_pos = receive_matches[-1].end()
            receive_dummy = '\n    <receive-mesh name="fm_dummy_mesh" from="fm" />'
            wave_content = wave_content[:insert_pos] + receive_dummy + wave_content[insert_pos:]
        
        # Add read-data and mapping at the end
        read_data_additions = '\n\n    <!-- Dummy scalar reads for latency testing -->'
        for name in scalar_names:
            read_data_additions += f'\n    <read-data name="{name}" mesh="wave_dummy_mesh" />'
        
        read_data_additions += '\n\n    <!-- Mapping for dummy mesh -->'
        read_data_additions += '\n    <mapping:nearest-neighbor'
        read_data_additions += '\n      direction="read"'
        read_data_additions += '\n      from="fm_dummy_mesh"'
        read_data_additions += '\n      to="wave_dummy_mesh"'
        read_data_additions += '\n      constraint="consistent" />'
        
        # Insert before last line of participant content
        wave_content = wave_content.rstrip() + read_data_additions + '\n  '
        
        # Replace the participant content
        content = content[:wave_match.start(2)] + wave_content + content[wave_match.end(2):]
    
    # 6. Add exchanges to coupling scheme
    # Find coupling scheme
    coupling_pattern = r'(<coupling-scheme:(serial|parallel)-explicit>)(.*?)(</coupling-scheme:\2-explicit>)'
    coupling_match = re.search(coupling_pattern, content, re.DOTALL)
    
    if coupling_match:
        coupling_content = coupling_match.group(3)
        # Remove existing dummy scalar exchanges
        coupling_content = re.sub(r'\s*<exchange\s+data="fm_scalar_\d+"\s+mesh="fm_dummy_mesh"\s+from="fm"\s+to="wave"\s*/>', '', coupling_content)
        
        # Add new exchanges at the end
        exchange_additions = '\n\n    <!-- Dummy scalar exchanges for latency testing -->'
        for name in scalar_names:
            exchange_additions += f'\n    <exchange data="{name}" mesh="fm_dummy_mesh" from="fm" to="wave" />'
        
        # Insert before closing tag
        coupling_content = coupling_content.rstrip() + exchange_additions + '\n  '
        
        # Replace the coupling scheme content
        content = content[:coupling_match.start(3)] + coupling_content + content[coupling_match.end(3):]
    
    # Write the modified XML
    with open(output_file, 'w') as f:
        f.write(content)
    
    print(f"\nModified preCICE configuration written to: {output_file}")
    print(f"  - Removed VTK export declarations (reduces I/O overhead)")
    print(f"  - Added {num_scalars} dummy scalar declarations")
    print(f"  - Added/updated fm_dummy_mesh and wave_dummy_mesh")
    print(f"  - Added {num_scalars} write-data declarations to FM")
    print(f"  - Added {num_scalars} read-data declarations to Wave")
    print(f"  - Added {num_scalars} exchanges to coupling scheme")

def set_environment_variable(num_scalars):
    """Set the PRECICE_NUM_DUMMY_SCALARS environment variable."""
    env_file = os.path.expanduser('~/.precice_env')
    
    # Write to a file that can be sourced
    with open(env_file, 'w') as f:
        f.write(f'export PRECICE_NUM_DUMMY_SCALARS={num_scalars}\n')
    
    # Also set in current process
    os.environ['PRECICE_NUM_DUMMY_SCALARS'] = str(num_scalars)
    
    print("\nEnvironment variable set:")
    print(f"  PRECICE_NUM_DUMMY_SCALARS={num_scalars}")
    print("\nTo use in your shell, run:")
    print(f"  source {env_file}")
    print("Or manually:")
    print(f"  export PRECICE_NUM_DUMMY_SCALARS={num_scalars}")

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Add dummy scalar fields to preCICE configuration for latency testing',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Add 100 dummy scalars to config (default)
  python3 add_dummy_scalars.py
  
  # Add 500 dummy scalars
  python3 add_dummy_scalars.py 500
  
  # Use different input/output files
  python3 add_dummy_scalars.py 100 --input ../precice_config.xml --output precice_config_with_scalars.xml
  
  # Set environment variable without modifying config
  python3 add_dummy_scalars.py 100 --set-env-only
        """
    )
    
    parser.add_argument('num_scalars', type=int, nargs='?', default=100,
                        help='Number of dummy scalars to add (default: 100, range: 0-1000000)')
    parser.add_argument('--input', '-i', default='precice_config_template.xml',
                        help='Input preCICE configuration file')
    parser.add_argument('--output', '-o', default='precice_config.xml',
                        help='Output preCICE configuration file')
    parser.add_argument('--set-env-only', action='store_true',
                        help='Only set environment variable, do not modify config')
    
    args = parser.parse_args()
    
    # Validate scalar count
    if args.num_scalars < 0:
        print(f"Error: num_scalars must be non-negative, got {args.num_scalars}")
        sys.exit(1)
    elif args.num_scalars > 1000000:
        print(f"Warning: num_scalars is very large ({args.num_scalars}), capping at 1000000")
        args.num_scalars = 1000000
    
    # Set environment variable
    set_environment_variable(args.num_scalars)
    
    # Modify config unless --set-env-only
    if not args.set_env_only:
        if not os.path.exists(args.input):
            print(f"Error: Input file not found: {args.input}")
            sys.exit(1)
        
        add_dummy_scalars_to_config(args.input, args.output, args.num_scalars)
        
        print(f"\n{'='*60}")
        print("Next steps:")
        print(f"{'='*60}")
        print("1. Source the environment variable:")
        print("   source ~/.precice_env")
        print("2. Run FM and Wave with the modified configuration")
        print(f"   Both will automatically use {args.num_scalars} dummy scalars")
