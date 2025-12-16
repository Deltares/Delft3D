#!/usr/bin/env python3
"""
Add dummy scalar fields to preCICE configuration template for testing.
Works with precice_config_template.xml which has comment markers for insertion points.

Uses direct mesh access: FM provides fm_dummy_mesh, Wave receives it (no mapping).
"""

import os
import sys
import re

def add_dummy_scalars_to_config(input_file, output_file, num_scalars):
    """
    Add dummy scalar fields to preCICE configuration template.
    Template must have comment markers at insertion points.
    
    Args:
        input_file: Path to precice_config_template.xml
        output_file: Path to output precice_config.xml
        num_scalars: Number of dummy scalar fields to add
    """
    
    with open(input_file, 'r') as f:
        content = f.read()
    
    # Remove VTK export lines to reduce I/O overhead
    content = re.sub(r'\s*<export:vtu[^>]*/>.*\n?', '', content)
    
    if num_scalars == 0:
        with open(output_file, 'w') as f:
            f.write(content)
        print(f"\nGenerated {output_file} with 0 dummy scalars (template unchanged)")
        print("  - Removed VTK export declarations")
        return
    
    # Generate scalar names
    scalar_names = [f'fm_scalar_{i:07d}' for i in range(1, num_scalars + 1)]
    
    print(f"Generating configuration with {num_scalars} dummy scalars...")
    
    # 1. Add data declarations after comment marker
    marker = '<!-- Dummy scalars for testing (added by add_dummy_scalars.py) -->'
    if marker not in content:
        print(f"ERROR: Could not find data declarations marker in template")
        return
    
    scalar_decls = marker
    for name in scalar_names:
        scalar_decls += f'\n  <data:scalar name="{name}" />'
    content = content.replace(marker, scalar_decls)
    
    # 2. Add use-data to fm_dummy_mesh
    marker = '<!-- Dummy scalar use-data entries added by add_dummy_scalars.py -->'
    if marker not in content:
        print(f"ERROR: Could not find fm_dummy_mesh marker in template")
        return
    
    use_data = marker
    for name in scalar_names:
        use_data += f'\n    <use-data name="{name}" />'
    content = content.replace(marker, use_data)
    
    # 3. Add write-data to FM participant
    marker = '<!-- Dummy scalar write-data entries added by add_dummy_scalars.py -->'
    if marker not in content:
        print(f"ERROR: Could not find FM write-data marker in template")
        return
    
    write_data = marker
    for name in scalar_names:
        write_data += f'\n    <write-data name="{name}" mesh="fm_dummy_mesh" />'
    content = content.replace(marker, write_data)
    
    # 4. Add read-data to Wave participant
    marker = '<!-- Dummy scalar read-data entries added by add_dummy_scalars.py -->'
    if marker not in content:
        print(f"ERROR: Could not find Wave read-data marker in template")
        return
    
    read_data = marker
    for name in scalar_names:
        read_data += f'\n    <read-data name="{name}" mesh="fm_dummy_mesh" />'
    content = content.replace(marker, read_data)
    
    # 5. Add exchanges to coupling scheme
    marker = '<!-- Dummy scalar exchanges added by add_dummy_scalars.py -->'
    if marker not in content:
        print(f"ERROR: Could not find coupling scheme marker in template")
        return
    
    exchanges = marker
    for name in scalar_names:
        exchanges += f'\n    <exchange data="{name}" mesh="fm_dummy_mesh" from="fm" to="wave" />'
    content = content.replace(marker, exchanges)
    
    # Write output
    with open(output_file, 'w') as f:
        f.write(content)
    
    print(f"\n{'='*60}")
    print(f"Successfully generated {output_file}")
    print(f"{'='*60}")
    print(f"Configuration: {num_scalars} dummy scalars")
    print(f"  ✓ Data declarations: {num_scalars} scalars")
    print(f"  ✓ FM dummy mesh: {num_scalars} fields")
    print(f"  ✓ FM participant: provide-mesh + {num_scalars} write-data")
    print(f"  ✓ Wave participant: receive-mesh + {num_scalars} read-data")
    print(f"  ✓ Coupling scheme: {num_scalars} exchanges")
    print(f"  ✓ Direct mesh access: NO mapping (Wave receives fm_dummy_mesh)")
    print(f"  ✓ VTK exports: REMOVED (reduced I/O overhead)")


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
