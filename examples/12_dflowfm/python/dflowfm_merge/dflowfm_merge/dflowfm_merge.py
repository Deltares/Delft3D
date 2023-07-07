import glob
import merger

if __name__ == '__main__':
    import argparse
    import glob

    parser = argparse.ArgumentParser(description = 'Merge multiple NetCDF files generated during parallel run of D-FLOW FM into a single NetCDF file.')

    parser.add_argument('--input', nargs = '?', help = 'input files to merge: model_output_*.nc', required = True)
    parser.add_argument('--output', nargs = '?', help = 'output file name: model_output_merged.nc', required = True)

    args = parser.parse_args()

    files = glob.glob(args.input)

    m = merger.merger(files, args.output)
    m.run()
