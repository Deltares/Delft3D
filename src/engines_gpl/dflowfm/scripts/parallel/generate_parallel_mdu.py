import sys
import os
import argparse
from mdu import *

arguments = argparse.ArgumentParser(description='Generate subdomain MDU files based on the main MDU file and subdomain polygons POL file.')
arguments.add_argument('--mdu', nargs='?', help='input MDI file', required=True)
arguments.add_argument('--count', nargs='?', help='number of subdomains', required=True)
arguments.add_argument('--domains', nargs='?', help='partitioning (subdomains) POL file', required=True)
arguments.add_argument('--icgsolver', nargs='?', help='Icgsolver, 6 = PETSc, 7 = GS', required=True)
args = arguments.parse_args()

cfg = MduParserKeepComments()
cfg.read(args.mdu)

mdubase = os.path.splitext(args.mdu)[0]
netbase = cfg.get('geometry', 'NetFile').rsplit("_net.nc")[0]

for i in range(int(args.count)):
    n = "{:04d}".format(i)

    with open(mdubase + '_' + n + '.mdu', 'w') as f:
        cfg.set('geometry', 'NetFile', netbase + '_' + n + '_net.nc')
        cfg.set('geometry', 'PartitionFile', args.domains)
        cfg.set('output', 'SnapshotDir', 'snapshots_' + n)
        cfg.set('output', 'RestartFile', mdubase + '_' + n + '_rst.nc')
        cfg.set('numerics', 'Icgsolver', args.icgsolver)
        cfg.write(f)
