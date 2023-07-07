from setuptools import setup, find_packages

setup(
    name='dflowfm-merge',
    description='Tool used to merge DFLOW-FM output NetCDF files',
    version='0.1dev',
    packages=['dflowfm_merge'],
    author='Gennadii Donchyts',
    author_email='gennadii.donchyts@deltares.nl',
    requires=['netCDF4'],
    license='LGPL',
    long_description=open('README.txt').read(),
)
