from setuptools import setup, find_packages

setup(
    name='dflowfm',
    description='Python Wrapper for DFLOW-FM Hydrodynamic Model',
    version='0.1dev',
    packages=['dflowfm'],
    author='Gennadii Donchyts',
    author_email='gennadii.donchyts@deltares.nl',
    install_requires=['bmi'],
    license='GPL',
    long_description=open('README.txt').read(),
)
