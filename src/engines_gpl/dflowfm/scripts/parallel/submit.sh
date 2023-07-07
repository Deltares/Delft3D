#!/bin/bash

if [ "${DOLISA}" = "1" ]; then
	RUNSCRIPT=run-lisa.sh
	queue=cores8
	MAXTIME=10:00:00
elif [ "${DOGORDON}" = "1" ]; then
	RUNSCRIPT=run-gordon.sh
	queue=normal
	MAXTIME=03:00:00
else
	RUNSCRIPT=$(which run.sh)
#	queue=normal
	queue=normal-i7
fi


if [ -z "$1" ]; then
	echo "Error: MDU-file and/or number of nodes missing on commandline."
	echo "Usage: submit.sh <MDU-file> <nr of nodes> [nr of procs/node] [jobname] [maxtime]"
	exit 1
else
        MDUfile=$1
	numnode=$2
fi

if [ -z "$3" ]; then
	numcore=4
	echo "Nr. of cores/node missing on commandline: defaulting to: $numcore"
else
	numcore=$3
fi

if [ -z "$4" ]; then
	JOBNAME="dflowfm_${numnode}x${numcore}"
else
	JOBNAME="$4_${numnode}x${numcore}"
fi

if [ -n "$5" ]; then
	MAXTIME=$5
fi


np=$((${numnode}*${numcore}))

if [ "${DOLISA}" = "1" ]; then
	if [ -e ${RUNSCRIPT} ]; then
		rm ${RUNSCRIPT}
	fi
	echo "#PBS -lwalltime=${MAXTIME}" > ${RUNSCRIPT}
	echo "#PBS -lnodes=${numnode}:${queue}:infiniband:ppn=${numcore}" >> ${RUNSCRIPT}
	echo "module load ${MODULE_MPI}" >> ${RUNSCRIPT}
	echo "module load openmpi/intel" >> ${RUNSCRIPT}
        echo "module load fortran/intel/64" >> ${RUNSCRIPT}
#        echo "module load paffinity"        >> ${RUNSCRIPT}
	echo "ulimit -s unlimited"          >> ${RUNSCRIPT}
        echo 'cd $PBS_O_WORKDIR' >> ${RUNSCRIPT}

	echo "mpiexec dflowfm -autostartstop ${MDUfile}  > out.txt 2> err.txt"  >> ${RUNSCRIPT}
elif [ "${DOGORDON}" = "1" ]; then
	if [ -e ${RUNSCRIPT} ]; then
		rm ${RUNSCRIPT}
	fi
	echo "#!/bin/bash" > ${RUNSCRIPT}
	echo "#PBS -V" >> ${RUNSCRIPT}
	echo "#PBS -q normal" >> ${RUNSCRIPT}
	echo "#PBS -A und105" >> ${RUNSCRIPT}
	echo "#PBS -l nodes=$numnode:ppn=$numcore:native" >> ${RUNSCRIPT}
	echo "#PBS -l walltime=$MAXTIME" >> ${RUNSCRIPT}
	echo "#PBS -N $JOBNAME" >> ${RUNSCRIPT}
	echo "#PBS -M arthur.vandam@deltares.nl" >> ${RUNSCRIPT}
	echo "#PBS -m abe" >> ${RUNSCRIPT}

	echo ". /etc/profile.d/modules.sh" >> ${RUNSCRIPT}
	echo "module load intel" >> ${RUNSCRIPT}
	echo "module load mvapich2_ib/1.8a1p1" >> ${RUNSCRIPT}
	echo "#module load petsc/3.2.p3" >> ${RUNSCRIPT}

	echo "export NETCDFROOT=/oasis/scratch/hellyj/netcdf-4.2" >> ${RUNSCRIPT}
	echo "export DFLOWFMROOT=\$PKGROOT/dflowfm" >> ${RUNSCRIPT}
	echo "export PETSCROOT=\$PKGROOT/petsc/3.4.0_mvapich2_intel" >> ${RUNSCRIPT}

	echo "export LD_LIBRARY_PATH=\$NETCDFROOT/lib:\$LD_LIBRARY_PATH" >> ${RUNSCRIPT}
	echo "export LD_LIBRARY_PATH=\$PETSCROOT/lib:\$LD_LIBRARY_PATH" >> ${RUNSCRIPT}
	rundir=`pwd -P`
	echo "cd $rundir" >> ${RUNSCRIPT}

	echo "date > clock.txt" >> ${RUNSCRIPT}
	echo "mpirun_rsh -np $np -hostfile \$PBS_NODEFILE \$DFLOWFMROOT/bin/dflowfm --autostartstop $MDUfile" >> ${RUNSCRIPT}
	echo "date >> clock.txt" >> ${RUNSCRIPT}
fi

#casename=${numnode}x${numcore}

#mkdir $casename
#cd $casename
#cp ../invoer .
#cp ../${RUNSCRIPT} .


if [ "${DOLISA}" = "1" ]; then
	qsub -N ${JOBNAME} ${RUNSCRIPT}
elif [ "${DOGORDON}" = "1" ]; then
	echo You may first inspect the generated runscript: ${RUNSCRIPT}
	echo Then submit your job using:
	echo qsub ${RUNSCRIPT}
else
	echo "qsub -q $queue -pe distrib ${numnode} -N ${JOBNAME} ${RUNSCRIPT} ${MDUfile} ${numcore}"
	qsub -q $queue -pe distrib ${numnode} -N ${JOBNAME} ${RUNSCRIPT} ${MDUfile} ${numcore}
fi
#cd ..

