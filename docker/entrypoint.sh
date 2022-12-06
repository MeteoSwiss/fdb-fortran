#!/bin/bash

set -eux

. $SPACK_ROOT/share/spack/setup-env.sh
spack env activate /scratch/spack-env/

export PATH=$PATH:`spack location -i fdb-fortran`
export PATH=$PATH:`spack location -i fdb`/bin
export FDB5_CONFIG_FILE=/scratch/fdb_config.yaml

fdb-info --all

for f in /fdb_data_files/*;
    do 
    echo "Archiving $f" ; 
    fdbf-write --keys=generatingProcessIdentifier,productionStatusOfProcessedData,discipline,parameterCategory,dataDate,dataTime,endStep,productDefinitionTemplateNumber,typeOfFirstFixedSurface,level,parameterNumber $f
done
