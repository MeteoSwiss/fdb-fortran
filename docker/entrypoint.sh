#!/bin/bash

set -eux

cat > /scratch/fdb_config.yaml <<EOF
---
type: local
engine: toc
schema: /scratch/fdb_schema
spaces:
- handler: Default
  roots:
  - path: /fdb_data
EOF

export FDB5_CONFIG_FILE=/scratch/fdb_config.yaml

. $SPACK_ROOT/share/spack/setup-env.sh
spack env activate spack-env

export PATH=$PATH:`spack location -i fdb-fortran`
export PATH=$PATH:`spack location -i fdb`/bin

fdb-info --all

for f in /fdb_data_files/*;
    do 
    echo "Archiving $f" ; 
    fdbf-write --keys=generatingProcessIdentifier,productionStatusOfProcessedData,discipline,parameterCategory,dataDate,dataTime,endStep,productDefinitionTemplateNumber,typeOfFirstFixedSurface,level,parameterNumber $f
done
