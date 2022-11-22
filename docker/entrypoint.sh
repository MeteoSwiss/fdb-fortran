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

export PATH=$PATH:`cat /root/fdbf_prefix`
export PATH=$PATH:`cat /root/fdb_prefix`/bin
export FDB5_CONFIG_FILE=/scratch/fdb_config.yaml

fdb-info --all

for f in /fdb_data_files/*;
    do 
    echo "Archiving $f" ; 
    fdbf-write --keys=generatingProcessIdentifier,productionStatusOfProcessedData,discipline,parameterCategory,validityDate,validityTime,productDefinitionTemplateNumber,typeOfFirstFixedSurface,level,parameterNumber $f
done
