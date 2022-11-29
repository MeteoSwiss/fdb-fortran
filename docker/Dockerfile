ARG container_registry
ARG spack_tag=dev_flexpart-fdb
FROM ${container_registry}/flexpart-poc/spack:${spack_tag} as intermediate
ARG TOKEN
ENV TOKEN=$TOKEN \
    SPACK_BRANCH=fdb_v0.18.1

RUN spack spec fdb-fortran ^fdb@inspect ^eckit@1.20.2: ~mpi ^eccodes@2.19.0 jp2k=none +fortran +build_shared_libs ^hdf5 ~mpi

# Prevent docker to cache git clones if the are changes in repo
ADD https://api.github.com/repos/C2SM/spack-c2sm/git/refs/heads/${SPACK_BRANCH} version_c2sm.json
RUN git config --global url."https://x-access-token:${TOKEN}@github.com/MeteoSwiss-APN/".insteadOf "git@github.com:MeteoSwiss-APN/"
RUN git config --global url."https://x-access-token:${TOKEN}@github.com/MeteoSwiss-APN/".insteadOf "https://github.com/MeteoSwiss-APN/"

RUN spack config add "config:install_tree:root: /opt/spack"
RUN spack -d install fdb-fortran ^fdb@master ^eckit@1.20.2: ~mpi ^eccodes@2.19.0 jp2k=none +fortran +build_shared_libs ^hdf5 ~mpi
RUN echo `spack location -i fdb-fortran`> /root/fdbf_prefix
RUN echo `spack location -i fdb`> /root/fdb_prefix

FROM ${container_registry}/flexpart-poc/spack:${spack_tag}
RUN mkdir -p /opt/spack/
COPY --from=intermediate /root/fdbf_prefix /root/fdbf_prefix
COPY --from=intermediate /root/fdb_prefix /root/fdb_prefix
COPY --from=intermediate /opt/spack/ /opt/spack/

RUN mkdir /fdb_data_files
RUN mkdir /fdb_data
WORKDIR /scratch
COPY entrypoint.sh /scratch/entrypoint.sh 

ENTRYPOINT ["/bin/bash", "/scratch/entrypoint.sh"]

CMD [""]