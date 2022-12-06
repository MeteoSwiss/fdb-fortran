ARG container_registry
FROM ${container_registry}/flexpart-poc/spack-dependencies:latest as intermediate
ARG TOKEN
ENV TOKEN=$TOKEN \
    SPACK_BRANCH=fdb_v0.18.1

# Prevent docker to cache git clones if the are changes in repo
ADD https://api.github.com/repos/C2SM/spack-c2sm/git/refs/heads/${SPACK_BRANCH} version_c2sm.json
RUN git config --global url."https://x-access-token:${TOKEN}@github.com/MeteoSwiss-APN/".insteadOf "git@github.com:MeteoSwiss-APN/"
RUN git config --global url."https://x-access-token:${TOKEN}@github.com/MeteoSwiss-APN/".insteadOf "https://github.com/MeteoSwiss-APN/"

RUN mkdir -p /scratch/spack-env/ 
COPY spack.yaml /scratch/spack-env/spack.yaml

RUN apt-get -yqq update \
    && apt-get -yqq install --no-install-recommends \
    zlib1g && spack external find

RUN spack env activate -p /scratch/spack-env/ && spack concretize && spack install

FROM ${container_registry}/flexpart-poc/spack-dependencies:latest

RUN mkdir -p /opt/spack/
RUN mkdir -p /root/c2sm-spack/

COPY --chmod=777 --from=intermediate /opt/spack/ /opt/spack/
COPY --from=intermediate /root/c2sm-spack/ /root/c2sm-spack/
COPY --chmod=777 --from=intermediate /scratch /scratch

COPY fdb_config.yaml /scratch/fdb_config.yaml

RUN mkdir /fdb_data_files
RUN mkdir /fdb_data

WORKDIR /scratch

COPY entrypoint.sh /scratch/entrypoint.sh 

ENTRYPOINT ["/bin/bash", "/scratch/entrypoint.sh"]

CMD [""]
