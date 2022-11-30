ARG container_registry
ARG spack_tag=dev_flexpart-fdb
FROM ${container_registry}/flexpart-poc/spack:${spack_tag} as intermediate
ARG TOKEN
ENV TOKEN=$TOKEN \
    SPACK_BRANCH=fdb_v0.18.1

# Prevent docker to cache git clones if the are changes in repo
ADD https://api.github.com/repos/C2SM/spack-c2sm/git/refs/heads/${SPACK_BRANCH} version_c2sm.json
RUN git config --global url."https://x-access-token:${TOKEN}@github.com/MeteoSwiss-APN/".insteadOf "git@github.com:MeteoSwiss-APN/"
RUN git config --global url."https://x-access-token:${TOKEN}@github.com/MeteoSwiss-APN/".insteadOf "https://github.com/MeteoSwiss-APN/"

RUN mkdir -p /scratch/spack-env/ 
COPY docker/spack.yaml /scratch/spack-env/spack.yaml
RUN  spack env create spack-env /scratch/spack-env/spack.yaml

RUN apt-get -yqq update \
    && apt-get -yqq install --no-install-recommends \
    zlib1g && spack external find

RUN spack env activate -p spack-env && spack concretize && spack install

FROM ${container_registry}/flexpart-poc/spack:${spack_tag}

RUN mkdir -p /opt/spack/
RUN mkdir -p /root/c2sm-spack/

COPY --from=intermediate /opt/spack/ /opt/spack/
COPY --from=intermediate /root/c2sm-spack/ /root/c2sm-spack/
COPY --from=intermediate /scratch /scratch

RUN mkdir /fdb_data_files
RUN mkdir /fdb_data
WORKDIR /scratch
COPY entrypoint.sh /scratch/entrypoint.sh 

ENTRYPOINT ["/bin/bash", "/scratch/entrypoint.sh"]

CMD [""]
