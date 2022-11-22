
FDB container
=======


1. Sync data from S3 bucket into rzplus-poc/fdb/data folder. (This is original GRIB data to be be added to fdb using fdb-write)
```
cd rzplus-poc/fdb/data ; aws s3 sync s3://flexpart-poc-fdb .
```

2. Create volume (This is where data will be stored by fdb, ie created by fdb-write)
```
docker volume create fdb_data
```

3. Run docker container with fdb/data folder (Host Volume) and fdb_data volume mounted.
```
docker run --tty --interactive --name=fdb -v "$(pwd)"/data:/root/scratch/FDB_ROOT/fdb/root --mount source=fdb_data,destination=/root/scratch/FDB_ROOT/root $container_registry/$(cat TAG) 
```

4. Stop/Remove fdb container
```
docker container stop fdb; docker container rm fdb;
```