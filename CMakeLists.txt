cmake_minimum_required(VERSION 3.10)
project(fdb-fortran
  LANGUAGES CXX
  VERSION 0.1)

find_package(fdb5 REQUIRED)
find_package(eckit REQUIRED)
find_package(metkit REQUIRED)
add_subdirectory(src)

