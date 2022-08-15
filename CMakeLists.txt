cmake_minimum_required(VERSION 3.10)
project(fdb-fortran
  LANGUAGES CXX Fortran
  VERSION 0.1)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

find_package(fdb5 REQUIRED)
find_package(eckit REQUIRED)
find_package(metkit REQUIRED)
add_subdirectory(src)

add_executable(test_fdb_archive test/test_fdb_archive.f90)
target_link_libraries(test_fdb_archive fdbf eccodes_f90)
target_include_directories(test_fdb_archive PUBLIC ${CMAKE_BINARY_DIR}/modules)

add_executable(test_fdb_retrieve test/test_fdb_retrieve.f90)
target_link_libraries(test_fdb_retrieve fdbf eccodes_f90)
target_include_directories(test_fdb_retrieve PUBLIC ${CMAKE_BINARY_DIR}/modules)

install( )