set( fdb_fortran_tools
    fdbf-write
)

foreach(_tool ${fdb_fortran_tools})
    add_executable( ${_tool} tools/${_tool}.cc)
    target_include_directories(${_tool} PUBLIC .)
    target_link_libraries(${_tool} eckit)
    target_link_libraries(${_tool} fdb5)
    target_link_libraries(${_tool} fdb_fortran_lib)
endforeach()

set( fdb_fortran_lib_srcs
   message/MessageArchiverF.cc
   message/MessageArchiverF.h
)

add_library(fdb_fortran_lib ${fdb_fortran_lib_srcs})
target_include_directories(fdb_fortran_lib PUBLIC .)
target_link_libraries(fdb_fortran_lib eckit)
target_link_libraries(fdb_fortran_lib fdb5)
