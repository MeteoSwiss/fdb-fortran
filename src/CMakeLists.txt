set( fdb_fortran_tools
    fdbf-write
)

foreach(_tool ${fdb_fortran_tools})
    add_executable( ${_tool} tools/${_tool}.cc)
    target_include_directories(${_tool} PUBLIC .)
    target_link_libraries(${_tool} eckit)
    target_link_libraries(${_tool} fdb5)
    target_link_libraries(${_tool} fdb_message_lib)
endforeach()

set( fdb_message_srcs
   message/MessageArchiverF.cc
   message/MessageArchiverF.h
)

add_library(fdb_message_lib ${fdb_message_srcs})
target_include_directories(fdb_message_lib PUBLIC .)
target_link_libraries(fdb_message_lib eckit)
target_link_libraries(fdb_message_lib fdb5)

set( fdb_fortran_srcs
   fortran/fdb.f90
)

add_library(fdbf ${fdb_fortran_srcs} )
target_link_libraries(fdbf fdb5)

