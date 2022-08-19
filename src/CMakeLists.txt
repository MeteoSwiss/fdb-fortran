set( fdb_fortran_tools
    fdbf-write
)

foreach(_tool ${fdb_fortran_tools})
    add_executable( ${_tool} tools/${_tool}.cc)
    target_include_directories(${_tool} PUBLIC .)
    target_link_libraries(${_tool} eckit)
    target_link_libraries(${_tool} fdb5)
    target_link_libraries(${_tool} fdb_message_lib)
    install(TARGETS ${_tool}
    DESTINATION ${CMAKE_INSTALL_PREFIX}
    )
endforeach()

set( fdb_message_srcs
   message/MessageArchiverF.cc
   message/MessageArchiverF.h
)

add_library(fdb_message_lib ${fdb_message_srcs})
target_include_directories(fdb_message_lib PUBLIC .)
target_link_libraries(fdb_message_lib eckit)
target_link_libraries(fdb_message_lib fdb5)
install(TARGETS fdb_message_lib
DESTINATION ${CMAKE_INSTALL_PREFIX}
)

set( fdb_fortran_srcs
   fortran/fdb.f90
)

add_library(fdbf ${fdb_fortran_srcs} )
target_include_directories(fdbf PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(fdbf PUBLIC fdb5)
install(TARGETS fdbf
DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} 
DESTINATION ${CMAKE_INSTALL_PREFIX}/include/fortran/fdb
)

