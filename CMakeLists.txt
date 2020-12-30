cmake_minimum_required(VERSION 3.13)

project(rapid)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

find_package(Idris2)

set(RAPID_SUPPORT_BINARIES "${CMAKE_BINARY_DIR}/build/support")
add_custom_target(test)

add_idris_package(rapid2-cg rapid-cg.ipkg
  src/Rapid.idr
  src/Rapid/Driver.idr
  )

add_idris_package(rapidc rapidc.ipkg
  src/Rapid.idr
  src/Rapid/Driver.idr
  )

add_subdirectory(llvm)
add_subdirectory(rts)

add_custom_target(rapidc_test
  COMMAND cmake -E env "IDRIS2_DATA=${RAPID_SUPPORT_BINARIES}:${CMAKE_CURRENT_SOURCE_DIR}/support:$ENV{IDRIS2_DATA}" rapidc=${rapidc_EXECUTABLE} ./runtests.sh --good
  DEPENDS rapidc rapid_rts
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  USES_TERMINAL
  )
add_dependencies(test rapidc_test)
