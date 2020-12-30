cmake_minimum_required(VERSION 3.13)

project(rapid)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

find_package(Idris2)

add_idris_package(rapid2-cg rapid-cg.ipkg
  src/Rapid.idr
  src/Rapid/Driver.idr
  )

add_idris_package(rapidc rapidc.ipkg
  src/Rapid.idr
  src/Rapid/Driver.idr
  )

message("rapidc is: ${rapidc_EXECUTABLE}")

add_custom_target(rapidc_test
  COMMAND cmake -E env rapidc=${rapidc_EXECUTABLE} ./runtests.sh --good
  DEPENDS rapidc rts
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  USES_TERMINAL
  )
