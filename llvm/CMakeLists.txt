cmake_minimum_required(VERSION 3.12)
project(RapidLLVMPasses)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(rapid MODULE Passes/LowerRapidIntrinsics.cpp)

set_property(TARGET rapid PROPERTY CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Allow undefined symbols
target_link_libraries(rapid
  "$<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>")

add_custom_command(
  OUTPUT "${RAPID_SUPPORT_BINARIES}/rapid/librapid.so"
  COMMAND "cmake" ARGS "-E" copy_if_different $<TARGET_FILE:rapid> "${RAPID_SUPPORT_BINARIES}/rapid"
  DEPENDS rapid
  )

if (${RAPID_SUPPORT_BINARIES})
  add_custom_target(rapid_install_llvm_passes ALL
    DEPENDS "${RAPID_SUPPORT_BINARIES}/rapid/librapid.so"
    )
endif()

add_custom_target(rapid_test_llvm_passes
                  COMMAND lit test -v
                  DEPENDS rapid)

if (NOT TARGET test)
  add_custom_target(test)
endif()
add_dependencies(test rapid_test_llvm_passes)
