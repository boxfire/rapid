cmake_minimum_required(VERSION 3.12)
project(RapidLLVMPasses)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(rapid MODULE Passes/LowerRapidIntrinsics.cpp)

set_property(TARGET rapid PROPERTY CXX_STANDARD 17)

# Allow undefined symbols
target_link_libraries(rapid
  "$<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>")

add_custom_target(test
                  COMMAND lit test
                  DEPENDS rapid)
