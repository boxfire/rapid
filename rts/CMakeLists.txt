set(RTS_SOURCES
  builtin.c
  gc.c
  getline.c
  object.c
  rts.c
  ../external/llvm-statepoint-utils/build/statepoint.c
  )

set(RTS_BITCODE)
set(RAPID_RTS_LIB "${RAPID_SUPPORT_BINARIES}/rapid/runtime.bc")
set(RAPID_PLATFORM_LIB "${RAPID_SUPPORT_BINARIES}/rapid/platform.a")

set(CFLAGS)
list(APPEND CFLAGS "-I" ".")
list(APPEND CFLAGS "-I" "/usr/local/include")
list(APPEND CFLAGS "-I" "../external/llvm-statepoint-utils/dist")

list(APPEND CFLAGS "-D_POSIX_C_SOURCE=200809L")
if (APPLE)
  list(APPEND CFLAGS "-D_DARWIN_C_SOURCE")
else()
  list(APPEND CFLAGS "-D_GNU_SOURCE")
endif()

foreach(C_SOURCE ${RTS_SOURCES})
  string(REGEX REPLACE "\.c$" ".bc" BC_TARGET ${C_SOURCE})
  add_custom_command(
    OUTPUT "${CMAKE_BINARY_DIR}/${BC_TARGET}"
    COMMAND clang ARGS "-flto" "-g" "-O3" "-c" "-Wall" "-Wpedantic" "-Werror" "-Wno-format"
    ${CFLAGS} "-std=c11" "-o" "${CMAKE_BINARY_DIR}/${BC_TARGET}" "${C_SOURCE}"
    DEPENDS ${C_SOURCE}
    )
  set(RTS_BITCODE "${RTS_BITCODE};${CMAKE_BINARY_DIR}/${BC_TARGET}")
endforeach()

add_custom_command(
  OUTPUT "${RAPID_RTS_LIB}"
  COMMAND llvm-link ARGS -o "${RAPID_RTS_LIB}"
  ${RTS_BITCODE}
  DEPENDS ${RTS_BITCODE}
  )

set(RAPID_PLATFORM_SOURCE "arch_x86-64.S")
add_custom_command(
  OUTPUT "${RAPID_PLATFORM_LIB}"
  COMMAND clang ARGS "-c" "-o" "${RAPID_PLATFORM_LIB}.o" ${RAPID_PLATFORM_SOURCE}
  COMMAND llvm-ar ARGS rcsu "${RAPID_PLATFORM_LIB}" "${RAPID_PLATFORM_LIB}.o"
  DEPENDS ${RAPID_PLATFORM_SOURCE}
  )

add_custom_target(rapid_rts
  DEPENDS "${RAPID_RTS_LIB}" "${RAPID_PLATFORM_LIB}"
  )
