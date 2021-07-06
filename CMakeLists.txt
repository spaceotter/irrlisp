cmake_minimum_required(VERSION 3.20)
project(irrlisp)

set(BUILD_SHARED_LIBS OFF)

add_subdirectory(irrlicht)
add_subdirectory(ecl)
set(BUILD_UNPLUSPLUS_EXAMPLES OFF)
add_subdirectory(unplusplus)

#get_target_property(IRRLICHT_LIB IrrlichtMt LIBRARY_OUTPUT_NAME)
#set(IRRLICHT_LIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libIrrlichtMt.a")
set(IRRLICHT_INC "${PROJECT_SOURCE_DIR}/irrlicht/include")

include(unplusplus/cmake/unplusplus.cmake)
add_unplusplus_clib("${IRRLICHT_INC}/irrlicht.h"
    IrrlichtMt
    --excludes-file "${CMAKE_CURRENT_SOURCE_DIR}/unplusplus/examples/irrlicht/excludes.txt"
    --no-deprecated
    --extra-arg-before=-std=gnu++17)
set_target_properties(IrrlichtMt-clib PROPERTIES CXX_STANDARD 17)

set(ST_JSON_SRC "${PROJECT_SOURCE_DIR}/ST-JSON")
set(CL_UPP_SRC "${PROJECT_SOURCE_DIR}/cl-upp")
set(IRRLISP_SRC "${PROJECT_SOURCE_DIR}/src")
set(SWANK_SRC "${PROJECT_SOURCE_DIR}/swank")

configure_file(
    "${PROJECT_SOURCE_DIR}/irrlisp-build.lisp.in"
    "${PROJECT_BINARY_DIR}/irrlisp-build.lisp")
set(IRRLISP_ECL_LIB "${PROJECT_BINARY_DIR}/irrlisp.a")

# TODO: don't use glob
file(GLOB_RECURSE IRRLISP_COMMON_LISP_SRC LIST_DIRECTORIES false
    "${ST_JSON_SRC}/*.lisp" "${ST_JSON_SRC}/*.asd"
    "${CL_UPP_SRC}/*.lisp" "${CL_UPP_SRC}/*.asd"
    "${IRRLISP_SRC}/*.lisp" "${IRRLISP_SRC}/*.asd"
    "${SWANK_SRC}/*.lisp" "${SWANK_SRC}/*.asd"
    )

add_custom_command(
    OUTPUT "${IRRLISP_ECL_LIB}"
    COMMAND
    "${ECL_BINARY_DIR}/ecl"
    -norc
    -load "irrlisp-build.lisp"
    COMMAND
    mv "${PROJECT_BINARY_DIR}/irrlisp--all-systems.a" "${IRRLISP_ECL_LIB}"
    DEPENDS ecl-prj "${PROJECT_BINARY_DIR}/irrlisp-build.lisp" IrrlichtMt-clib
    ${IRRLISP_COMMON_LISP_SRC})

add_custom_target(irrlisp-lib
    DEPENDS "${IRRLISP_ECL_LIB}" IrrlichtMt-clib ecl
    )

add_executable(irrlisp src/main.cpp)
target_compile_options(irrlisp PUBLIC -Werror-implicit-function-declaration)
add_dependencies(irrlisp irrlisp-lib)
target_link_libraries(irrlisp "${IRRLISP_ECL_LIB}" IrrlichtMt-clib ecl)
