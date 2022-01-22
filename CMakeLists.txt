cmake_minimum_required(VERSION 3.20)
project(irrlisp)

set(BUILD_SHARED_LIBS OFF)

add_subdirectory(irrlicht)
add_subdirectory(ecl)
set(BUILD_UNPLUSPLUS_EXAMPLES OFF)
add_subdirectory(unplusplus)

set(IRRIMGUI_STATIC_LIBRARY ON)
add_subdirectory(IrrIMGUI)

set(IRRLICHT_INCLUDE_DIR "${PROJECT_SOURCE_DIR}/irrlicht/include")
add_unplusplus_clib(IrrlichtMt-clib
    HEADER "${IRRLICHT_INCLUDE_DIR}/irrlicht.h"
    LIBRARY IrrlichtMt
    EXCLUDES_FILE "${CMAKE_CURRENT_SOURCE_DIR}/unplusplus/examples/irrlicht/excludes.txt"
    NO_DEPRECATED
    CXXFLAGS -std=gnu++17)
set_target_properties(IrrlichtMt-clib PROPERTIES CXX_STANDARD 17)

set(ST_JSON_SRC "${PROJECT_SOURCE_DIR}/ST-JSON")
set(CL_UPP_SRC "${PROJECT_SOURCE_DIR}/cl-upp")
set(IRRLISP_SRC "${PROJECT_SOURCE_DIR}/src")
set(SWANK_SRC "${PROJECT_SOURCE_DIR}/swank")

add_ecl_library(SYSTEM_NAME ST-JSON
    LISP_SRC "${ST_JSON_SRC}")
add_ecl_library(SYSTEM_NAME swank
    LISP_SRC "${SWANK_SRC}")
add_ecl_library(SYSTEM_NAME cl-upp
    LISP_SRC "${CL_UPP_SRC}"
    SYSTEM_PATHS "${ST_JSON_SRC}"
    SYSTEM_DEPS ST-JSON)
add_ecl_library(SYSTEM_NAME irrlisp
    LISP_SRC "${IRRLISP_SRC}"
    SYSTEM_PATHS
    "${CL_UPP_SRC}"
    "${ST_JSON_SRC}"
    "${SWANK_SRC}"
    SYSTEM_DEPS ST-JSON cl-upp swank IrrlichtMt-clib)

add_executable(irrlisp-test src/main.cpp)
target_link_libraries(irrlisp-test irrlisp IrrIMGUI)
