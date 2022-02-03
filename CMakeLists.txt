cmake_minimum_required(VERSION 3.20)
project(irrlisp)

set(CMAKE_CXX_STANDARD 17)

set(BUILD_SHARED_LIBS OFF CACHE BOOL "build shared libraries" FORCE)
add_subdirectory(irrlicht)
add_subdirectory(ecl)
set(BUILD_UNPLUSPLUS_EXAMPLES OFF CACHE BOOL "unplusplus examples" FORCE)
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
set_target_properties(IrrlichtMt-clib PROPERTIES
    CXX_STANDARD 17
    POSITION_INDEPENDENT_CODE ON)

set(ST_JSON_SRC "${PROJECT_SOURCE_DIR}/ST-JSON")
set(CL_UPP_SRC "${PROJECT_SOURCE_DIR}/cl-upp")
set(IRRLISP_SRC "${PROJECT_SOURCE_DIR}/src")
set(SWANK_SRC "${PROJECT_SOURCE_DIR}/swank")
get_target_property(IRRLICHT_LINK_DIR IrrlichtMt LIBRARY_OUTPUT_DIRECTORY)

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
    SYSTEM_DEPS ST-JSON cl-upp swank IrrlichtMt-clib
    ECL_LD_FLAGS "-L${PROJECT_BINARY_DIR} -lIrrlichtMt-clib -lstdc++ -L${IRRLICHT_LINK_DIR} -Wl,-rpath=${IRRLICHT_LINK_DIR} -lIrrlichtMt")
add_ecl_library(SYSTEM_NAME irrlisp-test
    LISP_SRC "${IRRLISP_SRC}"
    SYSTEM_PATHS
    "${CL_UPP_SRC}"
    "${ST_JSON_SRC}"
    "${SWANK_SRC}"
    SYSTEM_DEPS irrlisp)

emit_all_systems_header()

add_executable(irrlisp-tester src/main.cpp)
target_link_libraries(irrlisp-tester irrlisp-test IrrIMGUI)
target_include_directories(irrlisp-tester PUBLIC "${LISP_SYSTEMS_INCLUDE}")
