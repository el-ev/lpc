cmake_minimum_required(VERSION 3.28)

project(lpc VERSION 0.1.0 LANGUAGES CXX)

if(NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  message(FATAL_ERROR
    "lpc requires Clang with libc++ (for C++26 std modules); "
    "got ${CMAKE_CXX_COMPILER_ID}. "
    "Set CC/CXX or CMAKE_CXX_COMPILER accordingly.")
endif()

# Apple ar/ranlib cannot index LLVM bitcode produced by -flto.  Use the
# archiver tools paired with Clang so Release static libraries remain valid.
if(APPLE AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  if(CMAKE_AR STREQUAL "/usr/bin/ar" AND CMAKE_CXX_COMPILER_AR)
    set(CMAKE_AR "${CMAKE_CXX_COMPILER_AR}")
  endif()
  if(CMAKE_RANLIB STREQUAL "/usr/bin/ranlib" AND CMAKE_CXX_COMPILER_RANLIB)
    set(CMAKE_RANLIB "${CMAKE_CXX_COMPILER_RANLIB}")
  endif()
endif()

add_compile_options(-stdlib=libc++ -fdiagnostics-color=always)
add_link_options(-stdlib=libc++)

if(DEFINED ENV{NIX_CC})
  foreach(_implicit_dir IN LISTS CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES)
    add_compile_options("SHELL:-isystem ${_implicit_dir}")
  endforeach()
  unset(_implicit_dir)
endif()

if(APPLE AND EXISTS "/opt/homebrew/etc/clang")
  add_compile_options(--config-system-dir=/opt/homebrew/etc/clang)
  if(EXISTS "/opt/homebrew/opt/llvm/lib/c++")
    add_link_options(-L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++)
  endif()
endif()

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(LIBCXX_MODULES_JSON "" CACHE FILEPATH "Path to libc++.modules.json")

if(NOT LIBCXX_MODULES_JSON AND DEFINED ENV{LIBCXX_MODULES_JSON})
  set(LIBCXX_MODULES_JSON "$ENV{LIBCXX_MODULES_JSON}" CACHE FILEPATH
    "Path to libc++.modules.json" FORCE)
endif()

if(NOT LIBCXX_MODULES_JSON)
  execute_process(
    COMMAND "${CMAKE_CXX_COMPILER}" -print-library-module-manifest-path
    OUTPUT_VARIABLE _libcxx_manifest
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(IS_ABSOLUTE "${_libcxx_manifest}" AND EXISTS "${_libcxx_manifest}")
    set(LIBCXX_MODULES_JSON "${_libcxx_manifest}" CACHE FILEPATH
      "Path to libc++.modules.json" FORCE)
  endif()
  unset(_libcxx_manifest)
endif()

if(NOT LIBCXX_MODULES_JSON)
  get_filename_component(_cxx_dir "${CMAKE_CXX_COMPILER}" DIRECTORY)
  find_file(_libcxx_modules_json
    NAMES libc++.modules.json
    PATHS
      "${_cxx_dir}/../lib/c++"
      "${_cxx_dir}/../lib"
      "/opt/homebrew/opt/llvm/lib/c++"
      "/opt/homebrew/lib/c++"
      "/usr/local/opt/llvm/lib/c++"
      "/usr/local/lib/c++"
      "/usr/lib/c++"
    NO_DEFAULT_PATH
  )
  if(_libcxx_modules_json)
    set(LIBCXX_MODULES_JSON "${_libcxx_modules_json}" CACHE FILEPATH
      "Path to libc++.modules.json" FORCE)
  endif()
  unset(_libcxx_modules_json CACHE)
  unset(_cxx_dir)
endif()

if(LIBCXX_MODULES_JSON AND EXISTS "${LIBCXX_MODULES_JSON}")
  message(STATUS "Found libc++ modules manifest: ${LIBCXX_MODULES_JSON}")
  get_filename_component(_libcxx_manifest_dir "${LIBCXX_MODULES_JSON}" DIRECTORY)

  file(READ "${LIBCXX_MODULES_JSON}" _manifest_json)
  string(JSON _modules_len LENGTH "${_manifest_json}" "modules")

  set(_std_module_sources "")
  set(_std_module_include_dirs "")
  math(EXPR _last_idx "${_modules_len} - 1")
  foreach(_idx RANGE ${_last_idx})
    string(JSON _source_path GET "${_manifest_json}" "modules" ${_idx} "source-path")
    cmake_path(ABSOLUTE_PATH _source_path BASE_DIRECTORY "${_libcxx_manifest_dir}" NORMALIZE)
    list(APPEND _std_module_sources "${_source_path}")

    string(JSON _has_local ERROR_VARIABLE _local_err GET "${_manifest_json}" "modules" ${_idx} "local-arguments" "system-include-directories")
    if(NOT _local_err)
      string(JSON _inc_len LENGTH "${_manifest_json}" "modules" ${_idx} "local-arguments" "system-include-directories")
      math(EXPR _inc_last "${_inc_len} - 1")
      foreach(_inc_idx RANGE ${_inc_last})
        string(JSON _inc_path GET "${_manifest_json}" "modules" ${_idx} "local-arguments" "system-include-directories" ${_inc_idx})
        cmake_path(ABSOLUTE_PATH _inc_path BASE_DIRECTORY "${_libcxx_manifest_dir}" NORMALIZE)
        list(APPEND _std_module_include_dirs "${_inc_path}")
      endforeach()
    endif()
  endforeach()

  list(REMOVE_DUPLICATES _std_module_include_dirs)
  message(STATUS "libc++ std module sources: ${_std_module_sources}")
  message(STATUS "libc++ std module include dirs: ${_std_module_include_dirs}")
else()
  message(FATAL_ERROR
    "Could not find libc++.modules.json. "
    "Set -DLIBCXX_MODULES_JSON=<path> or export LIBCXX_MODULES_JSON, "
    "or ensure LLVM/libc++ is installed (e.g., brew install llvm).")
endif()

# Build a static library providing the std module
add_library(std_module STATIC)
target_sources(std_module
  PUBLIC
  FILE_SET CXX_MODULES
  BASE_DIRS ${_std_module_include_dirs}
  FILES ${_std_module_sources}
)
target_include_directories(std_module SYSTEM PUBLIC ${_std_module_include_dirs})
target_compile_options(std_module PRIVATE
  -Wno-reserved-module-identifier
  -Wno-reserved-identifier
)
# std_module must be compiled with the same target features as consumers,
# otherwise the precompiled module (.pcm) will have a configuration mismatch.
target_compile_options(std_module PRIVATE $<$<CONFIG:Debug>:-g -O0 -fsanitize=address,undefined>)
target_compile_options(std_module PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)
target_link_options(std_module PRIVATE $<$<CONFIG:Debug>:-fsanitize=address,undefined>)
set_target_properties(std_module PROPERTIES CXX_MODULE_STD OFF)

file(GLOB_RECURSE MAIN "src/main.cpp")
file(GLOB_RECURSE CPP_MODULE_INTERFACE "src/*.cppm")
file(GLOB_RECURSE CPP_MODULE_IMPL "src/*.cpp")
list(REMOVE_ITEM CPP_MODULE_IMPL ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

add_executable(${PROJECT_NAME} ${MAIN} ${CPP_MODULE_IMPL})

target_sources(${PROJECT_NAME}
  PRIVATE
  FILE_SET CXX_MODULES FILES ${CPP_MODULE_INTERFACE}
)
target_link_libraries(${PROJECT_NAME} PRIVATE std_module)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_MODULE_STD OFF)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -Wpedantic -ffile-prefix-map=${CMAKE_SOURCE_DIR}=.)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build (Debug/Release)" FORCE)
endif()

target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-g -fsanitize=address,undefined -O0>)
target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)

target_link_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address,undefined>)

# --- Test targets ---
file(GLOB TEST_SOURCES "test/cpp/*.cpp")
file(GLOB TEST_MODULE_INTERFACE "test/cpp/utils/*.cppm")
set(TEST_TARGETS "")
foreach(TEST_SOURCE ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_SOURCE} ${CPP_MODULE_IMPL})

    target_sources(${TEST_NAME}
      PRIVATE
      FILE_SET CXX_MODULES FILES ${TEST_MODULE_INTERFACE} ${CPP_MODULE_INTERFACE}
    )
    target_link_libraries(${TEST_NAME} PRIVATE std_module)
    set_target_properties(${TEST_NAME} PROPERTIES CXX_MODULE_STD OFF)

    target_compile_options(${TEST_NAME} PRIVATE -Wall -Wextra -Wpedantic)
    target_compile_options(${TEST_NAME} PRIVATE $<$<CONFIG:Debug>:-g -O0 -fsanitize=address,undefined>)
    target_compile_options(${TEST_NAME} PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)
    target_link_options(${TEST_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address,undefined>)

    list(APPEND TEST_TARGETS ${TEST_NAME})
endforeach()

if(TEST_TARGETS)
    add_custom_target(test_cpp
        COMMAND ${CMAKE_COMMAND} -E echo "Running C++ tests..."
        DEPENDS ${TEST_TARGETS}
        COMMENT "Running all C++ tests"
    )

    foreach(TEST_TARGET ${TEST_TARGETS})
        add_custom_command(TARGET test_cpp POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E echo "Running ${TEST_TARGET}..."
            COMMAND $<TARGET_FILE:${TEST_TARGET}>
            COMMENT "Running test: ${TEST_TARGET}"
        )
    endforeach()
endif()
