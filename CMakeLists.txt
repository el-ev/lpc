cmake_minimum_required(VERSION 3.28)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")

if(APPLE)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --config-system-dir=/opt/homebrew/etc/clang")
  # Ensure the linker uses Homebrew's libc++ (not Apple's system libc++)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++")
endif()

set(CMAKE_C_COMPILER "/opt/homebrew/opt/llvm/bin/clang")
set(CMAKE_CXX_COMPILER "/opt/homebrew/opt/llvm/bin/clang++")

project(lpc VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# --- import std setup ---
# CMake's auto-detection of libc++.modules.json fails on Homebrew LLVM because
# clang -print-library-module-manifest-path returns <NOT PRESENT>.
# We manually locate the manifest and provide it to cmake.

# Try compiler detection first
execute_process(
  COMMAND "${CMAKE_CXX_COMPILER}" -print-library-module-manifest-path
  OUTPUT_VARIABLE _libcxx_manifest
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)

if(NOT IS_ABSOLUTE "${_libcxx_manifest}" OR NOT EXISTS "${_libcxx_manifest}")
  # Fallback: search known paths (Homebrew LLVM)
  unset(_libcxx_manifest)
  unset(_libcxx_manifest CACHE)
  find_file(_libcxx_manifest
    NAMES libc++.modules.json
    PATHS
      "/opt/homebrew/opt/llvm/lib/c++"
      "/opt/homebrew/lib/c++"
      "/usr/local/lib/c++"
      "/usr/lib/c++"
    NO_DEFAULT_PATH
  )
endif()

if(EXISTS "${_libcxx_manifest}")
  message(STATUS "Found libc++ modules manifest: ${_libcxx_manifest}")
  get_filename_component(_libcxx_manifest_dir "${_libcxx_manifest}" DIRECTORY)

  # Read the manifest to find std module source paths
  file(READ "${_libcxx_manifest}" _manifest_json)
  # Extract the source paths from the JSON (they're relative to the manifest dir)
  string(JSON _modules_array GET "${_manifest_json}" "modules")
  string(JSON _modules_len LENGTH "${_manifest_json}" "modules")

  set(_std_module_sources "")
  set(_std_module_include_dirs "")
  math(EXPR _last_idx "${_modules_len} - 1")
  foreach(_idx RANGE ${_last_idx})
    string(JSON _source_path GET "${_manifest_json}" "modules" ${_idx} "source-path")
    cmake_path(ABSOLUTE_PATH _source_path BASE_DIRECTORY "${_libcxx_manifest_dir}" NORMALIZE)
    list(APPEND _std_module_sources "${_source_path}")

    # Extract include directories if present
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
    "Ensure LLVM/libc++ is installed (e.g., brew install llvm).")
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
target_compile_options(std_module PRIVATE $<$<CONFIG:Debug>:-g -O0>)
target_compile_options(std_module PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)
target_link_options(std_module PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)
set_target_properties(std_module PROPERTIES CXX_MODULE_STD OFF)

# --- Main target ---
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

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-color=always")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -Wpedantic)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build (Debug/Release)" FORCE)
endif()

target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-g -O0>)
target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)

target_link_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)

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
    target_compile_options(${TEST_NAME} PRIVATE $<$<CONFIG:Debug>:-g -O0>)
    target_compile_options(${TEST_NAME} PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)
    target_link_options(${TEST_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)

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
