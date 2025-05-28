cmake_minimum_required(VERSION 3.28)

set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "a9e1cf81-9932-4810-974b-6eccaf14e457")
# next: d0edc3af-4c50-42ea-a356-e2862fe7a444
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")

if(APPLE)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --config-system-dir=/opt/homebrew/etc/clang")
endif()

set(CMAKE_C_COMPILER "clang")
set(CMAKE_CXX_COMPILER "clang++")

project(lpc VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_MODULE_STD ON)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE MAIN "src/main.cpp")
file(GLOB_RECURSE CPP_MODULE_INTERFACE "src/*.cppm")
file(GLOB_RECURSE CPP_MODULE_IMPL "src/*.cpp")
list(REMOVE_ITEM CPP_MODULE_IMPL ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

add_executable(${PROJECT_NAME} ${MAIN} ${CPP_MODULE_IMPL})

target_sources(${PROJECT_NAME} 
  PRIVATE 
  FILE_SET CXX_MODULES FILES ${CPP_MODULE_INTERFACE}
)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-color=always")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -Wpedantic)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build (Debug/Release)" FORCE)
endif()

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0")
target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-g -O0>)

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -march=native -O3 -flto -DNDEBUG -ffile-prefix-map=${CMAKE_SOURCE_DIR}=/build")
target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Release>:-march=native -O3 -flto -DNDEBUG>)

target_link_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)

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
