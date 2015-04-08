find_package(Boost REQUIRED COMPONENTS program_options)
add_executable (gecko main.cpp)
target_link_libraries(gecko ${Boost_LIBRARIES} geckort)
