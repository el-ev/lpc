import std;
import lpc.test.framework;
import lpc.utils.arena;

#include "utils/macros.hpp"

using namespace lpc::test;
using lpc::utils::Arena;

struct TestTag { };
struct StringTag { };

// Concrete arena for testing (Arena's constructor is protected)
class TestArena : public Arena<TestTag, int, std::uint32_t> {
public:
    TestArena()
        : Arena() {
    }
    explicit TestArena(std::size_t cap)
        : Arena(cap) {
    }
};

class StringArena : public Arena<StringTag, std::string, std::uint32_t> {
public:
    StringArena()
        : Arena() {
    }
};

TEST(arena_empty_initially) {
    TestArena arena;
    ASSERT_TRUE(arena.empty());
    ASSERT_EQ(arena.size(), std::size_t(0));
}

TEST(arena_insert_and_access) {
    TestArena arena;
    auto ref = arena.insert(42);
    ASSERT_TRUE(ref.is_valid());
    ASSERT_FALSE(arena.empty());
    ASSERT_EQ(arena.size(), std::size_t(1));
    ASSERT_EQ(arena.at(ref), 42);
}

TEST(arena_emplace) {
    TestArena arena;
    auto ref = arena.emplace(100);
    ASSERT_TRUE(ref.is_valid());
    ASSERT_EQ(arena.at(ref), 100);
}

TEST(arena_multiple_elements) {
    TestArena arena;
    auto ref1 = arena.emplace(10);
    auto ref2 = arena.emplace(20);
    auto ref3 = arena.emplace(30);

    ASSERT_EQ(arena.size(), std::size_t(3));
    ASSERT_EQ(arena.at(ref1), 10);
    ASSERT_EQ(arena.at(ref2), 20);
    ASSERT_EQ(arena.at(ref3), 30);
}

TEST(arena_get_returns_nullptr_for_invalid) {
    TestArena arena;
    auto invalid = TestArena::elem_ref::invalid();
    ASSERT_FALSE(invalid.is_valid());
    ASSERT_TRUE(arena.get(invalid) == nullptr);
}

TEST(arena_get_returns_pointer_for_valid) {
    TestArena arena;
    auto ref = arena.emplace(55);
    auto* ptr = arena.get(ref);
    ASSERT_TRUE(ptr != nullptr);
    ASSERT_EQ(*ptr, 55);
}

TEST(arena_at_throws_for_invalid) {
    TestArena arena;
    auto invalid = TestArena::elem_ref::invalid();
    ASSERT_THROW((void)arena.at(invalid), std::out_of_range);
}

TEST(arena_reserve) {
    TestArena arena;
    arena.reserve(100);
    ASSERT_GT(arena.capacity(), std::size_t(0));
    ASSERT_TRUE(arena.empty());
}

TEST(arena_clear) {
    TestArena arena;
    (void)arena.emplace(1);
    (void)arena.emplace(2);
    (void)arena.emplace(3);
    ASSERT_EQ(arena.size(), std::size_t(3));

    arena.clear();
    ASSERT_TRUE(arena.empty());
    ASSERT_EQ(arena.size(), std::size_t(0));
}

TEST(arena_back_ref) {
    TestArena arena;

    // Empty arena returns invalid
    auto invalid_back = arena.back_ref();
    ASSERT_FALSE(invalid_back.is_valid());

    (void)arena.emplace(10);
    ASSERT_EQ(arena.at(arena.back_ref()), 10);

    (void)arena.emplace(20);
    ASSERT_EQ(arena.at(arena.back_ref()), 20);
}

TEST(arena_elem_ref_comparison) {
    TestArena arena;
    auto ref1 = arena.emplace(10);
    auto ref2 = arena.emplace(20);

    ASSERT_TRUE(ref1 == ref1);
    ASSERT_FALSE(ref1 == ref2);
    ASSERT_TRUE(ref1 < ref2);
}

TEST(arena_with_strings) {
    StringArena arena;
    auto ref1 = arena.emplace("hello");
    auto ref2 = arena.emplace("world");

    ASSERT_EQ(arena.at(ref1), std::string("hello"));
    ASSERT_EQ(arena.at(ref2), std::string("world"));
}

TEST(arena_iterators) {
    TestArena arena;
    (void)arena.emplace(10);
    (void)arena.emplace(20);
    (void)arena.emplace(30);

    std::vector<int> values;
    for (auto it = arena.begin(); it != arena.end(); ++it)
        values.push_back(*it);

    ASSERT_EQ(values.size(), std::size_t(3));
    ASSERT_EQ(values[0], 10);
    ASSERT_EQ(values[1], 20);
    ASSERT_EQ(values[2], 30);
}

TEST(arena_initial_capacity) {
    TestArena arena(50);
    ASSERT_TRUE(arena.empty());
    ASSERT_GT(arena.capacity(), std::size_t(0));
}

auto main() -> int {
    return RUN_TESTS();
}
