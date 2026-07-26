import std;
import lpc.test.framework;
import lpc.utils.tagged_union;

#include "utils/macros.hpp"

using namespace lpc::test;
using lpc::utils::TaggedUnion;

TEST(one_int_elem_default) {
    TaggedUnion<int> one_int;
    ASSERT_TRUE(one_int.valueless());
    ASSERT_EQ(one_int.index(), static_cast<std::size_t>(-1));
    ASSERT_FALSE(one_int.isa<int>());
    ASSERT_TRUE(one_int.get<int>() == nullptr);
    one_int = 42;
    ASSERT_FALSE(one_int.valueless());
    ASSERT_EQ(one_int.index(), static_cast<std::size_t>(0));
    ASSERT_TRUE(one_int.isa<int>());
    ASSERT_TRUE(one_int.get<int>() != nullptr);
    ASSERT_TRUE(one_int.get<0>() != nullptr);
    ASSERT_EQ(*one_int.get<int>(), 42);
    ASSERT_EQ(*one_int.get_unchecked<int>(), 42);
    ASSERT_NE(*one_int.get_unchecked<int>(), 43);
}

TEST(multiple_types_basic) {
    TaggedUnion<int, std::string, double> variant;
    ASSERT_TRUE(variant.valueless());

    variant = 42;
    ASSERT_FALSE(variant.valueless());
    ASSERT_EQ(variant.index(), static_cast<std::size_t>(0));
    ASSERT_TRUE(variant.isa<int>());
    ASSERT_FALSE(variant.isa<std::string>());
    ASSERT_FALSE(variant.isa<double>());
    ASSERT_EQ(*variant.get_unchecked<int>(), 42);

    variant = std::string("hello");
    ASSERT_EQ(variant.index(), static_cast<std::size_t>(1));
    ASSERT_FALSE(variant.isa<int>());
    ASSERT_TRUE(variant.isa<std::string>());
    ASSERT_FALSE(variant.isa<double>());
    ASSERT_EQ(*variant.get_unchecked<std::string>(), "hello");

    variant = 3.14;
    ASSERT_EQ(variant.index(), static_cast<std::size_t>(2));
    ASSERT_FALSE(variant.isa<int>());
    ASSERT_FALSE(variant.isa<std::string>());
    ASSERT_TRUE(variant.isa<double>());
    ASSERT_EQ(*variant.get_unchecked<double>(), 3.14);
}

TEST(copy_constructor) {
    TaggedUnion<int, std::string> original(42);
    TaggedUnion<int, std::string> copy(original);

    ASSERT_FALSE(copy.valueless());
    ASSERT_EQ(copy.index(), std::size_t(0));
    ASSERT_TRUE(copy.isa<int>());
    ASSERT_EQ(*copy.get_unchecked<int>(), 42);

    // Original should still be valid
    ASSERT_EQ(*original.get_unchecked<int>(), 42);
}

TEST(move_constructor) {
    TaggedUnion<int, std::string> original(std::string("test"));
    TaggedUnion<int, std::string> moved(std::move(original));

    ASSERT_FALSE(moved.valueless());
    ASSERT_EQ(moved.index(), std::size_t(1));
    ASSERT_TRUE(moved.isa<std::string>());
    ASSERT_EQ(*moved.get_unchecked<std::string>(), "test");

    // Original should be valueless after move
    ASSERT_TRUE(original.valueless());
}

TEST(copy_assignment) {
    TaggedUnion<int, std::string> variant1(42);
    TaggedUnion<int, std::string> variant2(std::string("hello"));

    variant2 = variant1;

    ASSERT_FALSE(variant2.valueless());
    ASSERT_EQ(variant2.index(), std::size_t(0));
    ASSERT_TRUE(variant2.isa<int>());
    ASSERT_EQ(*variant2.get_unchecked<int>(), 42);
}

TEST(move_assignment) {
    TaggedUnion<int, std::string> variant1(std::string("test"));
    TaggedUnion<int, std::string> variant2(42);

    variant2 = std::move(variant1);

    ASSERT_FALSE(variant2.valueless());
    ASSERT_EQ(variant2.index(), std::size_t(1));
    ASSERT_TRUE(variant2.isa<std::string>());
    ASSERT_EQ(*variant2.get_unchecked<std::string>(), "test");

    ASSERT_TRUE(variant1.valueless());
}

TEST(emplace_functionality) {
    TaggedUnion<int, std::string> variant;

    auto& str_ref = variant.emplace<std::string>("hello world");
    ASSERT_FALSE(variant.valueless());
    ASSERT_EQ(variant.index(), static_cast<std::size_t>(1));
    ASSERT_TRUE(variant.isa<std::string>());
    ASSERT_EQ(*variant.get_unchecked<std::string>(), "hello world");
    ASSERT_EQ(&str_ref, variant.get_unchecked<std::string>());

    auto& int_ref = variant.emplace<int>(99);
    ASSERT_EQ(variant.index(), static_cast<std::size_t>(0));
    ASSERT_TRUE(variant.isa<int>());
    ASSERT_EQ(*variant.get_unchecked<int>(), 99);
    ASSERT_EQ(&int_ref, variant.get_unchecked<int>());
}

TEST(visit_functionality) {
    TaggedUnion<int, std::string, double> variant;

    variant = 42;
    auto result1 = variant.visit([](const auto& value) -> std::string {
        if constexpr (std::is_same_v<std::decay_t<decltype(value)>, int>) {
            return "int: " + std::to_string(value);
        } else if constexpr (std::is_same_v<std::decay_t<decltype(value)>,
                                 std::string>) {
            return "string: " + value;
        } else {
            return "double: " + std::to_string(value);
        }
    });
    ASSERT_EQ(result1, "int: 42");

    variant = std::string("test");
    auto result2 = variant.visit([](const auto& value) -> std::string {
        if constexpr (std::is_same_v<std::decay_t<decltype(value)>, int>) {
            return "int: " + std::to_string(value);
        } else if constexpr (std::is_same_v<std::decay_t<decltype(value)>,
                                 std::string>) {
            return "string: " + value;
        } else {
            return "double: " + std::to_string(value);
        }
    });
    ASSERT_EQ(result2, "string: test");
}

TEST(get_with_optional) {
    TaggedUnion<int, std::string> variant(42);

    auto* int_ptr = variant.get<int>();
    ASSERT_TRUE(int_ptr != nullptr);
    ASSERT_EQ(*int_ptr, 42);

    auto* str_ptr = variant.get<std::string>();
    ASSERT_TRUE(str_ptr == nullptr);

    variant = std::string("hello");
    int_ptr = variant.get<int>();
    ASSERT_TRUE(int_ptr == nullptr);

    str_ptr = variant.get<std::string>();
    ASSERT_TRUE(str_ptr != nullptr);
    ASSERT_EQ(*str_ptr, std::string("hello"));
}

TEST(free_function_isa) {
    TaggedUnion<int, std::string> variant(42);

    ASSERT_TRUE(lpc::utils::tagged_union::isa<int>(variant));
    ASSERT_FALSE(
        lpc::utils::tagged_union::isa<std::string>(variant));

    variant = std::string("test");
    ASSERT_FALSE(lpc::utils::tagged_union::isa<int>(variant));
    ASSERT_TRUE(
        lpc::utils::tagged_union::isa<std::string>(variant));
}

TEST(constructor_with_value) {
    TaggedUnion<int, std::string> variant1(42);
    ASSERT_FALSE(variant1.valueless());
    ASSERT_EQ(variant1.index(), static_cast<std::size_t>(0));
    ASSERT_TRUE(variant1.isa<int>());
    ASSERT_EQ(*variant1.get_unchecked<int>(), 42);

    TaggedUnion<int, std::string> variant2(std::string("hello"));
    ASSERT_FALSE(variant2.valueless());
    ASSERT_EQ(variant2.index(), static_cast<std::size_t>(1));
    ASSERT_TRUE(variant2.isa<std::string>());
    ASSERT_EQ(*variant2.get_unchecked<std::string>(), std::string("hello"));
}

auto main() -> int {
    return RUN_TESTS();
}
