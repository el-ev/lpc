#ifndef LPC_TEST_CPP_UTILS_MACROS_HPP
#define LPC_TEST_CPP_UTILS_MACROS_HPP

#define TEST(name)                                                             \
    void test_##name();                                                        \
    struct TestRegistrar_##name {                                              \
        TestRegistrar_##name() {                                               \
            lpc::test::get_runner().start_test(#name);                         \
            test_##name();                                                     \
        }                                                                      \
    };                                                                         \
    static TestRegistrar_##name registrar_##name;                              \
    void test_##name()

#define RUN_TESTS() lpc::test::get_runner().run_summary()

#define ASSERT_TRUE(condition)                                                 \
    lpc::test::get_runner().assert_true(condition, #condition)

#define ASSERT_FALSE(condition)                                                \
    lpc::test::get_runner().assert_true(!(condition), #condition " is false")

#define ASSERT_EQ(expected, actual)                                            \
    lpc::test::get_runner().assert_eq(                                         \
        expected, actual, #expected " == " #actual)

#define ASSERT_NE(expected, actual)                                            \
    lpc::test::get_runner().assert_true(                                       \
        expected != actual, #expected " != " #actual)

#define ASSERT_GT(lhs, rhs)                                                    \
    lpc::test::get_runner().assert_true(                                       \
        lhs > rhs, #lhs " > " #rhs)

#define ASSERT_THROW(expr, exception_type)                                     \
    try {                                                                      \
        expr;                                                                  \
        lpc::test::get_runner().assert_true(false,                             \
            "Expected exception " #exception_type " but none was thrown");     \
    } catch (const exception_type&) {                                          \
        lpc::test::get_runner().assert_true(true);                             \
    } catch (...) {                                                            \
        lpc::test::get_runner().assert_true(false,                             \
            "Expected exception " #exception_type " but got a different one"); \
    }

#endif // LPC_TEST_CPP_UTILS_MACROS_HPP