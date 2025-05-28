export module lpc.test.framework;

import std;

export namespace lpc::test {
    struct TestResult {
        std::string name;
        bool passed;
        std::string message;
    };

    class TestRunner {
    private:
        std::vector<TestResult> results;
        std::string current_test;

    public:
        void start_test(const std::string& name) {
            current_test = name;
        }

        void assert_true(bool condition, const std::string& message = "") {
            results.push_back({current_test, condition, message});
            if (!condition) {
                std::cout << "FAIL: " << current_test << " - " << message << '\n';
            }
        }

        void assert_eq(auto expected, auto actual, const std::string& message = "") {
            bool condition = (expected == actual);
            std::stringstream ss;
            if (!message.empty()) ss << message << " - ";
            ss << "Expected: " << expected << ", Actual: " << actual;
            results.push_back({current_test, condition, ss.str()});
            if (!condition) {
                std::cout << "FAIL: " << current_test << " - " << ss.str() << '\n';
            }
        }

        int run_summary() {
            int passed = 0;
            int failed = 0;
            
            for (const auto& result : results) {
                if (result.passed) passed++;
                else failed++;
            }

            std::cout << "\n=== Test Summary ===" << '\n';
            std::cout << "Passed: " << passed << '\n';
            std::cout << "Failed: " << failed << '\n';
            std::cout << "Total:  " << (passed + failed) << '\n';

            return failed == 0 ? 0 : 1;
        }
    };

    inline TestRunner& get_runner() {
        static TestRunner runner;
        return runner;
    }
}

#define TEST(name) \
    void test_##name(); \
    struct TestRegistrar_##name { \
        TestRegistrar_##name() { \
            test_utils::get_runner().start_test(#name); \
            test_##name(); \
        } \
    }; \
    static TestRegistrar_##name registrar_##name; \
    void test_##name()

#define ASSERT_TRUE(condition) \
    test_utils::get_runner().assert_true(condition, #condition)

#define ASSERT_EQ(expected, actual) \
    test_utils::get_runner().assert_eq(expected, actual, #expected " == " #actual)

#define RUN_TESTS() \
    test_utils::get_runner().run_summary()
