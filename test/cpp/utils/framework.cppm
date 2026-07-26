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
        int test_count = 0;

    public:
        void start_test(const std::string& name) {
            current_test = name;
            test_count++;
        }

        void assert_true(bool condition, const std::string& message = "") {
            results.push_back({ current_test, condition, message });
            if (!condition) {
                std::cout << "  FAIL: " << current_test << " - " << message
                          << '\n';
            }
        }

        void assert_eq(
            auto expected, auto actual, const std::string& message = "") {
            bool condition = (expected == actual);
            std::stringstream ss;
            if (!message.empty())
                ss << message << " - ";
            ss << "Expected: " << expected << ", Actual: " << actual;
            results.push_back({ current_test, condition, ss.str() });
            if (!condition) {
                std::cout << "  FAIL: " << current_test << " - " << ss.str()
                          << '\n';
            }
        }

        int run_summary() {
            int passed = 0;
            int failed = 0;
            std::set<std::string> failed_tests;

            for (const auto& result : results) {
                if (result.passed)
                    passed++;
                else {
                    failed++;
                    failed_tests.insert(result.name);
                }
            }

            std::cout << "\n=== Test Summary ===" << '\n';
            std::cout << "Tests run:       " << test_count << '\n';
            std::cout << "Assertions:      " << (passed + failed) << '\n';
            std::cout << "  Passed:        " << passed << '\n';
            std::cout << "  Failed:        " << failed << '\n';

            if (!failed_tests.empty()) {
                std::cout << "\nFailed tests:" << '\n';
                for (const auto& name : failed_tests)
                    std::cout << "  - " << name << '\n';
            }

            std::cout << '\n'
                      << (failed == 0 ? "ALL TESTS PASSED" : "SOME TESTS FAILED")
                      << '\n';

            return failed == 0 ? 0 : 1;
        }
    };

    inline TestRunner& get_runner() {
        static TestRunner runner;
        return runner;
    }
}
