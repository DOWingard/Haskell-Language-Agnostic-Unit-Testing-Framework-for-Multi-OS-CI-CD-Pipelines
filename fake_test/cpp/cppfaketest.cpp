#include <iostream>
#include <sstream>
#include <vector>
#include <string>

// Test Addition
int add(int a, int b) {
    return a + b;
}

// Test string
bool is_palindrome(const std::string& s) {
    int n = s.size();
    for (int i = 0; i < n / 2; ++i)
        if (s[i] != s[n - i - 1])
            return false;
    return true;
}

// Test Factorial
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

// Updated main:

int main() {
    std::string line;
    if (!std::getline(std::cin, line)) {
        std::cerr << "No input given" << std::endl;
        return 1;
    }

    std::istringstream iss(line);
    std::vector<std::string> args;
    std::string token;
    while (iss >> token) args.push_back(token);

    if (args.empty()) {
        std::cerr << "No command given" << std::endl;
        return 1;
    }

    std::string test = args[0];

    if (test == "add") {
        if (args.size() != 3) {
            std::cerr << "Usage: add <int1> <int2>" << std::endl;
            return 1;
        }
        int a = std::stoi(args[1]);
        int b = std::stoi(args[2]);
        std::cout << add(a, b) << "\n";
    }
    else if (test == "palindrome") {
        if (args.size() != 2) {
            std::cerr << "Usage: palindrome <string>" << std::endl;
            return 1;
        }
        std::string s = args[1];
        std::cout << (is_palindrome(s) ? "true" : "false") << "\n";
    }
    else if (test == "factorial") {
        if (args.size() != 2) {
            std::cerr << "Usage: factorial <int>" << std::endl;
            return 1;
        }
        int n = std::stoi(args[1]);
        std::cout << factorial(n) << "\n";
    }
    else {
        std::cerr << "Unknown test: " << test << std::endl;
        return 1;
    }

    return 0;
}
