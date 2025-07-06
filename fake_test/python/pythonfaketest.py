import math
import sys
import json
import unittest

# 1. Linear function: f(x) = 3x + 2
def linear(x):
    return 3 * x + 2

# 2. 2D vector rotation (CCW)
def rotate_vector(x, y, theta_rad):
    cos_t = math.cos(theta_rad)
    sin_t = math.sin(theta_rad)
    return (
        round(x * cos_t - y * sin_t, 6),
        round(x * sin_t + y * cos_t, 6)
    )

# 3. Dictionary structure 
def is_valid_user(user_dict):
    return (
        isinstance(user_dict, dict) and
        "name" in user_dict and
        "age" in user_dict and
        isinstance(user_dict["name"], str) and
        isinstance(user_dict["age"], int)
    )


def main():
    if len(sys.argv) < 2:
        print("Usage: pythonfaketest.py <command>", file=sys.stderr)
        sys.exit(1)

    cmd = sys.argv[1]
    input_data = sys.stdin.read().strip()

    if cmd == "TEST_linear":
        x = int(input_data)
        print(linear(x))
    elif cmd == "TEST_rotate_vector":
        parts = input_data.split()
        x, y, theta = float(parts[0]), float(parts[1]), float(parts[2])
        rx, ry = rotate_vector(x, y, theta)
        print(f"{rx} {ry}")
    elif cmd == "TEST_is_valid_user":
        user_dict = json.loads(input_data)
        print(str(is_valid_user(user_dict)))
    elif cmd == "unittest":
        unittest.main(argv=[sys.argv[0]])
    else:
        print(f"Unknown command: {cmd}", file=sys.stderr)
        sys.exit(1)

# ---------------- UNIT TESTS ------------------

class TestMathUtils(unittest.TestCase):
    def test_linear(self):
        self.assertEqual(linear(1), 5)
        self.assertEqual(linear(0), 2)
        self.assertEqual(linear(-1), -1)

    def test_rotate_vector(self):
        self.assertEqual(rotate_vector(1, 0, math.pi / 2), (0.0, 1.0))
        self.assertEqual(rotate_vector(0, 1, math.pi), (0.0, -1.0))
        self.assertEqual(rotate_vector(1, 1, 0), (1.0, 1.0))

    def test_is_valid_user(self):
        self.assertTrue(is_valid_user({"name": "Alice", "age": 30}))
        self.assertFalse(is_valid_user({"name": "Bob"}))
        self.assertFalse(is_valid_user({"name": 123, "age": "old"}))

if __name__ == "__main__":
    main()
