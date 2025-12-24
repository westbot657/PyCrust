
class TestClass:
    def __init__(self, a: str, b: int, c: float, d: dict[str, int], e: list[float]):
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.e = e

        print([0x_FF, 0b_01, 0o_67, 1., .1, 1e1, 1e1_1, 1_1e1, 15j, 1.2e+10, 1.2e-10, 1.e1])

    def __repr__(self):
        return "test class\UF112F313ad\u1Ee798347\x23794\8\0123\767\N{POTATO}"

def main():
    t = TestClass("Hello!", 123_456, 123_456.789_0, {"idk": 4}, [2.3, 4.5, 6.0, .3])
    print(t)

if __name__ == "__main__":
    main()