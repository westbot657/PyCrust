
class TestClass:
    def __init__(self, a: str, b: int, c: float, d: dict[str, int], e: list[float]):
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.e = e

    def __repr__(self):
        return "test class"

def main():
    t = TestClass("Hello!", 123_456, 123_456.789_0, {"idk": 4}, [2.3, 4.5, 6.0, .3])
    print(t)

if __name__ == "__main__":
    main()