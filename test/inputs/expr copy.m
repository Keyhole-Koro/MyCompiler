int add(int a, int b) {
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

int main() {
    int x = 9;
    int y = add(x, 1);
    y = sub(y, 2);
    y = add(y, 3);
    return 0;
}
