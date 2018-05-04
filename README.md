double square_root(int num) {
  double x1 = (num * 1.0) / 2;
  double x2= (x1 + (num / x1)) / 2;
  while(abs(x1 - x2) >= 0.0000001) {
    x1 = x2;
    x2 = (x1 + (num / x1)) / 2;
  }
  return x2;
}