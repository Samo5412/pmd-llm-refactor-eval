public void exampleCyclomatic(int x) {
  int result = 0;
  if (x > 0) {         // +1 CC
    result = 1;
  } else if (x == 0) { // +1 CC
    result = 0;
  } else {
    result = -1;
  }
}