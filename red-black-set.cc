// Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved.

#include <cstdlib>
#include <ctime>
#include <set>

int main() {
  std::srand(std::time(nullptr));
  std::set<int> s;
  for (int i = 0; i < (1 << 20); ++i) {
    s.insert(std::rand());
  }
  return 0;
}
