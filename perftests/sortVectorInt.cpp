#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <chrono>
#include <functional>

std::mt19937 rng(1337);
std::uniform_int_distribution<int> dist(0, 1e6);
auto random_int = std::bind(dist, std::ref(rng));

void sort_vector_int(int n)
{
   std::vector<int> v(n);
   std::generate(v.begin(), v.end(), random_int);

   auto start = std::chrono::high_resolution_clock::now();
   std::sort(v.begin(), v.end());
   auto finish = std::chrono::high_resolution_clock::now();

   std::chrono::duration<double> diff = finish-start;
   std::cout << "Sorted std::vector<int>: " << n << " elements in " << diff.count() << " s\n";
}

int main()
{
   sort_vector_int(10000);
   sort_vector_int(100000);
   sort_vector_int(1000000);
   sort_vector_int(10000000);
   sort_vector_int(100000000);
}
