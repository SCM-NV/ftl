// Copyright (c) 2016, 2017  Robert Rüger
//
// This file is part of of the Fortran Template Library.
//
// The Fortran Template Library is free software: you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The Fortran Template Library is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
// General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along
// with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.


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
