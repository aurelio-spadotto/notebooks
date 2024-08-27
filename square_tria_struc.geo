SetFactory("OpenCASCADE");

N = 161;
h = 1/N;
// Define the square geometry
Point(1) = {-0.5, -0.5, 0, h};
Point(2) = {0.5, -0.5, 0, h};
Point(3) = {0.5, 0.5, 0, h};
Point(4) = {-0.5, 0.5, 0, h};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line Loop(1) = {1, 2, 3, 4};
Plane Surface(1) = {1};

Transfinite Surface {1};

// Generate the mesh
Mesh 2;


