L = 1;
h = L/40;

Point (1) = {-0.5*L, -0.5*L, 0., h};
Point (2) = {0.5*L, -0.5*L, 0., h};
Point (3) = {0.5*L, 0.5*L, 0., h};
Point (4) = {-0.5*L, 0.5*L, 0., h};

Line (1) = {1,2};
Line (2) = {2,3};
Line (3) = {3,4};
Line (4) = {4,1};

Curve Loop (1) = {1,2,3,4};
Surface (1)    = {1};

Physical Surface ("domain") = {1};

Mesh.MshFileVersion = 2.2;
Mesh 2;















