
#ifndef lmgc2clipper_h
#define lmgc2clipper_h

extern "C" void clipper_intersection(double *p1, int n1, double *p2, int n2, double shrink1, double shrink2, double delta, double **p3, int **n3, int & size_n3, double ** area);

#endif // lmgc2clipper_h
