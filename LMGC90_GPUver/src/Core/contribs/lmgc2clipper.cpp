
#include "clipper.hpp"
#include <cmath>

using namespace ClipperLib;

extern "C" void clipper_intersection(double *p1, int n1, double *p2, int n2, double shrink1, double shrink2, double delta, double **p3, int **n3, int & size_n3, double ** area)
{
    int    i, j, ii, nn;
    Path   path1, path2;
    Paths  poly1, poly2, solution;

    Clipper       clipper;
    ClipperOffset offset ;

    // to map from double to int
    double scale = std::pow(double(10.), double(10.));

    for (i = 0; i < n1; i++)
    {
        path1.push_back(IntPoint((cInt)(p1[2*i] * scale),(cInt)(p1[2*i+1] * scale)));
    };

    offset.Clear();
    offset.AddPath(path1, jtSquare, etClosedPolygon);
    offset.Execute(poly1, -1.*scale*shrink1);

    //SimplifyPolygons(poly1, pftEvenOdd);
    //CleanPolygons(poly1, 2.*delta);

    for (i = 0; i < n2; i++)
    {
        path2.push_back(IntPoint((cInt)(p2[2*i] * scale),(cInt)(p2[2*i+1] * scale)));
    };

    offset.Clear();
    offset.AddPath(path2, jtSquare, etClosedPolygon);
    offset.Execute(poly2, -1.*scale*shrink2);

    //SimplifyPolygons(poly2, pftEvenOdd);
    //CleanPolygons(poly2, 2.*delta);

    clipper.Clear();
    clipper.AddPaths(poly1, ptSubject, true);
    clipper.AddPaths(poly2, ptClip   , true);

    clipper.Execute(ctIntersection, solution, pftEvenOdd, pftEvenOdd);

    // delta : parametre de simplification
    delta = delta * scale;
    CleanPolygons(solution, delta);

    double sum_area = 0;
    int    sum_n3   = 0;
    size_n3  = solution.size();
    if (solution.size() > 0)
    {
        *n3      = new int [ size_n3 ];
        *area    = new double [ size_n3 ];

        for (i = 0; i < solution.size(); ++i)
        {
            (*n3)[i]   = solution[i].size();
            (*area)[i] = Area(solution[i]) / (scale*scale);

            sum_n3   += (*n3)[i];
            sum_area += (*area)[i];
        };

        if (sum_area > 0.)
        {
            *p3 = new double [ 2*sum_n3 ];

            ii = 0;
            for (i = 0; i < solution.size(); ++i)
            {
                nn = solution[i].size();
                for (j = 0; j < nn; ++j)
                {
                    (*p3)[2*ii]   = solution[i][j].X / scale;
                    (*p3)[2*ii+1] = solution[i][j].Y / scale;
                    ++ii;
                };
            };
        };
    };
} 
