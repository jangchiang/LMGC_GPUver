import sys
import math
#from pathlib import Path

import numpy as np

import utils 


def A_end(radius=1., length=3., nb_steps=600, no_test=False):

    test_name = 'A_end'

    alt    = 3*radius
    c1 = [-length*1.2, 0., alt]
    c2 = [ length*1.2, 0., alt]
    c3 = [        0. , 0., 0. ]
    
    a1 = 0.
    a2 = math.pi/2.
    
    cylinders = ( (radius, length*0.5, c1, a1, a2, False,),
                  (radius, length*0.5, c2, a1, a2, False,),
                  (radius, length*0.5, c3, a1, a2, True ,),
                )
    
   
    #wd = Path('./A_end')
    wd = './'+test_name
    utils.generate(cylinders, 0.1*radius, wd)
    inters = utils.compute(nb_steps, str(wd))

    ref = np.array([[-1.89067615e+00,  0.00000000e+00,  9.20529141e-01,
                     -9.20528334e-01,  0.00000000e+00, -3.90675808e-01,
                     -3.90675808e-01,  0.00000000e+00,  9.20528334e-01,
                      0.00000000e+00,  1.00000000e+00,  0.00000000e+00,
                      6.50853649e+03,  2.16951216e+04,  0.00000000e+00,
                     -2.24892200e-01, -1.75284041e-03,  0.00000000e+00,
                      2.11758237e-22,                                  ],
                    [ 1.89067615e+00,  0.00000000e+00,  9.20529141e-01,
                     -9.20528334e-01,  0.00000000e+00,  3.90675808e-01,
                      3.90675808e-01,  0.00000000e+00,  9.20528334e-01,
                      0.00000000e+00,  1.00000000e+00, -0.00000000e+00,
                     -6.50853649e+03,  2.16951216e+04,  0.00000000e+00,
                      2.24892200e-01, -1.75284041e-03,  0.00000000e+00,
                      2.11758237e-22,                                  ]],
                    dtype=float )

    if no_test:
        print( 'result of '+test_name)
        print( inters )
    else:
        assert np.allclose(inters,ref ), 'wrong inters result in '+test_name

def A_inside(radius=1., length=2., nb_steps=500, no_test=False):

    test_name = 'A_inside'

    alt    = 3.*radius
    c1 = [0., 0., alt]
    c2 = [0., 0., 0.]

    a1 = 0.
    a2 = math.pi/2.

    cylinders = ( (radius,   length*0.5, c1, a1, a2, False,),
                  (radius, 3*length*0.5, c2, a1, a2, True ,),
                )

    #wd = Path('./A_inside')
    wd = './'+test_name
    utils.generate(cylinders, 0.1*radius, wd)
    inters = utils.compute(nb_steps, str(wd))

    ref = np.array([[-1.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                      0.00000000e+00, -1.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                     -1.21154264e-12,  5.13650404e+04,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                     -6.33361141e-11,                                  ],
                    [ 1.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                      0.00000000e+00, -1.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                     -5.94272621e-13,  5.13650404e+04,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                     -2.22048074e-16,                                  ]],
                    dtype=float )

    idx = list(range(19))
    _ = idx.pop(14) # remove rls
    idx = np.array( idx )

    if no_test:
        print( 'result of '+test_name)
        print( inters )
    else:
        assert np.allclose(inters[:,idx],ref,atol=1e-6), 'wrong inters result in '+test_name


def A_mixte(radius=1., length=3., nb_steps=450, no_test=False):

    test_name = 'A_mixte'

    alt    = 3.*radius
    c1 = [-length*1.2, 0., alt]
    c2 = [ length*1.2, 0., alt]
    c3 = [        0. , 0., 0. ]

    a1 = 0.
    a2 = math.pi/2.

    cylinders = ( (radius,  length*0.5, c1, a1, a2, False,),
                  (radius,  length*0.5, c2, a1, a2, False,),
                  (radius,2*length*0.5, c3, a1, a2, True ,),
                )

    #wd = Path('./A_mixte')
    wd = './'+test_name
    utils.generate(cylinders, 0.1*radius, wd)
    inters = utils.compute(nb_steps, str(wd))

    ref = np.array([[-2.10000000e+00,  0.00000000e+00,  1.00447238e+00,
                      0.00000000e+00, -1.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00, -4.41450000e+00,  0.00000000e+00,
                      4.53025000e-03,                                   ],
                    [-3.00000000e+00,  0.00000000e+00,  1.00447238e+00,
                      0.00000000e+00, -1.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00, -4.41450000e+00,  0.00000000e+00,
                      4.53025000e-03,                                   ],
                    [ 2.10000000e+00,  0.00000000e+00,  1.00447238e+00,
                      2.21055960e-16, -1.00000000e+00,  0.00000000e+00,
                      2.21055960e-16,  0.00000000e+00,  1.00000000e+00,
                     -1.00000000e+00, -2.21055960e-16,  2.21055960e-16,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00, -4.41450000e+00, -9.75851534e-16,
                      4.53025000e-03,                                   ],
                    [ 3.00000000e+00,  0.00000000e+00,  1.00447238e+00,
                      0.00000000e+00, -1.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  1.00000000e+00,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00, -4.41450000e+00,  0.00000000e+00,
                      4.53025000e-03,                                   ]],
                    dtype=float )

    if no_test:
        print( 'result of '+test_name)
        print( inters )
    else:
        assert np.allclose(inters,ref), 'wrong inters result in '+test_name


def P_end(r=1., l=2., nb_steps=500, no_test=False):

    test_name = 'P_end'

    alt = 3.*r
    a1 = 0.
    a2 = math.pi/2.
    cylinders = ( (r,  l*0.5, [ 3.5, -1.1, alt], a2, a1, False,),
                  (r,  l*0.5, [-3.5, -1.1, alt], a2, a1, False,),
                  (r,3*l*0.5, [ 0.0,  0.0, 0. ], a1, a2, True ,),
                  (r,  l*0.5, [ 3.5,  7.1, alt], a2, a1, False,),
                  (r,  l*0.5, [-3.5,  7.1, alt], a2, a1, False,),
                  (r,3*l*0.5, [ 0.0,  6.0, 0. ], a1, a2, True ,),
                )

    #wd = Path('./P_end')
    wd = './'+test_name
    utils.generate(cylinders, 0.1*r, wd)
    inters = utils.compute(nb_steps, str(wd))

    ref = np.array([[ 3.26109774e+00, -1.02603601e-01,  9.59786424e-01,
                      0.00000000e+00, -9.94334438e-01, -1.06296871e-01,
                      2.61112169e-01, -1.02609270e-01,  9.59839452e-01,
                     -9.65308466e-01, -2.77554066e-02,  2.59632822e-01,
                      2.94879081e+04,  9.97325360e+04,  5.06511261e+03,
                     -2.42834926e+00, -3.46944695e-18, -4.17115464e-01,
                     -1.10493203e-04,                                   ],
                    [-3.26109774e+00, -1.02603601e-01,  9.59786424e-01,
                      0.00000000e+00, -9.94334438e-01, -1.06296871e-01,
                     -2.61112169e-01, -1.02609270e-01,  9.59839452e-01,
                     -9.65308466e-01,  2.77554066e-02, -2.59632822e-01,
                      2.94879081e+04,  9.97325360e+04, -5.06511261e+03,
                     -2.42834926e+00, -3.46944695e-18,  4.17115464e-01,
                     -1.10493203e-04,                                   ],
                    [ 3.26109774e+00,  6.10260360e+00,  9.59786424e-01,
                      0.00000000e+00, -9.94334438e-01,  1.06296871e-01,
                      2.61112169e-01,  1.02609270e-01,  9.59839452e-01,
                     -9.65308466e-01,  2.77554066e-02,  2.59632822e-01,
                     -2.94879081e+04,  9.97325360e+04,  5.06511261e+03,
                      2.42834926e+00,  0.00000000e+00, -4.17115464e-01,
                     -1.10493203e-04,                                   ],
                    [-3.26109774e+00,  6.10260360e+00,  9.59786424e-01,
                      0.00000000e+00, -9.94334438e-01,  1.06296871e-01,
                     -2.61112169e-01,  1.02609270e-01,  9.59839452e-01,
                     -9.65308466e-01, -2.77554066e-02, -2.59632822e-01,
                     -2.94879081e+04,  9.97325360e+04, -5.06511261e+03,
                      2.42834926e+00,  3.46944695e-18,  4.17115464e-01,
                     -1.10493203e-04,                                   ]],
                    dtype=float )

    if no_test:
        print( 'result of '+test_name)
        print( inters )
    else:
        assert np.allclose(inters,ref), 'wrong inters result in '+test_name

def P_inside(r=1., l=2., nb_steps=500, no_test=False):

    test_name = 'P_inside'

    alt = 3.*r
    a1 = 0.
    a2 = math.pi/2.
    cylinders = ( (r,  l*0.5, [ 0. , 0., alt], a2, a1, False,),
                  (r,  l*0.5, [-2.5, 1., alt], a2, a1, False,),
                  (r,  l*0.5, [ 2.5,-1., alt], a2, a1, False,),
                  (r,3*l*0.5, [ 0. , 0., 0. ], a1, a2, True ,),
                )

    #wd = Path('./P_inside')
    wd = './'+test_name
    utils.generate(cylinders, 0.1*r, wd)
    inters = utils.compute(nb_steps, str(wd))

    ref = np.array(
                   [[ 5.06924469e-17,  5.10861014e-17,  1.00000000e+00,
                     -6.12323400e-17, -1.00000000e+00,  5.10861014e-17,
                     -6.12323400e-17,  5.10861014e-17,  1.00000000e+00,
                     -1.00000000e+00,  6.12323400e-17, -6.12323400e-17,
                      9.90813166e-12,  1.02730081e+05, -1.45163154e-12,
                      0.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      0.00000000e+00,                                   ],
                    [-2.50000000e+00,  1.61072361e-01,  9.86727568e-01,
                      0.00000000e+00, -9.86937019e-01,  1.61106552e-01,
                      0.00000000e+00,  1.61106552e-01,  9.86937019e-01,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                     -5.17836634e+04,  1.72612211e+05,  0.00000000e+00,
                      2.71486888e+00,  0.00000000e+00,  0.00000000e+00,
                     -4.24446672e-04,                                   ],
                    [ 2.50000000e+00, -1.61072361e-01,  9.86727568e-01,
                     -0.00000000e+00, -9.86937019e-01, -1.61106552e-01,
                     -0.00000000e+00, -1.61106552e-01,  9.86937019e-01,
                     -1.00000000e+00,  0.00000000e+00,  0.00000000e+00,
                      5.17836634e+04,  1.72612211e+05,  0.00000000e+00,
                     -2.71486888e+00,  0.00000000e+00,  0.00000000e+00,
                     -4.24446672e-04,                                   ]],
                    dtype=float )

    if no_test:
        print( 'result of '+test_name)
        print( inters )
    else:
        assert np.allclose(inters,ref), 'wrong inters result in '+test_name


if __name__ == "__main__":

    if '--no-test' in sys.argv:
      no_test = True
    else:
      no_test = False

    A_end(no_test=no_test)
    A_inside(no_test=no_test)
    A_mixte(no_test=no_test)
    P_end(no_test=no_test)
    P_inside(no_test=no_test)

