################################################################################
##
#W  littletest.tst           test for package 'Unipot'             Sergei Haller
##
#H  @(#)$Id: littletest.tst,v 1.6 2004/11/16 16:41:32 gc1007 Exp $
##

gap> START_TEST("Just a little test for package unipot\n$Id: littletest.tst,v 1.6 2004/11/16 16:41:32 gc1007 Exp $");
gap> ss := SizeScreen();;
gap> SizeScreen( [80,ss[2]] );;
gap> LoadPackage("unipot", "1.2", false);
true
gap> F4 := UnipotChevSubGr("F", 4, Rationals); # Creating the group of type F_4 ...
<Unipotent subgroup of a Chevalley group of type F4 over Rationals>
gap> indets := [];
[  ]
gap> a  := Indeterminate(Rationals, "a",  indets);; # a
gap> Add(indets,last);
gap> A  := Indeterminate(Rationals, "A",  indets);; # \alpha
gap> Add(indets,last);
gap> b  := Indeterminate(Rationals, "b",  indets);; # b
gap> Add(indets,last);
gap> B  := Indeterminate(Rationals, "B",  indets);; # \beta
gap> Add(indets,last);
gap> B1 := Indeterminate(Rationals, "B1", indets);; # \beta^-1
gap> Add(indets,last);
gap> u  := Indeterminate(Rationals, "u",  indets);; # u
gap> Add(indets,last);
gap> uu := Indeterminate(Rationals, "uu", indets);; # \bar{u}
gap> Add(indets,last);
gap> v  := Indeterminate(Rationals, "v",  indets);; # v
gap> Add(indets,last);
gap> vv := Indeterminate(Rationals, "vv", indets);; # \bar{v}
gap> Add(indets,last);
gap> x  := Indeterminate(Rationals, "x",  indets);; # x
gap> Add(indets,last);
gap> xx := Indeterminate(Rationals, "xx", indets);; # \bar{x}
gap> Add(indets,last);
gap> y  := Indeterminate(Rationals, "y",  indets);; # y
gap> Add(indets,last);
gap> yy := Indeterminate(Rationals, "yy", indets);; # \bar{y}
gap> Add(indets,last);

gap> M := [
>           [ 1/2, -1/2, -1/2, -1/2 ],       # 4
>           [  0,    1,   -1,    0  ],       # 1   o---o=>=o---o
>           [  0,    0,    0,    1  ],       # 3   2   4   3   1
>           [  0,    0,    1,   -1  ],       # 2
>         ];;
gap>  
gap> N := [
>           [  2,  0, -1,  0 ],
>           [  0,  2,  0, -1 ],
>           [ -1,  0,  2, -1 ],
>           [  0, -1, -2,  2 ],
>         ];;
gap>  
gap> M1 := M^-1;;
gap> N1 := N^-1;;                                                                                                 
gap> u2 := UnipotChevElemByRoots( F4, 
>              [[ 1, 1,-1,-1]*M1*N/2,
>               [ 1, 1, 1, 1]*M1*N/2,
>               [ 1, 1,-1, 1]*M1*N/2,
>               [ 1, 1, 1,-1]*M1*N/2,
>               [ 1, 1, 0, 0]*M1*N  ], [B1*v, A*vv, B1*u, uu, a ]);
x_{[ 1, 1, -1, 0 ]}( B1*v ) * x_{[ -1, 0, 1, 0 ]}( A*vv ) * x_{
[ 0, 1, 1, -1 ]}( B1*u ) * x_{[ 0, 0, -1, 1 ]}( uu ) * x_{[ 0, 1, 0, 0 ]}( a )
gap> u4 := UnipotChevElemByRoots( F4,
>              [[ 1,-1,-1,-1]*M1*N/2,
>               [ 1,-1, 1, 1]*M1*N/2,
>               [ 1,-1,-1, 1]*M1*N/2,
>               [ 1,-1, 1,-1]*M1*N/2,
>               [ 1,-1, 0, 0]*M1*N  ], [ B1*v, A*vv, B1*u, uu, a ]);
x_{[ 2, 0, -1, 0 ]}( B1*v ) * x_{[ 0, -1, 1, 0 ]}( A*vv ) * x_{
[ 1, 0, 1, -1 ]}( B1*u ) * x_{[ 1, -1, -1, 1 ]}( uu ) * x_{
[ 2, -1, 0, 0 ]}( a )
gap>  
gap> u1 := UnipotChevElemByRoots( F4,
>      [[ 0, 1,-1, 0]*M1*N,  
>       [ 0, 1, 1, 0]*M1*N,  
>       [ 0, 1, 0,-1]*M1*N,  
>       [ 0, 1, 0, 1]*M1*N,  
>       [ 0, 1, 0, 0]*M1*N ], [ y, A*B^2*yy, x, A*xx, b ]);
x_{[ 0, 2, 0, -1 ]}( y ) * x_{[ -2, 0, 0, 1 ]}( A*B^2*yy ) * x_{
[ 0, 1, -2, 1 ]}( x ) * x_{[ -2, 1, 2, -1 ]}( A*xx ) * x_{
[ -1, 1, 0, 0 ]}( b )
gap>  
gap> u3 := UnipotChevElemByRoots( F4,
>      [[ 1, 0,-1, 0]*M1*N,  
>       [ 1, 0, 1, 0]*M1*N,  
>       [ 1, 0, 0,-1]*M1*N,  
>       [ 1, 0, 0, 1]*M1*N,  
>       [ 1, 0, 0, 0]*M1*N ], [ y, A*B^2*yy, x, A*xx, b ]);
x_{[ 2, 1, 0, -1 ]}( y ) * x_{[ 0, -1, 0, 1 ]}( A*B^2*yy ) * x_{
[ 2, 0, -2, 1 ]}( x ) * x_{[ 0, 0, 2, -1 ]}( A*xx ) * x_{[ 1, 0, 0, 0 ]}( b )
gap> Comm(u1,u2, "canonical");
<identity>
gap> Comm(u1,u3, "canonical");
x_{[ 0, 1, 0, 0 ]}( 2*A*B^2*y*yy-2*A*x*xx+2*b^2 )
gap> Comm(u1,u4,"canonical");
x_{[ 1, 1, -1, 0 ]}( b*B1*v+B1*u*x-uu*y ) * x_{
[ 0, 1, 1, -1 ]}( A*B1*v*xx-A*vv*y+b*B1*u ) * x_{
[ 0, 0, -1, 1 ]}( A*B^2*B1*v*yy+A*vv*x-b*uu ) * x_{
[ 2, 1, 0, -1 ]}( A*B1^2*v^2*xx+2*b*B1^2*u*v+B1^2*u^2*x-a*y ) * x_{
[ -1, 0, 1, 0 ]}( -A*B^2*B1*u*yy+A*b*vv-A*uu*xx ) * x_{
[ 2, 0, -2, 1 ]}( A*B^2*B1^2*v^2*yy-2*b*B1*uu*v-2*B1*u*uu*x+uu^2*y+a*x ) * x_{
[ 1, 0, 0, 0
 ]}( -A*B^2*B1^2*u*v*yy-A*b*B1*v*vv-A*B1*u*vv*x-A*B1*uu*v*xx+A*uu*vv*y-b*B1*u*\
uu+a*b ) * x_{
[ 0, 0, 2, -1 ]}( A*B^2*B1^2*u^2*yy-2*A^2*B1*v*vv*xx+A^2*vv^2*y-2*A*b*B1*u*vv+\
a*A*xx ) * x_{
[ 0, -1, 0, 1 ]}( 2*A^2*B^2*B1*v*vv*yy+2*A*B^2*B1*u*uu*yy-a*A*B^2*yy+A^2*vv^2*\
x-2*A*b*uu*vv+A*uu^2*xx ) * x_{
[ 0, 1, 0, 0 ]}( -A^2*B^2*B1^2*v^2*xx*yy-2*A*b*B^2*B1^2*u*v*yy-A*B^2*B1^2*u^2*\
x*yy-2*A^2*B1*v*vv*x*xx+a*A*B^2*y*yy+A^2*vv^2*x*y+2*A*b^2*B1*v*vv-2*A*B1*u*uu*\
x*xx-2*A*b*uu*vv*y+A*uu^2*xx*y+2*b^2*B1*u*uu+a*A*x*xx-a*b^2 )
gap> Comm(u2,u3,"canonical");
<identity>
gap> Comm(u2,u4,"canonical");
x_{[ 2, 1, 0, -1 ]}( 4*B1^2*u*v ) * x_{[ 1, 0, 0, 0 ]}( -2*B1*u*uu ) * x_{
[ 0, 0, 2, -1 ]}( -4*A*B1*u*vv )
gap> Comm(u3,u4,"canonical");
<identity>
gap> SizeScreen( ss );;
gap> STOP_TEST( "littletest.tst", 500000000 );
