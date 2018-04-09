################################################################################
##
#W  littletest.tst           test for package 'Unipot'             Sergei Haller
##
gap> START_TEST("littletest.tst");
gap> F4 := UnipotChevSubGr("F", 4, Rationals); # Creating the group of type F_4 ...
<Unipotent subgroup of a Chevalley group of type F4 over Rationals>
gap> myX:=function(name, num)
>   local ind;
>   ind := Indeterminate(Rationals, num);
>   SetName(ind, name);
>   ASS_GVAR(name, ind);
> end;;
gap> myX("a",  2000); # a
gap> myX("A",  2001); # \alpha
gap> myX("b",  2002); # b
gap> myX("B",  2003); # \beta
gap> myX("B1", 2004); # \beta^-1
gap> myX("u",  2005); # u
gap> myX("uu", 2006); # \bar{u}
gap> myX("v",  2007); # v
gap> myX("vv", 2008); # \bar{v}
gap> myX("x",  2009); # x
gap> myX("xx", 2010); # \bar{x}
gap> myX("y",  2011); # y
gap> myX("yy", 2012); # \bar{y}

#
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
[ 0, 0, 2, -1 
 ]}( A*B^2*B1^2*u^2*yy-2*A^2*B1*v*vv*xx+A^2*vv^2*y-2*A*b*B1*u*vv+a*A*xx ) * x_\
{
[ 0, -1, 0, 1 
 ]}( 2*A^2*B^2*B1*v*vv*yy+2*A*B^2*B1*u*uu*yy-a*A*B^2*yy+A^2*vv^2*x-2*A*b*uu*vv\
+A*uu^2*xx ) * x_{
[ 0, 1, 0, 0 
 ]}( -A^2*B^2*B1^2*v^2*xx*yy-2*A*b*B^2*B1^2*u*v*yy-A*B^2*B1^2*u^2*x*yy-2*A^2*B\
1*v*vv*x*xx+a*A*B^2*y*yy+A^2*vv^2*x*y+2*A*b^2*B1*v*vv-2*A*B1*u*uu*x*xx-2*A*b*u\
u*vv*y+A*uu^2*xx*y+2*b^2*B1*u*uu+a*A*x*xx-a*b^2 )
gap> Comm(u2,u3,"canonical");
<identity>
gap> Comm(u2,u4,"canonical");
x_{[ 2, 1, 0, -1 ]}( 4*B1^2*u*v ) * x_{[ 1, 0, 0, 0 ]}( -2*B1*u*uu ) * x_{
[ 0, 0, 2, -1 ]}( -4*A*B1*u*vv )
gap> Comm(u3,u4,"canonical");
<identity>
gap> STOP_TEST( "littletest.tst", 500000000 );
