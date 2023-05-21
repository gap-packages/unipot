################################################################################
##
#W  rootsys.gi         Some additional functionality for          Sergei Haller
##                               Root Systems
##
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##

############################################################################
##
#M  PositiveRootsFC( <R> )
#M  NegativeRootsFC( <R> )
##

InstallMethod( PositiveRootsFC,
     "for a root system",
     true, [ IsRootSystem ], 0,
     function( R )

          local posR,V,B;

          posR := PositiveRoots( R );
          V    := VectorSpace( Rationals, SimpleSystem(R) );
          B    := Basis( V, SimpleSystem(R) );

          return List( posR, r -> Coefficients( B, r ) );

     end
);

############################################################################
##
#M  NegativeRootsFC( <R> )
##

InstallMethod( NegativeRootsFC,
    "for a root system",
    true, [ IsRootSystem ], 0,
    R -> -PositiveRootsFC(R)
);

############################################################################
##
#M  \.( <R>, <name> ) . . . . . . . record component access for a root system
##
##
#T  These are for compatibility only and have to be removed.
##
##

InstallMethod( \., 
        "for a root system and a record component",
        true, [ IsRootSystem, IsObject ], 0, 
        function( R, name )
    
    name:= NameRNam( name );
    if name = "posroots" then
        return PositiveRoots(R);
    elif name = "posrootsFC" then
        return PositiveRootsFC(R);;
    else
        TryNextMethod();
    fi;
end );
