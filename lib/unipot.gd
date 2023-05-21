################################################################################
##
#W  unipot.gd                   Unipot package                     Sergei Haller
##
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##
##  This is the declaration part of the package
##

################################################################################
##
#C  IsUnipotChevFamily
##
##  Category of families of elements of a unipotent subgroup of a Chevalley 
##  group
##

DeclareCategory("IsUnipotChevFamily", IsFamily);

################################################################################
##
#C  IsUnipotChevElem
##
##  Category of elements of a unipotent subgroup of a Chevalley group.
##

DeclareCategory( "IsUnipotChevElem", IsMultiplicativeElementWithInverse );
DeclareCategoryCollections( "IsUnipotChevElem" );

################################################################################
##
#C  IsUnipotChevSubGr
##
##  Category of unipotent subgroups of a Chevalley group.
##

DeclareCategory( "IsUnipotChevSubGr", IsGroup and IsUnipotChevElemCollection );

################################################################################
##
#F  UnipotChevFamily( <type>, <n>, <F> )
##
##  This function creates a UnipotChevFamily of type <type> and of rank <n>
##  over the ring <F>.
##
##  <type> must be one of A, B, C, D, E, F, G
##  For the types A to D, <n> must be a positive integer.
##  For the type E, <n> must be one of 6, 7, 8.
##  For the type F, <n> must be 4.
##  For the type G, <n> must be 2.
##

DeclareGlobalFunction( "UnipotChevFamily" );

################################################################################
##
#F  UnipotChevSubGr( <type>, <n>, <F> )
##
##  `UnipotChevSubGr' returns the unipotent subgroup $U$ of the Chevalley group
##  of type <type>, rank <n> over the ring <F>.
##
##  <type> must be one of A, B, C, D, E, F, G
##  For the types A to D, <n> must be a positive integer.
##  For the type E, <n> must be one of 6, 7, 8.
##  For the type F, <n> must be 4.
##  For the type G, <n> must be 2.
##

DeclareGlobalFunction( "UnipotChevSubGr" );

################################################################################
##
#O  UnipotChevElem(                    <U>, <list>, <list> )  . . `undocumented'
##
#O  UnipotChevElemByRootNumbers(       <U>, <list>, <list> )
#O  UnipotChevElemByRN(                <U>, <list>, <list> )
##
#O  UnipotChevElemByFundamentalCoeffs( <U>, <list>, <list> )
#O  UnipotChevElemByFC(                <U>, <list>, <list> )
##
#O  UnipotChevElemByRoots(             <U>, <list>, <list> )
#O  UnipotChevElemByR(                 <U>, <list>, <list> )
##
##  Returns an element of a unipotent subgroup of a Chevalley group
##

DeclareOperation( "UnipotChevElem",                    
                         [ IsUnipotChevSubGr, IsRecord, IS_OPERATION ] );
DeclareOperation( "UnipotChevElemByRootNumbers",       
                         [ IsUnipotChevSubGr, IsList, IsList ] );
DeclareOperation( "UnipotChevElemByFundamentalCoeffs", 
                         [ IsUnipotChevSubGr, IsList, IsList ] );
DeclareOperation( "UnipotChevElemByRoots",             
                         [ IsUnipotChevSubGr, IsList, IsList ] );

DeclareSynonym( "UnipotChevElemByRN", UnipotChevElemByRootNumbers       );
DeclareSynonym( "UnipotChevElemByFC", UnipotChevElemByFundamentalCoeffs );
DeclareSynonym( "UnipotChevElemByR" , UnipotChevElemByRoots             );


################################################################################
##
#A  CanonicalForm( <x> )  . . . . . . . . . canonical form of a `UnipotChevElem'
##
##  `CanonicalForm' returns the canonical form of <x>. 
##  For more information on the canonical form see [Car72], Theorem 5.3.3 (ii).
##  

DeclareAttribute( "CanonicalForm",
                    IsUnipotChevElem
	        ) ;


DeclareGlobalFunction( "ChevalleyCommutatorConstant" );


################################################################################
##
#P  IsRootElement( <x> )  . . . . . . . . . . . . . . . . . . for UnipotChevElem
##
##  `IsRootElement' returns `true' if and only if <x> is a <root element>,
##  i.e $<x>=x_{r_i}(t)$ for some root $r_i$.
##
#N  *Note:* the canonical form of <x> may be a root element even if <x> isn't
#N  one.
##

DeclareProperty( "IsRootElement",
                    IsUnipotChevElem
	        ) ;



################################################################################
##
#V  UnipotChevInfo  . . . . . . . . . . . . . . . . . . . . . . . . . InfoClass
##
##  `UnipotChevInfo' is an `InfoClass' used in this package. `InfoLevel' of
##  this `InfoClass' is set to 1 by default.
##

DeclareInfoClass("UnipotChevInfo"); 
SetInfoLevel(UnipotChevInfo, 1);

################################################################################
##
#B  Bibliography
##
#B  [Car72] Roger W. Carter. Simple Groups of Lie Type.
#B          John Wiley \& Sons Ltd., New York, 1972.
#B          Wiley Classics Library Edition Published 1989.
##
################################################################################
