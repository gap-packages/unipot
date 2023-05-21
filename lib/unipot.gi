################################################################################
##
#W  unipot.gi                   Unipot package                     Sergei Haller
##
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##
##  This is the implementation part of the package
##


################################################################################
##
#R  IsUnipotChevRepByRootNumbers
#R  IsUnipotChevRepByFundamentalCoeffs
#R  IsUnipotChevRepByRoots
##
##  Different representations for elements of unipotent subgroups.  
##
##  Roots of elements with representation `IsUnipotChevRepByRootNumbers' are
##  represented by their numbers (positions) in `RootSystem(<U>).posroots'.
##
##  Roots of elements with representation `IsUnipotChevRepByFundamentalCoeffs'
##  are represented by coefficients of linear combinations of fundamental roots
##  `RootSystem(<U>).fundroots'.
##
##  Roots of elements with representation `IsUnipotChevRepByRoots' are
##  represented by roots themself.
##

DeclareRepresentation( "IsUnipotChevRepByRootNumbers",
                         IsAttributeStoringRep,
                         [ "roots", "felems"  ] );
DeclareRepresentation( "IsUnipotChevRepByFundamentalCoeffs",
                         IsAttributeStoringRep,
                         [ "roots", "felems"  ] );
DeclareRepresentation( "IsUnipotChevRepByRoots",
                         IsAttributeStoringRep,
                         [ "roots", "felems"  ] );


UNIPOT_DEFAULT_REP := IsUnipotChevRepByRootNumbers;

################################################################################
##
#F  UnipotChevFamily( <type>, <n>, <F> )
##
##  This function creates a UnipotChevFamily of type <type> and of rank <n>
##  over the ring <F>.
##
##  <type> must be one of "A", "B", "C", "D", "E", "F", "G"
##  For the type  "A", <n> must be a positive integer.
##  For the types "B" and "C", <n> must be a positive integer >= 2.
##  For the type  "D", <n> must be a positive integer >= 4.
##  For the type  "E", <n> must be one of 6, 7, 8.
##  For the type  "F", <n> must be 4.
##  For the type  "G", <n> must be 2.
##

InstallGlobalFunction(  UnipotChevFamily,
function( type, n, F )

     local Fam,     # The new Family
           L,       # The SimpleLieAlgebra of the given Type
           R,       # The RootSystem of L
           V,       # Vectorspace used to compute the posrootsFC
           B,       # The Basis of L resp. V
           T,       # StructureConstantsTable(B), taken from Lie Algebra
           N,       # StructureConstantsTable as we need it.
           H,h,     # ``half-the-Cartan-matrix''
           r,s;     # two roots used in for-loops

     # check the Arguments ...
     if not ( type in ["A", "B", "C", "D", "E", "F", "G"] ) then
          Error( "<type> must be one of ",
                 "\"A\", \"B\", \"C\", \"D\", \"E\", \"F\", \"G\" " );
     fi;


     if   ( type = "A" )         and not IsPosInt( n )                then
          Error( "<n> must be a positive integer for type A " );
     elif ( type in ["B", "C"] ) and not (IsPosInt( n ) and n >= 2 )  then
          Error( "<n> must be a positive integer for type ", type, " " );
     elif ( type = "D" )         and not (IsPosInt( n ) and n >= 4 )  then
          Error( "<n> must be a positive integer for type D " );
     elif ( type = "E" )         and not ( n in [6 ..8] )             then
          Error( "<n> must be one of 6, 7, 8 for type E " );
     elif ( type = "F" )         and not ( n = 4 )                    then
          Error( "<n> must be 4 for type F " );
     elif ( type = "G" )         and not ( n = 2 )                    then
          Error( "<n> must be 2 for type G " );
     fi;

     if not ( IsRing( F ) ) then
          Error( "<F> must be a ring." );
     fi;

     # Construct the family of our unipotent element objects.
     Fam:= NewFamily( Concatenation("UnipotChevFam", type, String( n )),
                      IsUnipotChevElem,
                      IsObject,
                      IsUnipotChevFamily );

##
#N  In the following two cases the roots are the same, but we need rational 
#N  structure constants.
#    L := SimpleLieAlgebra( type, n, F );          
     L := SimpleLieAlgebra( type, n, Rationals );  
     R := RootSystem( L );

     # Install the data.
     Fam!.type        := type;
     Fam!.rank        := n;
     Fam!.ring        := F;
     Fam!.rootsystem  := R;
     

     # Compute the structure constants ...
     N := NullMat( Length( PositiveRoots(R) ),
                   Length( PositiveRoots(R) ) );
     
     if   Fam!.type in ["B", "C", "F", "G"] then
#          B := Basis( L, Concatenation( R.rootvecs,
#                                  BasisVectors(Basis(CartanSubalgebra(L)))
#                                ));

          # NC = no check, ChevalleyBasis is a basis!
          # B := BasisNC( L, Flat(ChevalleyBasis( L )) );
          # CanonicalBasis is the Chevalley basis! and the structure
          # constants are already stored for CanonicalBasis.
          B := CanonicalBasis( L );

          T := StructureConstantsTable( B );
          # We only need a part of T,
          # example:  N_{r,s} = T[r][s][2][1]

          for r in [1..Length( PositiveRoots(R) )] do
               for s in [1..Length( PositiveRoots(R) )] do
                    if r < s and 
                       Position( PositiveRoots(R), 
                                 Sum( PositiveRoots(R){[r,s]} )
                               ) <> fail then
                         N[r][s] := T[r][s][2][1];
                         N[s][r] := T[s][r][2][1]; # = - N[r][s]
                    fi;
               od;
          od;
     else # Simple laced types
          H := function( M )
               local h, i, j, l;
               l := Length(M);
               h := IdentityMat(l);
               for i in [1..l-1] do
                    for j in [i+1..l] do
                         h[i][j] := M[i][j];
                    od;
               od;
               return h; # Immutable(h);
          end;
          h := H( CartanMatrix(R) );
          
          for r in [1..Length( PositiveRoots(R) )] do
               for s in [1..Length( PositiveRoots(R) )] do
                    if r < s and 
                       Position( PositiveRoots(R), 
                                 Sum( PositiveRoots(R){[r,s]} )
                               ) <> fail then
                         N[r][s] := (-1)^(  PositiveRootsFC(R)[r]
                                          * h 
                                          * PositiveRootsFC(R)[s] );
                         N[s][r] := - N[r][s];
                    fi;
               od;
          od;
     fi;

     Fam!.structconst := N; # war frueher T

     return  Fam;
end
);


################################################################################
##
#M  PrintObj( <Fam> )  . . . . . . . . . . . . . . . prints a `UnipotChevFamily'
##
##  Special Method for `UnipotChevFamily'
##

InstallMethod( PrintObj,
     "for a UnipotChevFamily",
     [ IsUnipotChevFamily ],
     function( Fam )
          Print( "UnipotChevFamily( \"",
                 Fam!.type, "\", ",
                 Fam!.rank, ", ",
                 Fam!.ring, " )"
               );
     end
);

################################################################################
##
#M  <Fam1> = <Fam2>  . . . . . . . . . . . . equality for two `UnipotChevFamily'
##
##  Returns `true' if both Familys have the same type and equal underlying
##  rings.
##
##  Are using this in Equality for UnipotChevSubGr
##

InstallMethod( \=,
     "for two UnipotChevFamily",
     IsIdenticalObj,
     [ IsUnipotChevFamily,
       IsUnipotChevFamily ],
     function( Fam1, Fam2 )
          return IsIdenticalObj( Fam1, Fam2 )
                 or (     ( Fam1!.type = Fam2!.type )
                      and ( Fam1!.rank = Fam2!.rank )
                      and ( Fam1!.ring = Fam2!.ring )
                    );
     end
);

################################################################################
##
#M  OneOp( <Fam> ) . . . . . . . . . . . the one-element of a `UnipotChevFamily'
##
##  Returns the identity in the family
##

InstallOtherMethod( OneOp,
     "for a UnipotChevFamily",
     [ IsUnipotChevFamily ],
     Fam -> UnipotChevElem( Fam, rec(roots:=[], felems:=[]),
                            UNIPOT_DEFAULT_REP )
);









################################################################################
##
#F  UnipotChevSubGr( <type>, <n>, <F> )
##
##  `UnipotChevSubGr' returns the unipotent subgroup $U$ of the Chevalley group
##  of type <type>, rank <n> over the ring <F>.
##
##  <type> must be one of "A", "B", "C", "D", "E", "F", "G"
##  For the type  "A", <n> must be a positive integer.
##  For the types "B" and "C", <n> must be a positive integer >= 2.
##  For the type  "D", <n> must be a positive integer >= 4.
##  For the type  "E", <n> must be one of 6, 7, 8.
##  For the type  "F", <n> must be 4.
##  For the type  "G", <n> must be 2.
##

InstallGlobalFunction(  UnipotChevSubGr,
function( type, n, F )
     local gr, Fam;

     Fam := UnipotChevFamily( type, n, F );
     
     gr := Objectify( NewType( CollectionsFamily( Fam ),
                               IsUnipotChevSubGr and IsAttributeStoringRep
                             ),
                      rec()
                    );

     SetIsWholeFamily( gr, true );

##
#N  see Theorem 5.3.3(i) in [Car72]
##
     SetIsNilpotentGroup( gr, true );
     
##
##  store the `RootSystem' in the attribute.
##
     SetRootSystem( gr, Fam!.rootsystem );

##
##  Store the property `IsFinite' (if the group is finite or not).
##  This helps choosing better Methods sometimes.
##  This is a ``cheap'' call, whereas calculating the size of the
##  group may require large powers of large integers.
##
     SetIsFinite( gr, IsFinite(F) );

     return gr;
end
);

################################################################################
##
#M  PrintObj( <U> ) . . . . . . . . . . . . . . . . . prints a `UnipotChevSubGr'
##
##  Special Method for `UnipotChevSubGr'
##

InstallMethod( PrintObj,
     "for a UnipotChevSubGr",
     [ IsUnipotChevSubGr ],
     function( U )
          local Fam;
          Fam := ElementsFamily( FamilyObj( U ) );
          
          Print( "UnipotChevSubGr( \"",
                 Fam!.type, "\", ",
                 Fam!.rank, ", ", 
                 Fam!.ring, " )"
               );
     end
);

################################################################################
##
#M  ViewObj( <U> ) . . . . . . . . . . . . . . . . . . . for a `UnipotChevSubGr'
##
##  Special Method for `UnipotChevSubGr'
##

InstallMethod( ViewObj,
     "for a UnipotChevSubGr",
     [ IsUnipotChevSubGr ], 1,
     function( U )
          local  type,
                 Fam;
          Fam := ElementsFamily( FamilyObj( U ) );
          type := Concatenation( Fam!.type, String(Fam!.rank) );
          
          Print("<Unipotent subgroup of a Chevalley group",
                " of type ", type, " over ");
          ViewObj( Fam!.ring ); Print(">");
     end
);

################################################################################
##
#M  <U1> = <U2> . . . . . . . . . . . . . . . equality for two `UnipotChevSubGr'
##
##  Special Method for `UnipotChevSubGr'
##

InstallMethod( \=,
     "for two UnipotChevSubGr",
     IsIdenticalObj,
     [ IsUnipotChevSubGr,
       IsUnipotChevSubGr ],
     function( U1, U2 )
          local Fam1, Fam2;

          Fam1 := ElementsFamily( FamilyObj( U1 ) );
          Fam2 := ElementsFamily( FamilyObj( U2 ) );
          
          return  IsIdenticalObj( U1, U2 ) or Fam1 = Fam2;
     end
);

################################################################################
##
#M  One( <U> )  . . . . . . . . . . . . . the one-element of a `UnipotChevSubGr'
##
##  Special Method for `UnipotChevSubGr'
##

InstallOtherMethod( One,
     "for a UnipotChevSubGr",
     [ IsUnipotChevSubGr ],
     U -> OneOp( U )
);

################################################################################
##
#M  OneOp( <U> )  . . . . . . . . . . . . the one-element of a `UnipotChevSubGr'
##
##  Special Method for `UnipotChevSubGr'
##

InstallOtherMethod( OneOp,
     "for a UnipotChevSubGr",
     [ IsUnipotChevSubGr ],
     U -> One(ElementsFamily( FamilyObj( U ) ))
);

################################################################################
##
#M  Size( <U> ) . . . . . . . . . . . . . the size of a finite `UnipotChevSubGr'
##
##  Special Method for finite `UnipotChevSubGr'
##
##  Are using the result of [Car72], Theorem 5.3.3 (ii) to compute the size.
##

InstallMethod( Size,
     "for a finite UnipotChevSubGr",
     [ IsUnipotChevSubGr and IsFinite ],
     function( U )
          local Fam, R;

          Fam := ElementsFamily( FamilyObj( U ) );
          R   := RootSystem(U);

          Info(UnipotChevInfo, 2, "The order of this group is ",
                                Size(Fam!.ring),"^",Length(PositiveRoots(R)),
                                " which is");
          return Size(Fam!.ring)^Length(PositiveRoots(R));
     end
);

################################################################################
##
#M  GeneratorsOfGroup( <U> )  . . . the generators of a finite `UnipotChevSubGr'
##
##  Special Method for finite `UnipotChevSubGr'
##
##

InstallOtherMethod( GeneratorsOfGroup,
     "for a finite UnipotChevSubGr",
     [ IsUnipotChevSubGr and IsFinite ],
     function( U )
          local Fam;

          Fam := ElementsFamily( FamilyObj( U ) );
          
          return 
          ListX( [1 .. Length(PositiveRoots(RootSystem(U)))], 
                 Difference( AsSet( Fam!.ring ), [Zero( Fam!.ring )] ),
                 function( r, x ) 
                      return UnipotChevElem( U, rec(roots:=[r], felems:=[x]), UNIPOT_DEFAULT_REP );
                 end
               );
     end
);


################################################################################
##
#M  CentralElement( <U> ) . . . . . . . . . . . . . . . for a  `UnipotChevSubGr'
##
##  Returns an arbitrary element from the center of <U>.
##
#T  This one has no proper name now. the declaration part should go into
#T  unipot.gd once it is working.
##

DeclareAttribute( "CentralElement", IsUnipotChevSubGr );

InstallMethod( CentralElement,
     "for a UnipotChevSubGr",
     [ IsUnipotChevSubGr ],
     function( U )
          local z,                 # the central element to be returned...
                t, tnr,            # the indeterminates for z ...
                posrootsFC, r,     # roots
                ring,              # underlying ring
                comm,              # the commutator of z with some elements ...
                ext,               # external rep of a polynom
                i,j;               # some loop variables ...
                
                
                
          posrootsFC := PositiveRootsFC(RootSystem(U));
          ring := ElementsFamily(FamilyObj(U))!.ring;
          
          # generate posroots many indeterminates...
          t := [];
          for r in [1..Length(posrootsFC)] do
               t[r] := Indeterminate( ring, 
                         Concatenation( "t_", String(r) ), t );
          od;
          
          # numbers of indeterminates ...
          tnr := List( t, 
                       s -> IndeterminateNumberOfUnivariateRationalFunction(s));
          
          # now <z> is an arbitrary Element in <U>:
          # (in fact, we could have called Representative(<U>), but
          #  we want to modify the lists <t> and <tnr> later on.)
          z := UnipotChevElemByFC( U, posrootsFC, t );

          # compute the commutator of z with any root elements
          # and draw conclusions
          for r in Reversed(posrootsFC) do
               repeat
                    comm := Comm( z, 
                                  UnipotChevElemByFC( U, r, One(ring) ), 
                                  "canonical" );
#                    Print("#I ");View(comm);Print("\n\n");
                    for i in comm!.felems do
                         ext := ExtRepPolynomialRatFun(i);
                         # if it is NOT a sum
                         if Length( ext ) = 2 then
                              for j in [1..Length(ext[1])] do
                                   if IsOddInt(j) and ext[1][j] in tnr then
                                        j:=Position(tnr, ext[1][j]);
#                                        Print("#I Eliminieren ",t[j],"\n");
                                        t[j] := Zero(ring);
                                        tnr[j] := Zero(ring);
                                        break;
                                   fi;
                              od;
                         fi;
                    od;
                    z := UnipotChevElemByFC( U, posrootsFC, t );
               until IsOne(comm);
          od;
          
          return z;
     end
);


################################################################################
##
#M  IsCentral( <U>, <z> ) . . . . for a `UnipotChevSubGr' and a `UnipotChevElem'
##

InstallMethod( IsCentral,
     "for a UnipotChevSubGr and a UnipotChevElem",
     IsCollsElms,
     [ IsUnipotChevSubGr, IsUnipotChevElem ],
     function( U, z )
          local posrootsFC, ring, r;
          
          # Treat the trivial case.
          if IsOne(z) then
               return true;
          fi;
          
          posrootsFC := PositiveRootsFC(RootSystem(U));
          ring := ElementsFamily(FamilyObj(U))!.ring;

          # The element <z> is central if and only if <z> commutes with all the 
          # elements x_r(1), r a positive root.
          for r in posrootsFC do;
               if not IsOne(Comm(z, UnipotChevElemByFC( U, r, One(ring) ), "canonical")) then
                    return false;
               fi;
          od;
          
          return true;
     end
);



################################################################################
##
#M  Representative( <U> ) . . . . . . . . . . . . . . . . . for `UnipotChevElem'
##
##  this one returns an element with ineterminates over the underlying ring 
##  instead of the ring elements itself. This allows ``symbolic'' computations.
##

InstallMethod( Representative,
     "for a UnipotChevElem",
     [ IsUnipotChevSubGr ],
     function( U )
          local posroots, r, indets;
          posroots := PositiveRoots(RootSystem(U));
          
          indets := [];
          for r in [1..Length(posroots)] do
               Add(indets,
                    Indeterminate( ElementsFamily(FamilyObj(U))!.ring,
                                   Concatenation("t_", String(r)),
                                   indets )
                   );
          od;
          
          return UnipotChevElem( U, rec(roots  := [1..Length(posroots)], 
                                        felems := indets ), UNIPOT_DEFAULT_REP );
     end
);




################################################################################
##
#M  UnipotChevElem( <Fam>, <record>, <rep> )
##
##  This is an `undocumented' function and should not be used at the GAP prompt
##  
##  the <record> must be of the following form:
##      rec( roots:=<roots>, felems:=<felems> )
##  where <roots> is a list of numbers from [1 .. <n>], <n> the number of
##  positive roots of the underlying root system and <felems> a list of the 
##  corresponding ring elements.
##  

InstallOtherMethod(  UnipotChevElem,
     "for UnipotChevFamily, a record and a string.",
     [IsUnipotChevFamily,
      IsRecord,
      IS_OPERATION],
     function( Fam, record, rep )
          local i,          # loop variable
                roots,      
                felems,     
                roots1,     # used for simplifying list
                F,          # the ring
                obj;        # the returned value

          roots  := record.roots;
          felems := record.felems;

          F := Fam!.ring;

          # check the lists
          if not Length( roots ) = Length( felems ) then
               Error( "<roots> and <felems> must have same length" );
          fi;

          for i in [1 .. Length(roots)] do
               if not ( IsBound(  roots[i] )
                    and IsBound( felems[i] ) ) then
                    Error(    
                         "<roots> and <felems> must be dense lists",
                         " of root numbers and ring elements,",
                         " respsctively.\n"
                    );
               fi;
               if not ( felems[i] in F )
                  and 
                  not IsIdenticalObj(ElementsFamily(FamilyObj(F)),
                                     CoefficientsFamily(FamilyObj(felems[i])))
                 then
                    Error( "<felems>[", i, "] must be an element ",
                           "of the ring ", F, "\n",
                           "or an Indeterminate over this ring." );
               fi;
               if not ( roots[i] in [1 .. Length( PositiveRoots(Fam!.rootsystem) )] ) then
                    Error( "<roots>[", i, "] must be an integer",
                           " in [1 .. <number of positive roots>]." );
               fi;
          od;
     
          if not rep in [ IsUnipotChevRepByRootNumbers,
                          IsUnipotChevRepByFundamentalCoeffs,
                          IsUnipotChevRepByRoots] then
               Error( "<rep> must be one of IsUnipotChevRepByRootNumbers,",
                      " IsUnipotChevRepByFundamentalCoeffs,",
                      " IsUnipotChevRepByRoots\n");
          fi;
          
          # use structural copies of the lists, so if one gave us mutable lists,
          # we do not mute them
          
          roots  := StructuralCopy(roots);
          felems := StructuralCopy(felems);

##     
#N  Note: The corresponding repeat-loop somewhere in CanonicalForm( <x> )
#N  MUST be always the same as the following loop here.
##
          # simplifying the element ...
          repeat
               roots1 := StructuralCopy(roots);
               for i in [ 1 .. Length(roots) ] do
                    if felems[i] = Zero(F)
                    or felems[i] = Zero(F)*Indeterminate(F) then
                         Unbind(  roots[i] );
                         Unbind( felems[i] );
                    elif i < Length(roots)
                     and roots[i] = roots[i+1] then
                         felems[i+1] := felems[i] + felems[i+1];
                         Unbind(  roots[i] );
                         Unbind( felems[i] );
                    fi;
               od;
               roots  := Compacted( roots );
               felems := Compacted( felems );
          until roots = roots1;
     
          obj := Objectify( NewType( Fam,
                                     IsUnipotChevElem and 
                                     rep and 
                                     IsCopyable),
                            rec(  roots := roots, 
                                            felems := felems )
                   );
          
     
          # we alredy know the canonical form of the object if the
          # length of <roots> is < 2:
          # it is the object itself:
          if Length(roots) < 2 then
               SetCanonicalForm( obj, obj );
          fi;
     
          # we alredy know that the element is the identity if the
          # length of <roots> is 0:
          if Length(roots) = 0 then
               SetIsOne( obj, true );

          # and we alredy know that the element is not the identity if the
          # length of <roots> is 1:
          elif Length(roots) = 1 then
               SetIsOne( obj, false );
          fi;
     
          return obj;
     end
);



################################################################################
##
#M  UnipotChevElem( <U>, <record>, <rep> )
##
##  This is an `undocumented' function and should not be used at the GAP prompt
##

InstallMethod(  UnipotChevElem,
     "for UnipotChevSubGr, a record and a string",
     [IsUnipotChevSubGr,
      IsRecord,
      IS_OPERATION],
     function( U, record, rep )
          return UnipotChevElem( ElementsFamily(FamilyObj(U)), record, rep );
     end
);


################################################################################
##
#M  UnipotChevElemByRootNumbers( <U>, <roots>, <felems> )
##
##  Returns an element of a unipotent subgroup <U> with representation
##  `IsUnipotChevRepByRootNumbers'.
##
##  <roots> should be a list of numbers of the roots in 
##  `RootSystem(<U>).posroots' and <felems> a list of corresponding ring 
##  elements,
##

InstallMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevSubGr and a list of positions of roots",
     [IsUnipotChevSubGr,
      IsList,
      IsList],
     function( U, roots, felems )
          return UnipotChevElem( U, rec( roots:=roots, felems:=felems ),
                                 IsUnipotChevRepByRootNumbers );
     end
);

################################################################################
##
#M  UnipotChevElemByRoots( <U>, <roots>, <felems> )
##
##  Returns an element of a unipotent subgroup <U> with representation
##  `IsUnipotChevRepByRoots'. 
##
##  <roots> should be a list of roots in `RootSystem(<U>).posroots' and 
##  <felems> a list of corresponding ring elements,
##

InstallMethod(  UnipotChevElemByRoots,
     "for UnipotChevSubGr and a list of roots",
     [IsUnipotChevSubGr,
      IsList,
      IsList],
     function( U, roots, felems )
          local list_of_positions,
                i,
                Fam,
                F;

          Fam := ElementsFamily(FamilyObj(U));

          F := Fam!.ring;
          list_of_positions := [];
          
          # check the lists
          if not Length( roots ) = Length( felems ) then
               Error( "<roots> and <felems> must have same length" );
          fi;
          for i in [1 .. Length(roots)] do
               if not ( IsBound(  roots[i] )
                    and IsBound( felems[i] ) ) then
                    Error(    
                         "<roots> and <felems> must be dense lists",
                         " of roots and ring elements,",
                         " respsctively.\n"
                    );
               fi;

               if not ( felems[i] in F )
                  and 
                  not IsIdenticalObj(ElementsFamily(FamilyObj(F)),
                                     CoefficientsFamily(FamilyObj(felems[i])))
                 then
                    Error( "<felems>[", i, "] must be an element ",
                           "of the ring ", F, "\n",
                           "or an Indeterminate over this ring." );
               fi;
               
               Add( list_of_positions, Position( PositiveRoots(RootSystem(U)), roots[i] ) );
               
               if list_of_positions[i] = fail then
                    Error( "<roots>[", i, "] must be a root." );
               fi;
          od;
               
          return UnipotChevElem( U, rec(  roots := list_of_positions,
                                         felems := felems ), 
                                IsUnipotChevRepByRoots );
     end
);

################################################################################
##
#M  UnipotChevElemByFundamentalCoeffs( <U>, <roots>, <felems> )
##
##  Returns an element of a unipotent subgroup <U> with representation
##  `IsUnipotChevRepByFundamentalCoeffs'. 
##
##  <roots> should be a list of roots as coefficients of a linear combination
##  of fundamental roots `RootSystem(<U>).fundroots' and <felems> a list of 
##  corresponding ring elements,
##

InstallMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevSubGr and a list of coordinates of roots",
     [IsUnipotChevSubGr,
      IsList,
      IsList],
     function( U, roots, felems )
          local list_of_positions,
                i,
                Fam,
                F;

          Fam := ElementsFamily(FamilyObj(U));

          F := Fam!.ring;
          list_of_positions := [];
          

          # check the lists
          if not Length( roots ) = Length( felems ) then
               Error( "<roots> and <felems> must have same length" );
          fi;
          for i in [1 .. Length(roots)] do
               if not ( IsBound(  roots[i] )
                    and IsBound( felems[i] ) ) then
                    Error(    
                         "<roots> and <felems> must be dense lists",
                         " of roots as coefficients of a linear",
                         " combination of fundamental roots",
                         " and ring elements,",
                         " respsctively.\n"
                    );
               fi;

               if not ( felems[i] in F )
                  and 
                  not IsIdenticalObj(ElementsFamily(FamilyObj(F)),
                                     CoefficientsFamily(FamilyObj(felems[i])))
                 then
                    Error( "<felems>[", i, "] must be an element ",
                           "of the ring ", F, "\n",
                           "or an Indeterminate over this ring." );
               fi;
               
               Add( list_of_positions, Position( PositiveRootsFC(RootSystem(U)), roots[i] ) );
               
               if list_of_positions[i] = fail then
                    Error( "<roots>[", i, "] must be a list of coefficients",
                           " representing a root as a linear combination of",
                           " fundamental roots." );
               fi;
          od;
               
          return UnipotChevElem( U, rec(  roots := list_of_positions,
                                         felems := felems ), 
                                IsUnipotChevRepByFundamentalCoeffs );
     end
);

################################################################################
##
#M  UnipotChevElemByRootNumbers( <U>, <root>, <felem> )
#M  UnipotChevElemByRoots( <U>, <root>, <felem> )
#M  UnipotChevElemByFundamentalCoeffs( <U>, <root>, <felem> )
##
##  These are abbreviations for `UnipotChevElemByXX( <U>, [<root>], [<felem>] )'
##

InstallOtherMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevSubGr, a position of a root and a ring rlement",
     [IsUnipotChevSubGr,
      IsPosInt,
      IsObject], # the third argument must be an element of Fam!.ring
     function( U, root, felem )
          return UnipotChevElemByRootNumbers( U, [root], [felem] );
     end
);

InstallOtherMethod(  UnipotChevElemByRoots,
     "for UnipotChevSubGr, a root and a ring rlement",
     [IsUnipotChevSubGr,
      IsList,
      IsObject], # the third argument must be an element of Fam!.ring
     function( U, root, felem )
          return UnipotChevElemByRoots( U, [root], [felem] );
     end
);

InstallOtherMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevSubGr, a lin. comb. of fund. roots and a ring element",
     [IsUnipotChevSubGr,
      IsList,
      IsObject], # the third argument must be an element of Fam!.ring
     function( U, root, felem )
          return UnipotChevElemByFundamentalCoeffs( U, [root], [felem] );
     end
);

################################################################################
##
#M  UnipotChevElemByRootNumbers( <U>, <list> )
#M  UnipotChevElemByRoots( <U>, <list> )
#M  UnipotChevElemByFundamentalCoeffs( <U>, <list> )
##
#N  These are old versions of `UnipotChevElemByXX'.
#N  They are *deprecated* and installed for compatibility only.
#N  They may be removed at any time.
##

InstallOtherMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevSubGr and a list. ***DEPRECATED***",
     [IsUnipotChevSubGr,
      IsList], 
     function( U, list )
          local roots, felems, i;
          
          Info(InfoWarning, 1, "UnipotChevElemByRootNumbers( <U>, <list> ) is ***DEPRECATED***");
          
          roots  := [];
          felems := [];
          
          # no checking the list at all.
          for i in [1 .. Length(list)] do;
               Add( roots,  list[i].r );
               Add( felems, list[i].x );
          od;
          return UnipotChevElemByRootNumbers( U, roots, felems );
     end
);

InstallOtherMethod(  UnipotChevElemByRoots,
     "for UnipotChevSubGr and a list. ***DEPRECATED***",
     [IsUnipotChevSubGr,
      IsList], 
     function( U, list )
          local roots, felems, i, Fam;
          
          Info(InfoWarning, 1, "UnipotChevElemByRoots( <U>, <list> ) is ***DEPRECATED***");
          
          roots  := [];
          felems := [];
                    
          # no checking the list at all.
          for i in [1 .. Length(list)] do;
               Add( roots,  list[i].r );
               Add( felems, list[i].x );
          od;
          return UnipotChevElemByRoots( U, roots, felems );
     end
);

InstallOtherMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevSubGr and a list. ***DEPRECATED***",
     [IsUnipotChevSubGr,
      IsList], 
     function( U, list )
          local roots, felems, i, Fam;
          
          Info(InfoWarning, 1, "UnipotChevElemByFundamentalCoefficions( <U>, <list> ) is ***DEPRECATED***");
          
          roots  := [];
          felems := [];
          
          # no checking the list at all.
          for i in [1 .. Length(list)] do;
               Add( roots,  list[i].coeffs );
               Add( felems, list[i].x );
          od;
          return UnipotChevElemByFundamentalCoeffs( U, roots, felems );
     end
);


################################################################################
##
#M  UnipotChevElemByRootNumbers( <x> )
#M  UnipotChevElemByFundamentalCoeffs( <x> )
#M  UnipotChevElemByRoots( <x> )
##
##  These are provided for converting elements to diferent representations.
##
##  E.g. If <x> has already the representation `IsUnipotChevRepByRoots', 
##       then <x> itself is returned by `UnipotChevElemByRoots( <x> ). 
##       Otherwise a *new* element with given representation is constructed. 
##

DeclareGlobalFunction( "ChangeUnipotChevRep" );
InstallOtherMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevElem",
     [IsUnipotChevElem],
     function( x )
          if IsUnipotChevRepByRootNumbers(x) then 
               return x;
          else
               return ChangeUnipotChevRep( x, 
                                           IsUnipotChevRepByRootNumbers );
          fi;
     end
);

InstallOtherMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevElem",
     [IsUnipotChevElem],
     function( x )
          if IsUnipotChevRepByFundamentalCoeffs(x) then 
               return x;
          else
               return ChangeUnipotChevRep( x, 
                                           IsUnipotChevRepByFundamentalCoeffs );
          fi;
     end
);

InstallOtherMethod(  UnipotChevElemByRoots,
     "for UnipotChevElem",
     [IsUnipotChevElem],
     function( x )
          if IsUnipotChevRepByRoots(x) then 
               return x;
          else
               return ChangeUnipotChevRep( x, 
                                           IsUnipotChevRepByRoots );
          fi;
     end
);


InstallGlobalFunction(  ChangeUnipotChevRep,
     function( x, rep )
          local Fam,
                new_obj,
                new_inv,
                new_canon;

          Fam := FamilyObj(x);
          
          new_obj := UnipotChevElem( Fam, rec(  roots := x!.roots,
                                               felems := x!.felems ), 
                                     rep );

          if HasIsOne(x) then
               SetIsOne( new_obj, IsOne(x) );
          fi;

          if HasInverse(x) then
               if IsIdenticalObj(x, Inverse(x)) then
                    SetInverse( new_obj, new_obj );
               else
                    new_inv := UnipotChevElem( Fam, 
                                          rec(  roots := Inverse(x)!.roots,
                                               felems := Inverse(x)!.felems ),
                                          rep );
                    if HasIsOne(Inverse(x)) then
                         SetIsOne( new_inv, IsOne(x) );
                    fi;
                    SetInverse( new_obj, new_inv );
                    SetInverse( new_inv, new_obj );
                    if HasCanonicalForm(x^-1) then
                         if IsIdenticalObj(x^-1, CanonicalForm(x^-1)) then
                              SetCanonicalForm( new_obj^-1, new_obj^-1 );
                         else
                              new_canon := 
                              UnipotChevElem( 
                                   Fam,
                                   rec( roots := CanonicalForm(x^-1)!.roots,
                                       felems := CanonicalForm(x^-1)!.felems ),
                                   rep );
                              # CanonicalForm always HasIsOne !
                              SetIsOne( new_canon, IsOne(x) );
                              SetCanonicalForm( new_inv,   new_canon );
                              SetCanonicalForm( new_canon, new_canon );
                              SetInverse( new_canon, new_obj );
                         fi;
                         
                    fi;
               fi;
          fi;
          if HasCanonicalForm(x) then
               if IsIdenticalObj( x, CanonicalForm(x) ) then
                    SetCanonicalForm( new_obj, new_obj );
               else
                    new_canon := UnipotChevElem(
                                    Fam,
                                    rec( roots := CanonicalForm(x)!.roots,
                                        felems := CanonicalForm(x)!.felems ),
                                    rep );
                    SetCanonicalForm( new_obj,   new_canon );
                    SetCanonicalForm( new_canon, new_canon );
                    if HasInverse( new_obj ) then
                         SetInverse( new_canon, Inverse(new_obj) );
                    fi;
               fi;
          fi;
          
          
          
          return new_obj;
     end
);


################################################################################
##
#M  PrintObj( <x> )  . . . . . . . . . . . . . . . . . prints a `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##

InstallMethod( PrintObj,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ], 1,
     function( x )
     local Fam;
          Fam := FamilyObj(x);
          
          if Length(x!.roots) = 0 then
               Print("One( ",  "UnipotChevSubGr( \"",
                      Fam!.type, "\", ", 
                      Fam!.rank, ", ", 
                      Fam!.ring, " )");  
          else
               if IsUnipotChevRepByRootNumbers(x) then
                    Print( "UnipotChevElemByRootNumbers" );
               elif IsUnipotChevRepByFundamentalCoeffs(x) then
                    Print( "UnipotChevElemByFundamentalCoeffs" );
               elif IsUnipotChevRepByRoots(x) then
                    Print( "UnipotChevElemByRoots" );
               fi;
               
               Print( "( ", "UnipotChevSubGr( \"",
                      Fam!.type, "\", ", 
                      Fam!.rank, ", ", 
                      Fam!.ring, " )", ", " );
                      
               if IsUnipotChevRepByRootNumbers(x) then
                    Print( x!.roots );
               elif IsUnipotChevRepByFundamentalCoeffs(x) then
                    Print( List(x!.roots, r -> PositiveRootsFC(Fam!.rootsystem)[r] ));
               elif IsUnipotChevRepByRoots(x) then
                    Print( List(x!.roots, r -> PositiveRoots  (Fam!.rootsystem)[r] ));
               fi;
               
               Print( ", ", x!.felems, " )");
               
          fi;
     end
);

################################################################################
##
#M  ViewObj( <x> )  . . . . . . . . . . . . . . . . . . . . for `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
#T  maybe should check for the length of the output and "beautify" the output 
#T  depending on the SizeScreen()
##

InstallMethod( ViewObj,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     function( x )
          local r, R;

          R := FamilyObj(x)!.rootsystem;

          if Length( x!.roots ) = 0 then
             Print( "<identity>" );  
          else
               for r in [ 1 .. Length(x!.roots) ] do
                    if r > 1 then Print(" * "); fi;
                    Print( "x_{" );
                    if IsUnipotChevRepByRootNumbers(x) then
                         ViewObj(                     x!.roots[r]   );
                    elif IsUnipotChevRepByFundamentalCoeffs(x) then
                         ViewObj( PositiveRootsFC(R)[ x!.roots[r] ] );
                    elif IsUnipotChevRepByRoots(x) then
                         ViewObj( PositiveRoots  (R)[ x!.roots[r] ] );
                    fi;
                    Print( "}( " );
                         ViewObj( x!.felems[r] );
                    Print( " )" );
               od;
          fi;
     end
);

################################################################################
##
#M  ShallowCopy( <x> )  . . . . . . . . . . . . . . . . . . for `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  should we have different Methods for each Representation? this means much
##  code duplication ...
##

InstallMethod( ShallowCopy,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     function( x )
          local rep, copy;
          
          if IsUnipotChevRepByRootNumbers(x) then
               rep := IsUnipotChevRepByRootNumbers;
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := IsUnipotChevRepByFundamentalCoeffs(x);
          elif IsUnipotChevRepByRoots(x) then
               rep := IsUnipotChevRepByRoots;
          fi;

          copy := UnipotChevElem( FamilyObj( x ), 
                                 rec( roots := x!.roots,
                                     felems := x!.felems ), 
                                 rep );
         
          if HasInverse(x) then
               SetInverse(copy, Inverse(x));
          fi;
          if HasCanonicalForm(x) then
               SetCanonicalForm(copy, CanonicalForm(x));
          fi;
          if HasIsOne(x) then
               SetIsOne(copy, IsOne(x));
          fi;
          
          return copy;
     end
);

################################################################################
##
#M  <x> = <y>  . . . . . . . . . . . . . . . . equality for two `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  If <x> and <y> are identical or are products of the *same* root elements
##  then `true' is returned. Otherwise canonical form of both arguments must be
##  computed (if not already known), which may be expensive. 
##  

InstallMethod( \=,
     "for two UnipotChevElem",
     IsIdenticalObj,
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     function( x, y )
          if IsIdenticalObj( x,y ) or 
             ( x!.roots = y!.roots and x!.felems = y!.felems ) then
               return true;
          else 

          if not HasCanonicalForm(x) then
               Info(UnipotChevInfo, 3,
                          "CanonicalForm for the 1st argument is not known.");
               Info(UnipotChevInfo, 3,
                          "                  computing it may take a while.");
          fi;
          if not HasCanonicalForm(y) then
               Info(UnipotChevInfo, 3,
                          "CanonicalForm for the 2nd argument is not known.");
               Info(UnipotChevInfo, 3,
                          "                  computing it may take a while.");
          fi;

               return    CanonicalForm(x)!.roots  = CanonicalForm(y)!.roots
                     and CanonicalForm(x)!.felems = CanonicalForm(y)!.felems;
          fi;
     end
);

################################################################################
##
#M  <x> < <y> . . . . . . . . . . . . . . . . less than for two `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  This is needed e.g. by `AsSSortetList'.
##
##  The ordering is computed in the following way:
##  Let    x = x_{r_1}(s_1) ... x_{r_n}(s_n)
##  and    y = x_{r_1}(t_1) ... x_{r_n}(t_n), then
##  
##             x < y   <=>  [ s_1, ..., s_n ] < [ t_1, ..., t_n ],
##  
##  where the lists are compared lexicographically.
##  e.g. x = x_1(1)x_2(1) = x_1(1)x_2(1)x_3(0)  --> field elems: [ 1, 1, 0 ]
##       y = x_1(1)x_3(1) = x_1(1)x_2(0)x_3(1)  --> field elems: [ 1, 0, 1 ]
##   --> y < x (above lists ordered lexicographically)
##  

InstallMethod( \<,
     "for two UnipotChevElem",
     IsIdenticalObj,
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     function( x, y )
          local Fam, R, r, pos, tx, ty, o;
          
          if IsIdenticalObj( x,y ) or 
             ( x!.roots = y!.roots and x!.felems = y!.felems ) then
               return false;
          else 

               if not HasCanonicalForm(x) then
                    Info(UnipotChevInfo, 3,
                               "CanonicalForm for the 1st argument is not known.");
                    Info(UnipotChevInfo, 3,
                               "                  computing it may take a while.");
               fi;
               if not HasCanonicalForm(y) then
                    Info(UnipotChevInfo, 3,
                               "CanonicalForm for the 2nd argument is not known.");
                    Info(UnipotChevInfo, 3,
                               "                  computing it may take a while.");
               fi;

               Fam := FamilyObj(x);
               R   := Fam!.rootsystem;
               o   := Zero(Fam!.ring);
               
               for r in [1 .. Length(PositiveRoots(R))] do;

                    pos := Position( CanonicalForm(x)!.roots, r );
                    if pos = fail then tx := o;
                                  else tx := CanonicalForm(x)!.felems[pos]; 
                    fi;
                    pos := Position( CanonicalForm(y)!.roots, r );
                    if pos = fail then ty := o;
                                  else ty := CanonicalForm(y)!.felems[pos]; 
                    fi;
                    if not tx = ty then
                         return tx < ty;
                    fi;                
               od;

               return false;
          fi;
     end
);

################################################################################
##
#M  <x> * <y>  . . . . . . . . . . . . . multiplication for two `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
#N  The representation of the product will be always the representation of the
#N  first argument.
##

InstallMethod( \*,
     "for two UnipotChevElem",
     IsIdenticalObj,
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     function( x, y )
          local rep;
          
          if IsUnipotChevRepByRootNumbers(x) then
               rep := IsUnipotChevRepByRootNumbers;
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := IsUnipotChevRepByFundamentalCoeffs;
          elif IsUnipotChevRepByRoots(x) then
               rep := IsUnipotChevRepByRoots;
          fi;
          
          return UnipotChevElem( 
                    FamilyObj( x ),
                    rec( roots := Concatenation( x!.roots,  y!.roots  ),
                        felems := Concatenation( x!.felems, y!.felems ) ),
                    rep
                 );
     end
);

################################################################################
##
#M  <x>^<i> . . . . . . . . . . . . . . . . . integer powers of `UnipotChevElem'
##
##  Special methods for root elements and for the identity.
##

InstallMethod( \^,
     "for a root element UnipotChevElem and an integer",
     [ IsUnipotChevElem and IsRootElement,
       IsInt ],
     function( x, i )
          local rep;
          
          if IsUnipotChevRepByRootNumbers(x) then
               rep := IsUnipotChevRepByRootNumbers;
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := IsUnipotChevRepByFundamentalCoeffs;
          elif IsUnipotChevRepByRoots(x) then
               rep := IsUnipotChevRepByRoots;
          fi;
          
          return UnipotChevElem( 
                         FamilyObj( x ),
                         rec( roots := x!.roots,
                             felems := [ i * x!.felems[1] ] ),
                         rep
                 );
     end
);

InstallMethod( \^,
     "for a identity UnipotChevElem and an integer",
     [ IsUnipotChevElem and IsOne,
       IsInt ],
     function( x, i )
          return x;
     end
);

################################################################################
##
#M  OneOp( <x> ) . . . . . . . . . . . . . the one-element of a `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  `OneOp' returns the multiplicative neutral element of <x>. This is equal to
##  <x>^0. 
##

InstallMethod( OneOp,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     x -> One( FamilyObj( x ) )
);


################################################################################
##
#M  IsOne( <x> )  . . . . . . . . . . . . the one-property of a `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##

InstallMethod( IsOne,
     "for a UnipotChevElem with known canonical form",
     [ IsUnipotChevElem and HasCanonicalForm ], 1,
     x -> IsOne( CanonicalForm( x ) )
);

InstallMethod( IsOne,
     "for a UnipotChevElem with known inverse",
     [ IsUnipotChevElem and HasInverse ],
     function( x )
          if HasIsOne( Inverse(x) ) then
               return IsOne( Inverse(x) );
          else
               TryNextMethod();
          fi;
     end
);

InstallMethod( IsOne,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     x -> IsOne( CanonicalForm( x ) )
);


################################################################################
##
#M  Inverse( <x> ) . . . . . . . . . . . . . . the inverse of a `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##

InstallMethod( Inverse,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     function( x )
          local inv, rep;
          if HasCanonicalForm( x ) and HasInverse(CanonicalForm( x )) then
               return Inverse(CanonicalForm( x ));
          fi;
          
          if IsUnipotChevRepByRootNumbers(x) then
               rep := IsUnipotChevRepByRootNumbers;
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := IsUnipotChevRepByFundamentalCoeffs;
          elif IsUnipotChevRepByRoots(x) then
               rep := IsUnipotChevRepByRoots;
          fi;
          
          inv := UnipotChevElem( 
                         FamilyObj( x ),
                         rec( roots :=  Reversed(x!.roots),
                             felems := -Reversed(x!.felems) ),
                         rep
                 );
                            
          # We alredy know the inverse of inv: it's x itself.
          SetInverse( inv, x );
          
          # Unfortunately we don't know the CanonicalForm for inv even
          # if we know it for x. If we would, we would store it here.
          # (Note that ``know'' have to be understood in the sense that
          # it is very cheap to get such a value.)

          # but the inverse of the canonical form of x is the same as
          # the inverse of x:
          if HasCanonicalForm( x ) then
               SetInverse( CanonicalForm(x), inv );
          fi;

          # if the Property IsOne of x is known, so it is also known for
          # the inverse!
          if HasIsOne( x ) then
               SetIsOne( inv, IsOne(x) );
          fi;
          
          return inv;
     end
);



################################################################################
##
#M  InverseOp( <x> ) . . . . . . . . . . . . . the inverse of a `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
#N  Ref, Chapter 28 (?reference:inverse) sais:
#N     The  default  method  of `Inverse'  is  to call `InverseOp' (note that
#N     `InverseOp'  must  *not*  delegate  to  `Inverse');  other  methods to
#N     compute inverses need to be installed only for `InverseOp'.
#N  
#N  In our case computing Inverse may be very time intensive, so
#N  we want to call `Inverse' instead of `InverseOp' because we
#N  want the inverse to be stored as attribute once computed
#N 
#N  In fact, the canonical form of an element is the time intensive
#N  operation. But consider the following example:
#N  x := UnipotChevElem(...);
#N  y := x^-1;
#N  CanonicalForm(y);
#N  z := x^-1;
#N  CanonicalForm(z); # must be computed again if Inverse not stored in x
#N 
#N  Note, that delegating `InverseOp' to `Inverse' *requires*
#N  `Inverse' to be overwritten not to call `InverseOp'.
##  

InstallMethod( InverseOp,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     x -> Inverse(x)
);


################################################################################
##
#F  ChevalleyCommutatorConstant( <i>, <j>, <r>, <s>, <Fam> )
##
##  This is an `undocumented' function and should not be used at the GAP prompt
##
##  The constants occuring in Chevalley's commutator formula. We compute them
##  as Rationals and embed them in the given ring afterwards. (It is known of 
##  them to be in [ 1, -1, 2, -2, 3, -3 ], in Char 2 this would be [ 1, 0 ] =
##  [ 1, 1, 0, 0, 1, 1 ] )
## 
##  The meaning of the arguments comes from the commutator formula, where <r>
##  and <s> are not the roots themself but their indices.
##  (e.g. roots are $r = r1$ and $s = r2$ --> arguments are $r = 1$, $s = 2$)
##

InstallGlobalFunction( ChevalleyCommutatorConstant,
     function( i, j, r, s, Fam)
          local M, N,
                const,
                roots,
                F;
          
          M := function( r, s, i )
               local m,   # the returned value
                     k,p; # used in the for-loop

               m := 1;
               Info( UnipotChevInfo, 6, 
                          "M( ", r, ", ", s, ", ", i, " ) = 1/", Factorial(i) );
               
               for k in [ 0 .. i-1 ] do
                    p := Position( roots, k * roots[r] + roots[s] );
                    Info( UnipotChevInfo, 6, "        * N( ", r, ", ", p, " )",
                                           "  [ = ", N[r][p], " ]" );
                    m := m * N[r][p];
               od;

               return m/Factorial(i);
          end;

          roots := PositiveRoots(Fam!.rootsystem);
          F     := Fam!.ring;
          N     := Fam!.structconst;
                    
          if not (r in [1 .. Length(roots)] and s in [1 .. Length(roots)]) then
               Error( "<r> and <s> must be integers in",
                      " [1 .. <number of positive roots>]." );
          fi;
          if not IsUnipotChevFamily(Fam) then
               Error( "<Fam> must be UnipotChevFamily (not ",
                      FamilyObj(Fam), ")." );
          fi;

          Info(UnipotChevInfo, 6, "C( ", i, ", ", j, ", ", r, ", ", s, " ) = ");

          
          if   j=1 then
               Info( UnipotChevInfo, 6, 
                     "        = M( ", r, ", ", s, ", ", i, " )" );
               const :=          M( r,   s, i );
          elif i=1 then
               Info( UnipotChevInfo, 6, 
                     "        = -1^", j, " * M( ", s, ", ", r, ", ", j, " )" );
               const := (-1)^j * M( s,   r, j );
          elif i=3 and j=2 then
               Info( UnipotChevInfo, 6, 
                     "        = 1/3 * M( ",
                     Position( roots, roots[r]+roots[s] ),
                     ", ", r, ", ", 2, " )" );
               const :=  1/3  * M( Position( roots, roots[r]+roots[s] ), r, 2 );
          elif i=2 and j=3 then
               Info( UnipotChevInfo, 6,
                     "        = -2/3 * M( ", 
                     Position( roots, roots[s]+roots[r] ), 
                     ", ", s,", ", 2, " )" );
               const := -2/3  * M( Position( roots, roots[s]+roots[r] ), s, 2 );
          else
               Error("i=",i, ", j=", j, " are illegal parameters.");
          fi;
          
          if not (const in [ 1, 2, 3, -1, -2, -3 ]) then
               Error("Something's gone wrong: the constant should be in",
                     " [ 1, 2, 3, -1, -2, -3 ]\n",
                     "instead of ", const);
          fi;
          
          return const*One(F);
     end
);

################################################################################
##
#M  <x>^<y>  . . . . . . . . . . . . . . . . . conjugation with `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  Doesn't do anything more than the default Method, but it stores the inverse
##  of <y>. And the result has the representation of <x>. (The default method
##  would return an element with representation of <y>.)
##

InstallMethod( \^,
     "for two UnipotChevElem's",
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     function( x, y )
          local fun;
          
          if IsUnipotChevRepByRootNumbers(x) then
               fun := UnipotChevElemByRN;
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               fun := UnipotChevElemByFC;
          elif IsUnipotChevRepByRoots(x) then
               fun := UnipotChevElemByR;
          fi;
          
          return fun( y^-1 * x * y );
     end
);

################################################################################
##
#M  Comm( <x>, <y> )  . . . . . . . . . . . . commutator of two `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  Doesn't do anything more than the default Method, but it stores the inverse
##  elements of <x> and <y>
##
##  The result has the representation of <x>.
##

InstallMethod( Comm,
     "for two UnipotChevElem",
     IsIdenticalObj,
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     function( x, y )
          return x^-1 * y^-1 * x * y;
     end
);

################################################################################
##
#M  Comm( <x>, <y>, "canonical" ) . . . . . . . . . commutator in canonical form
#M                                . . . . . . . . . . .  of two `UnipotChevElem'
##
##  Computes CanonicalForm of the Comm
##

InstallOtherMethod( Comm,
     "for two UnipotChevElem and a string",
     IsFamFamX,
     [ IsUnipotChevElem,
       IsUnipotChevElem,
       IsString ],
     function( x, y, canon)
          if canon <> "canonical" then
               Error("3rd argument (if present) must be \"canonical\"");
          fi;
          return CanonicalForm( Comm( x, y ) );
     end
);

################################################################################
##
#M  Comm( <x>, <y>, "canonical" ) . . . . . . . . . commutator in canonical form
#M                                . . .  of two root elements (`UnipotChevElem')
##
##  This Method calls `Comm( <x>, <y>, "as list" )', which uses a theorem,
##  known as Chevalley's commutator formula.
##

InstallOtherMethod( Comm,
     "for two root elements UnipotChevElem and a string",
     IsFamFamX,
     [ IsUnipotChevElem and IsRootElement,
       IsUnipotChevElem and IsRootElement,
       IsString ],
     function( x, y, canon )
          local rep, obj;

          if canon <> "canonical" then
               Error("3rd argument (if present) must be \"canonical\"");
          fi;

          if IsUnipotChevRepByRootNumbers(x) then
               rep := IsUnipotChevRepByRootNumbers;
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := IsUnipotChevRepByFundamentalCoeffs;
          elif IsUnipotChevRepByRoots(x) then
               rep := IsUnipotChevRepByRoots;
          fi;
     

          obj := UnipotChevElem(
                         FamilyObj(x),
                         Comm( rec(r := x!.roots[1], x := x!.felems[1]),
                               rec(r := y!.roots[1], x := y!.felems[1]),
                               "as list", 
                               FamilyObj(x) ),
                         rep
                      );

          # obj has already the canonical form
          SetCanonicalForm( obj, obj );

          return obj;
     end
);

################################################################################
##
#M  Comm( <x>, <y>, "as list", <Fam> )  . . . . . . commutator in canonical form
#M                              . . . .  of two root elements (`UnipotChevElem')
##
##  This is an `undocumented' function and should not be used at the GAP prompt
##
##  This method uses a theorem, known as Chevalley's commutator formula, 
##        see [Car72], Theorem 5.2.2 (especially Corollar 5.2.3), Pages 76,77
##

InstallOtherMethod( Comm,
     "for two records, a string and a UnipotChevFamily",
     IsFamFamXY,
     [ IsRecord,
       IsRecord,
       IsString,
       IsUnipotChevFamily ],
     function( x, y, str, Fam )
          local comm_roots,
                comm_felems,  # will be returned
                roots,        # Roots
                in12, in21,   # Hilfsvariablen
                add;          # Hilfsfunktion

          if str <> "as list" then
               Error("3rd argument must be \"as list\"");
          fi;
          
          comm_roots  := [];
          comm_felems := [];
          roots := PositiveRoots(Fam!.rootsystem);

          add := function(i,j)
               Add(comm_roots, 
                    Position( roots, j*roots[x.r] + i*roots[y.r] )         );
               Add(comm_felems, 
                    ChevalleyCommutatorConstant( i, j, y.r, x.r, Fam )
                              * (-y.x)^i * (x.x)^j                         );
          end;
          
##
#N  1*r + 1*s  can appear in a group of any type
#N  2*r + 1*s  can appear only in groups of types B_n, C_n, F_4, G_2
#N  3*r + 1*s  \ these both con only appear in a group of type G_2 
#N  3*r + 2*s  / and only together!
#N 
#N  see [Car72], Lemma 3.6.3
## 

          
          if (roots[y.r] + roots[x.r]) in roots then
               add(1,1);
               if Fam!.type in [ "B", "C", "F" ] then
               
                    if   (2*roots[y.r] + roots[x.r]) in roots then
                         add(2,1);
                    elif (roots[y.r] + 2*roots[x.r]) in roots then
                         add(1,2);
                    fi;

               elif (Fam!.type = "G") then
                    in21 := (2*roots[y.r] + roots[x.r]) in roots;
                    in12 := (roots[y.r] + 2*roots[x.r]) in roots;
                    
                    if   in21 and in12 then
                         # which of them is the first inside roots?
                         if   Position(roots, 2*roots[y.r] + roots[x.r])  
                            < Position(roots, roots[y.r] + 2*roots[x.r]) then
                              add(2,1);
                              add(1,2);
                         else
                              add(1,2);
                              add(2,1);
                         fi;
                    elif in21 then
                         add(2,1);
                         if (3*roots[y.r] + roots[x.r]) in roots then
                              add(3,1);
                              add(3,2);
                         fi;
                    elif in12 then
                         add(1,2);
                         if  (roots[y.r] + 3*roots[x.r]) in roots then
                              add(1,3);
                              add(2,3);
                         fi;
                    fi;
               fi;
          fi;
          return rec( roots := comm_roots, 
                     felems := comm_felems );
     end
);



################################################################################
##
#M  IsRootElement( <x> ) . . . . . . . . . is a `UnipotChevElem' a root element?
##
##  `IsRootElement' returns `true' if and only if <x> is a <root element>,
##  i.e $<x>=x_{r_i}(t)$ for some root $r_i$.
##
##  We store this property just after creating objects.
##
#N  *Note:* the canonical form of <x> may be a root element even if <x> isn't
#N  one.
##

InstallImmediateMethod( IsRootElement,
     "for a UnipotChevElem",
     IsUnipotChevElem,
     0,
     x -> ( Length( x!.roots ) = 1 )
);

################################################################################
##
#M  CanonicalForm( <x> )  . . . . . . . . . canonical form of a `UnipotChevElem'
##
##  `CanonicalForm' returns the canonical form of <x>. 
##  For more information on the canonical form see [Car72], Theorem 5.3.3 (ii).
##  

InstallMethod( CanonicalForm,
     "for a UnipotChevElem",
     [ IsUnipotChevElem ],
     function( x )
          local i,                 # used in afor loop
                canonical,         # the return value
                computeCanonical,  # subroutine
                rep,               # here we remember the representation of <x>
                F;                 # the ring

          F := FamilyObj(x)!.ring;

          ###################################################################
          ##
          ##  computes the canonical form, the argument is a list
          ##  representing the unipotent element
          ##
          computeCanonical := function( roots, felems )
               local k, j, comm;


               k := 2;
               while k <= Length( roots ) do
                    Info(UnipotChevInfo, 5, " ENTERING k = ", k );
                    if roots[k] <= roots[k-1] then
                         Info(UnipotChevInfo, 5, " k (= ", k, ") <= k-1" );
                         j := k;

                         repeat
                              Info(UnipotChevInfo, 5, "   ENTERING j = ", j );

                              if roots[j] = roots[j-1] then
                                   Info(UnipotChevInfo, 5, " j (= ", j, ") = j-1" );
                                   roots := Concatenation(
                                          roots{[1 .. j-2]},           
                                          roots{[j .. Length(roots)]}
                                   );
                                   felems := Concatenation(
                                        # begin of the list
                                          felems{[1 .. j-2]},           
                                        # fuse neighbouring l[j-1] and l[j] 
                                          [ felems[j-1] + felems[j] ],
                                        # the rest of the list    
                                          felems{[j+1 .. Length(felems)]}
                                   );
                                   Info(UnipotChevInfo, 5, " --> ", 
                                                            roots, felems );
          
                                   k := k-1;
                                   Info(UnipotChevInfo, 5, " --> k = ", k );
                              else 
                                   Info(UnipotChevInfo, 5, " j (= ", j, ") < j-1" );
                                   comm := Comm( rec( r :=  roots[j-1],
                                                      x := felems[j-1] ),
                                                 rec( r :=  roots[j],
                                                      x := felems[j]   ),
                                                 "as list",
                                                 FamilyObj(x) );
                                   Info(UnipotChevInfo, 5, "   comm = ", comm );
                                   if comm.roots = [] then
                                         roots{[j-1,j]} := roots{[j,j-1]};
                                        felems{[j-1,j]} := felems{[j,j-1]};
                                        Info(UnipotChevInfo, 5, " --> ", 
                                                  roots, felems );
                                   else
                                        roots := Concatenation(
                                             # begin of the list
                                               roots{[1 .. j-2]},           
                                             # exchange l[j-1] and l[j]
                                               roots{[j]}, roots{[j-1]},
                                             # and don't forget the commutator
                                               comm.roots,
                                             # the rest of the list    
                                               roots{[j+1 .. Length(roots)]}
                                        );
                                        felems := Concatenation(
                                             # begin of the list
                                               felems{[1 .. j-2]},           
                                             # exchange l[j-1] and l[j]
                                               felems{[j]}, felems{[j-1]},
                                             # and don't forget the commutator
                                               comm.felems,
                                             # the rest of the list    
                                               felems{[j+1 .. Length(felems)]}
                                        );
                                        Info(UnipotChevInfo, 5, " --> ", 
                                                  roots, felems );
                                        
                                        # The ordering is preserved until following k:
                                        k := j + Length(comm.roots);
                                        Info(UnipotChevInfo, 5, " --> k = ", k );
                                   fi;
                              fi;
                              j := j-1;
                         until ( j=1 ) or ( roots[j] > roots[j-1] );
                    fi;
                    k := k+1;
               od;
               
               return rec( roots := roots, felems := felems );
          end ;


          # first check if x alredy has the canonical form
          # and return x if so:
          
          canonical := x;
          for i in [1 .. Length(x!.roots)-1] do
               # if Length(x!.list) < 2, the canonical form is
               # alredy stored;
               if x!.roots[i] > x!.roots[i+1] then
                    Unbind(canonical);
               fi;
          od;
          
          # otherwise, compute the canonical form,
          if not IsBound(canonical) then

               if IsUnipotChevRepByRootNumbers(x) then
                    rep := IsUnipotChevRepByRootNumbers;
               elif IsUnipotChevRepByFundamentalCoeffs(x) then
                    rep := IsUnipotChevRepByFundamentalCoeffs;
               elif IsUnipotChevRepByRoots(x) then
                    rep := IsUnipotChevRepByRoots;
               fi;

               canonical :=
                    UnipotChevElem( FamilyObj( x ),
                         computeCanonical( ShallowCopy(x!.roots),
                                           ShallowCopy(x!.felems)  ),
                         rep
                    );
          fi;
          
          # Don't forget to store the canonicall form of the canonical form:
          SetCanonicalForm(canonical, canonical);

          # store the Inverse(x) as the Inverse(canonical) since x = canonical
          # (maybe the canonical form is known for x^-1 alredy. even if it 
          # isn't known now, it makes sense as shown by following example:
          #    gap> x:=...;;
          #    gap> x^-1;;
          #    gap> y:=CanonicalForm(x);;
          #    gap> IsIdenticalObj(x^-1, y^-1);
          #    true
          #  so after computing CanonicalForm(x^-1) we automatically know the
          #  canonical form of y^-1 and vice versa
          # )
          if HasInverse( x ) then
               SetInverse( canonical, Inverse(x) );
          fi;
          
          # Store the Property IsOne of the CanonicalForm and of x!
          SetIsOne(canonical, Length(canonical!.roots) = 0);
          SetIsOne(x,         Length(canonical!.roots) = 0);
          
          return canonical;
     end
);


################################################################################
##
#B  Bibliography
##
#B  [Car72] Roger W. Carter. Simple Groups of Lie Type.
#B          John Wiley \& Sons Ltd., New York, 1972.
#B          Wiley Classics Library Edition Published 1989.
##
################################################################################
