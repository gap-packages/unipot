################################################################################
##
#W  unipot.gi               share package `unipot'                 Sergei Haller
##
#H  @(#)$Id: unipot.gi,v 2.3 2000/07/13 12:44:18 gc1007 Exp $
##
##  This is the implementation part of the share package
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
                         [ "list"  ] );
DeclareRepresentation( "IsUnipotChevRepByFundamentalCoeffs",
                         IsAttributeStoringRep,
                         [ "list", "fund_coeffs" ] );
DeclareRepresentation( "IsUnipotChevRepByRoots",
                         IsAttributeStoringRep,
                         [ "list" ] );


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

InstallGlobalFunction(  UnipotChevFamily,
function( type, n, F )

     local Fam,     # The new Family
           L,       # The SimpleLieAlgebra of a given Type
           R,       # The RootSystem of L
           B,       # The Basis of L
           T;       # StructureConstantsTable(B)

     # check the Arguments ...
     if not ( type in ["A", "B", "C", "D", "E", "F", "G"] ) then
          Error( "<type> must be one of ",
                 "\"A\", \"B\", \"C\", \"D\", \"E\", \"F\", \"G\" " );
     fi;


     if   ( type in ["A", "B", "C", "D"] ) and not IsPosInt( n ) then
          Error( "<n> must be a positive integer for type ", type, " " );
     elif ( type = "E" ) and not ( n in [6 ..8] ) then
          Error( "<n> must be one of 6, 7, 8 for type E " );
     elif ( type = "F" ) and ( n <> 4) then
          Error( "<n> must be 4 for type F " );
     elif ( type = "G" ) and ( n <> 2) then
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
     B := Basis( L, Concatenation( R.rootvecs,
                                  BasisVectors(Basis(CartanSubalgebra(L)))
                                ));
     T := StructureConstantsTable( B );  #example:  N_{r,s} = T[r][s][2][1]
      
     # Install the data.
     Fam!.type        := type;
     Fam!.rang        := n;
     Fam!.ring        := F;
##
#T  Brauchen wir die negativen Wurzeln ueberhaupt irgendwo? Falls nicht,
#T  brauchen diese nicht ueberall mitgeschleppt zu werden \dots
#T  Sowohl posroots als auch roots zu speichern ist an sich keine (oder kaum
#T  eine) Speicherplatzverschwendung, da
#T  IsIdenticalObj( posroots[i], roots[i] ) = true
##
     Fam!.roots       := R.roots;
     Fam!.posroots    := R.roots{[1..Length(R.roots)/2]}; 
#    Fam!.rootvecs    := R.rootvecs;    # brauchen wir diese noch?
     Fam!.fundroots   := R.fundroots;
#    Fam!.cartanmat   := R.cartanmat;   # brauchen wir diese noch?
     Fam!.structconst := T; 

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
     ReturnTrue,
     [ IsUnipotChevFamily ], 0,
     function( Fam )
          Print( "UnipotChevFamily( \"",
                 Fam!.type, "\", ",
                 Fam!.rang, ", ",
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
     0,
     function( Fam1, Fam2 )
          return IsIdenticalObj( Fam1, Fam2 )
                 or (     ( Fam1!.type = Fam2!.type )
                      and ( Fam1!.rang = Fam2!.rang )
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
     ReturnTrue,
     [ IsUnipotChevFamily ], 0,
     Fam -> UnipotChevElem( Fam, rec(list:=[]), "ByRootNumbers" )
);









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

InstallGlobalFunction(  UnipotChevSubGr,
function( type, n, F )
     local gr, fam;

     fam := UnipotChevFamily( type, n, F );
     
     gr := Objectify( NewType( CollectionsFamily(fam),
                               IsUnipotChevSubGr and IsAttributeStoringRep
                             ),
                      rec()
                    );

     SetIsWholeFamily( gr, true );

##
#N  see Theorem 5.3.3(i) in [Car72]
##
     SetIsNilpotentGroup( gr, true );
     
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
     ReturnTrue,
     [ IsUnipotChevSubGr ], 0,
     function( U )
          local Fam;
          Fam := ElementsFamily( FamilyObj( U ) );
          
          Print( "UnipotChevSubGr( \"",
                 Fam!.type, "\", ",
                 Fam!.rang, ", ", 
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
     ReturnTrue,
     [ IsUnipotChevSubGr ], 0,
     function( U )
          local  type,
                 Fam;
          Fam := ElementsFamily( FamilyObj( U ) );
          type := Concatenation( Fam!.type, String(Fam!.rang) );
          
          Print("<Unipotent subgroup of a Chevalley group",
                " of type ", type, " over ", Fam!.ring, ">");
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
     0,
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
     ReturnTrue,
     [ IsUnipotChevSubGr ],
     0,
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
     ReturnTrue,
     [ IsUnipotChevSubGr ], 0,
     U -> One(ElementsFamily( FamilyObj( U ) ))
);

################################################################################
##
#M  Size( <U> )  . . . . . . . . . . . . . . . . the size of a `UnipotChevSubGr'
##
##  Special Method for `UnipotChevSubGr'
##
##  Are using the result of [Car72], Theorem 5.3.3 (ii) to compute the size.
##

InstallOtherMethod( Size,
     "for a UnipotChevSubGr",
     ReturnTrue,
     [ IsUnipotChevSubGr ], 0,
     function( U )
          local Fam;

          Fam := ElementsFamily( FamilyObj( U ) );

          if Size(Fam!.ring) = infinity then
               return infinity;
          else
               Info(UnipotChevInfo, 1, "The order of this group is ",
                                     Size(Fam!.ring),"^",Length(Fam!.posroots),
                                     " which is");
               return Size(Fam!.ring)^Length(Fam!.posroots);
          fi;
     end
);

################################################################################
##
#M  RootSystem( <U> )  . . . . . . . . . . the rootsystem of a `UnipotChevSubGr'
##
##  This one is similar to `RootSystem' for semsimple Lie algebras from GAP 4.1
##  (see GAP4.1 Reference Manual, section 58.7 for further information).
##
##  `RootSystem' calculates the root system of the unipotent subgroup <U>.
##  The output is a record with the following components:
##    - `fundroots' A set of fundamental roots
##    - `posroots'  The set of positive roots of the root system. The positive
##                  roots are listed <according to increasing height>.
##                                          

InstallOtherMethod( RootSystem,
     "for a UnipotChevSubGr",
     ReturnTrue,
     [ IsUnipotChevSubGr ],
     0,
     function( U )
          local Fam;

          Fam := ElementsFamily( FamilyObj( U ) );
          
          return 
               rec(
                    posroots  := Fam!.posroots ,
                    fundroots := Fam!.fundroots
               );
     end
);








################################################################################
##
#M  UnipotChevElem( <Fam>, <record>, <rep> )
##
##  This is an `undocumented' function and should not be used at the GAP prompt
##

InstallOtherMethod(  UnipotChevElem,
     "for UnipotChevFamily, a record and a string.",
     ReturnTrue,
     [IsUnipotChevFamily,
      IsRecord,
      IsString],
     0,
     function( Fam, record, rep )
          local elem, i,    # we use them in a for loop
                list, 
                list1,      # used for simplifying list
                fund_coeffs,
                V,B,        # used for constructing fund_coeffs
                F,          # the ring
                obj;        # the returned value

          list := record.list;
          if rep = "ByFundamentalCoeffs" then
               if not IsBound(record.fund_coeffs) then
                    V:= VectorSpace( Rationals, Fam!.fundroots );
                    B:= Basis( V, Fam!.fundroots );
          
                    fund_coeffs := [];

                    for elem in list do
                         Add(fund_coeffs,
                              rec(
                                   coeffs :=
                                       Coefficients( B, Fam!.posroots[elem.r] ),
                                   x := elem.x
                              )
                         );
                    od;
               else
                    fund_coeffs := record.fund_coeffs;
               fi;
          fi;
          
          F := Fam!.ring;

          # check the list
          for elem in [1 .. Length(list)] do
               if not ( IsBound( list[elem] )
                    and IsRecord( list[elem] )
                    and ( AsSet(RecNames( list[elem] )) = AsSet( ["r", "x"] ) )
                      ) then
                    Error(    
                         "<list> must be a dense list of records",
                         " of type rec( r:=<i>, x:=<t> )\n",
                         " where <i> must be an integer in",
                         " [1 .. <number of positive roots>]\n",
                         " and <t> an element of the ring ",
                         F,
                         "."
                    );
               fi;
     
               if not ( list[elem].x in F ) then
                    Error( "<list>[", elem, "].x must be an element",
                           " of the ring ", F, "." );
               fi;
               if not (list[elem].r in [1 .. Length(Fam!.posroots)]) then
                    Error( "<list>[", elem, "].r must be an integer",
                           " in [1 .. <number of positive roots>]." );
               fi;
          od;
     
          # use a structural copy of the list, so if one gave us a mutable list,
          # we do not mute it
          
               list        := StructuralCopy(list);
          if IsBound(fund_coeffs) then
               fund_coeffs := StructuralCopy(fund_coeffs);
          fi;
     
     
#N  Note: The corresponding repeat-loop somewhere in CanonicalForm( <x> )
#N  MUST be always the same as the following loop here.

          # simplifying the list ...
          repeat
               list1 := StructuralCopy(list);
               for i in [ 1 .. Length(list) ] do
                    if list[i].x = Zero(F) then
                         Unbind( list[i] );
                         if IsBound(fund_coeffs) then
                              Unbind( fund_coeffs[i] );
                         fi;
                    elif i < Length(list)
                     and list[i].r = list[i+1].r then
                         list[i+1] := rec( r := list[i+1].r,
                                           x := list[i+1].x + list[i].x);
                         Unbind( list[i] );
                         if IsBound(fund_coeffs) then
                              fund_coeffs[i+1] :=
                                        rec( coeffs := fund_coeffs[i+1].coeffs,
                                                  x := list[i+1].x);
                              Unbind( fund_coeffs[i] );
                         fi;
                    fi;
               od;
               list := Compacted( list );
               if IsBound(fund_coeffs) then
                    fund_coeffs := Compacted( fund_coeffs );
               fi;
          until list = list1;
     
          # Make the record immutable
          # Vielleicht ist es die "bessere" Vorgehensweise, damit sich
          # nicht unbemerkt Fehler einschleichen ...
          
          if rep = "ByRootNumbers" then
               obj := Objectify( NewType( Fam,
                                          IsUnipotChevElem and 
                                          IsUnipotChevRepByRootNumbers and 
                                          IsCopyable),
                                 Immutable( rec(list := list) )
                        );
          elif rep = "ByFundamentalCoeffs" then
               obj := Objectify( NewType( Fam,
                                          IsUnipotChevElem and 
                                          IsUnipotChevRepByFundamentalCoeffs and
                                          IsCopyable),
                                 Immutable( rec(       list := list, 
                                                fund_coeffs := fund_coeffs ) )
                        );
          elif rep = "ByRoots" then
               obj := Objectify( NewType( Fam, 
                                          IsUnipotChevElem and 
                                          IsUnipotChevRepByRoots and 
                                          IsCopyable),
                                 Immutable( rec(list := list ) )
                        );
          else
               Error("3rd argument must not be \"",rep,"\"");
          fi;
          
     
          # we alredy know the canonical form of the object if the
          # length of the list is < 2:
          # it is the object itself:
          if Length(list) < 2 then
               SetCanonicalForm( obj, obj );
          fi;
     
          # we alredy know that the element is the identity if the
          # length of the list is 0:
          if Length(list) = 0 then
               SetIsOne( obj, true );
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

InstallOtherMethod(  UnipotChevElem,
     "for UnipotChevSubGr, a record and a string",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsRecord,
      IsString],
     0,
     function( U, record, rep )
          return UnipotChevElem( ElementsFamily(FamilyObj(U)), record, rep );
     end
);


################################################################################
##
#M  UnipotChevElemByRootNumbers( <U>, <list> )
##
##  Returns an element of a unipotent subgroup <U> with representation
##  `IsUnipotChevRepByRootNumbers'.
##
##  <list> should be a list of records with components <r> and <x> representing
##  the number of the root in `RootSystem(<U>).posroots' and a ring element,
##  respectively. 
##

InstallMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevSubGr and a list of positions of roots",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsList],
     0,
     function( U, list )
          return UnipotChevElem( U, rec(list:=list), "ByRootNumbers" );
     end
);

################################################################################
##
#M  UnipotChevElemByRoots( <U>, <list> )
##
##  Returns an element of a unipotent subgroup <U> with representation
##  `IsUnipotChevRepByRoots'. 
##
##  <list> should be a list of records with components <r> and <x> representing
##  the root in `RootSystem(<U>).posroots' and a ring element, respectively.
##

InstallMethod(  UnipotChevElemByRoots,
     "for UnipotChevSubGr and a list of roots",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsList],
     0,
     function( U, list )
          local list_of_positions,
                elem,
                Fam,
                F;

          Fam := ElementsFamily(FamilyObj(U));

          F := Fam!.ring;
          list_of_positions := [];
          
          # check the list
          for elem in [1 .. Length(list)] do
               if not ( IsBound( list[elem] )
                    and IsRecord( list[elem] )
                    and ( AsSet(RecNames( list[elem] )) = AsSet( ["r", "x"] ) )
                      ) then
     
                         Error(    
                              "<list> must be a dense list of records",
                              " of type rec( r:=<r>, x:=<t> )\n",
                              " where <r> must be a root and <t> an element",
                              " of the ring ",
                              F,
                              "."
                         );
               fi;

               if not ( list[elem].x in F ) then
                    Error( "<list>[", elem, "].x must be an element",
                           " of the ring ", F, "." );
               fi;

               if not ( list[elem].r in Fam!.posroots) then
                    Error( "<list>[", elem, "].r must be a root" );
               fi;
               
               Add(list_of_positions,
                    rec(
                         r := Position( Fam!.posroots, list[elem].r ),
                         x := list[elem].x
                    )
               );
          od;
               
          return UnipotChevElem( U, rec(list:=list_of_positions ), "ByRoots" );
     end
);

################################################################################
##
#M  UnipotChevElemByFundamentalCoeffs( <U>, <list> )
##
##  Returns an element of a unipotent subgroup <U> with representation
##  `IsUnipotChevRepByFundamentalCoeffs'. 
##
##  <list> should be a list of records with components <coeffs> and <x>
##  representing a root in `RootSystem(<U>).posroots' as coefficients of a
##  linear combination of fundamental roots `RootSystem(<U>).fundroots' and a 
##  ring element, respectively.
##

InstallOtherMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevSubGr and a list of coordinates of roots",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsList],
     0,
     function( U, list )
          local list_of_positions,
                elem,
                Fam,
                F,
                str;

          Fam := ElementsFamily(FamilyObj(U));

          F := Fam!.ring;
          list_of_positions := [];
          
          str := "fundamental roots";

          # check the list
          for elem in [1 .. Length(list)] do
               if not ( IsBound( list[elem] )
                    and IsRecord( list[elem] )
                    and ( AsSet( RecNames( list[elem] ) ) 
                          = AsSet( ["coeffs", "x"] ) )
                      ) then
     
                         Error(    
                              "<list> must be a dense list of records",
                              " of type rec( coeffs:=<c>, x:=<t> )\n",
                              " where <coeffs> must be a list of",
                              " coefficients representing a root as a\n",
                              " linear combination of ", str, " and <t>",
                              " an element of the ring ",
                              F,
                              "."
                         );
               fi;

               if not ( list[elem].x in F ) then
                    Error( "<list>[", elem, "].x must be an element",
                           " of the ring ", F, "." );
               fi;

               if not (IsList(list[elem].coeffs) and
                       Length(list[elem].coeffs) = Length(Fam!.fundroots)) then
                    Error( "<list>[", elem, "].coeffs must be a list of",
                           " coefficients representing a root as a\n",
                           " linear combination of ", str );
               fi;
               
               Add(list_of_positions,
                    rec(
                         r := Position( Fam!.posroots,
                                        list[elem].coeffs * Fam!.fundroots ),
                         x := list[elem].x
                    )
               );
          od;
               
          return UnipotChevElem( U, 
                                 rec(       list := list_of_positions,
                                     fund_coeffs := list),
                                 "ByFundamentalCoeffs" );
     end
);

################################################################################
##
#M  UnipotChevElemByRootNumbers( <U>, <r>, <x> )
##
##  This is an abbreviation for `UnipotChevElemByRootNumbers( <U>, <list> )'
##  if <list> consists of only one record.
##

InstallOtherMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevSubGr, a position of a root and a ring rlement",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsPosInt,
      IsObject], # the third argument must be an element of Fam!.ring
     0,
     function( U, r, x )
          return UnipotChevElemByRootNumbers( U, [rec(r:=r, x:=x)] );
     end
);

################################################################################
##
#M  UnipotChevElemByRoos( <U>, <r>, <x> )
##
##  This is an abbreviation for `UnipotChevElemByRoots( <U>, <list> )'
##  if <list> consists of only one record.
##

InstallOtherMethod(  UnipotChevElemByRoots,
     "for UnipotChevSubGr, a root and a ring rlement",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsList,
      IsObject], # the third argument must be an element of Fam!.ring
     0,
     function( U, r, x )
          return UnipotChevElemByRoots( U, [rec(r:=r, x:=x)] );
     end
);

################################################################################
##
#M  UnipotChevElemByFundamentalCoeffs( <U>, <coeffs>, <x> )
##
##  This is an abbreviation for `UnipotChevElemByFundamentalCoeffs(<U>, <list>)'
##  if <list> consists of only one record.
##

InstallOtherMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevSubGr, a lin. comb. of fund. roots and a ring element",
     ReturnTrue,
     [IsUnipotChevSubGr,
      IsList,
      IsObject], # the third argument must be an element of Fam!.ring
     0,
     function( U, coeffs, x )
          return UnipotChevElemByFundamentalCoeffs( U,
                                                    [ rec( coeffs := coeffs,
                                                                x :=  x ) 
                                                    ] );
     end
);

################################################################################
##
#M  UnipotChevElemByRootNumbers( <x> )
##
##  This one is provided for converting elements to the representation
##  `IsUnipotChevRepByRootNumbers'. 
##
##  If <x> has already the representation `IsUnipotChevRepByRootNumbers', then
##  <x> itself is returned. Otherwise a *new* element with representation
##  `IsUnipotChevRepByRootNumbers' is constructed. 
##

InstallOtherMethod(  UnipotChevElemByRootNumbers,
     "for UnipotChevElem",
     ReturnTrue,
     [IsUnipotChevElem],
     0,
     function( x )
          local Fam,
                new_obj,
                new_inv,
                new_canon;

          if IsUnipotChevRepByRootNumbers(x) then 
               return x;
          fi;

          Fam := FamilyObj(x);
          
          new_obj := UnipotChevElem( Fam, rec(list:=x!.list), "ByRootNumbers");
          
          if HasInverse(x) then
               if IsIdenticalObj(x, Inverse(x)) then
                    SetInverse( new_obj, new_obj );
               else
                    new_inv := UnipotChevElem( Fam, 
                                               rec( list := Inverse(x)!.list ),
                                               "ByRootNumbers");
                    SetInverse( new_obj, new_inv );
                    SetInverse( new_inv, new_obj );
                    if HasCanonicalForm(x^-1) then
                         if IsIdenticalObj(x^-1, CanonicalForm(x^-1)) then
                              SetCanonicalForm( new_obj^-1, new_obj^-1 );
                         else
                              new_canon := 
                                   UnipotChevElem( 
                                        Fam,
                                        rec( list := CanonicalForm(x^-1)!.list),
                                        "ByRootNumbers" );
                              SetCanonicalForm( new_inv,   new_canon );
                              SetCanonicalForm( new_canon, new_canon );
                              SetInverse( new_canon, new_obj );
                         fi;
                         
                    fi;
               fi;
          fi;
          if HasCanonicalForm(x) then
               if IsIdenticalObj(x, CanonicalForm(x)) then
                    SetCanonicalForm( new_obj, new_obj );
               else
                    new_canon := UnipotChevElem(
                                    Fam,
                                    rec( list := CanonicalForm(x)!.list ),
                                    "ByRootNumbers" );
                    SetCanonicalForm( new_obj,   new_canon );
                    SetCanonicalForm( new_canon, new_canon );
                    if HasInverse(new_obj) then
                         SetInverse( new_canon, Inverse(new_obj) );
                    fi;
               fi;
          fi;
          
          return new_obj;
     end
);

################################################################################
##
#M  UnipotChevElemByFundamentalCoeffs( <x> )
##
##  This one is provided for converting elements to the representation
##  `IsUnipotChevRepByFundamentalCoeffs'. 
##
##  If <x> has already the representation `IsUnipotChevRepByFundamentalCoeffs',
##  then <x> itself is returned. Otherwise a *new* element with representation
##  `IsUnipotChevRepByFundamentalCoeffs' is constructed. 
##

InstallOtherMethod(  UnipotChevElemByFundamentalCoeffs,
     "for UnipotChevElem",
     ReturnTrue,
     [IsUnipotChevElem],
     0,
     function( x )
          local fund_coeffs,
                Fam,
                new_obj,
                new_inv,
                new_canon;

          if IsUnipotChevRepByFundamentalCoeffs(x) then 
               return x;
          fi;

          Fam := FamilyObj(x);
          
          new_obj := UnipotChevElem( Fam,
                                     rec( list := x!.list ), 
                                     "ByFundamentalCoeffs" );
          
          if HasInverse(x) then
               if IsIdenticalObj(x, Inverse(x)) then
                    SetInverse( new_obj, new_obj );
               else
                    new_inv := UnipotChevElem( Fam, 
                                               rec( list := Inverse(x)!.list ), 
                                               "ByFundamentalCoeffs" );
                    SetInverse( new_obj, new_inv );
                    SetInverse( new_inv, new_obj );
                    if HasCanonicalForm(x^-1) then
                         if IsIdenticalObj(x^-1, CanonicalForm(x^-1)) then
                              SetCanonicalForm( new_obj^-1, new_obj^-1 );
                         else
                              new_canon :=
                                   UnipotChevElem( 
                                        Fam, 
                                        rec( list := CanonicalForm(x^-1)!.list),
                                        "ByFundamentalCoeffs" );
                              SetCanonicalForm( new_inv,   new_canon );
                              SetCanonicalForm( new_canon, new_canon );
                              SetInverse( new_canon, new_obj );
                         fi;
                         
                    fi;
               fi;
          fi;
          if HasCanonicalForm(x) then
               if IsIdenticalObj(x, CanonicalForm(x)) then
                    SetCanonicalForm( new_obj, new_obj );
               else
                    new_canon := UnipotChevElem( 
                                    Fam, 
                                    rec( list := CanonicalForm(x)!.list ), 
                                    "ByFundamentalCoeffs");
                    SetCanonicalForm( new_obj,   new_canon );
                    SetCanonicalForm( new_canon, new_canon );
                    if HasInverse(new_obj) then
                         SetInverse( new_canon, Inverse(new_obj) );
                    fi;
               fi;
          fi;
          
          return new_obj;
     end
);

################################################################################
##
#M  UnipotChevElemByRoots( <x> )
##
##  This one is provided for converting elements to the representation
##  `IsUnipotChevRepByRoots'. 
##
##  If <x> has already the representation `IsUnipotChevRepByRoots', then <x>
##  itself is returned. Otherwise a *new* element with representation
##  `IsUnipotChevRepByRoots' is constructed. 
##

InstallOtherMethod(  UnipotChevElemByRoots,
     "for UnipotChevElem",
     ReturnTrue,
     [IsUnipotChevElem],
     0,
     function( x )
          local Fam,
                new_obj,
                new_inv,
                new_canon;

          if IsUnipotChevRepByRoots(x) then 
               return x;
          fi;

          Fam := FamilyObj(x);
          
          new_obj := UnipotChevElem( Fam, rec(list:=x!.list), "ByRoots");
          
          if HasInverse(x) then
               if IsIdenticalObj(x, Inverse(x)) then
                    SetInverse( new_obj, new_obj );
               else
                    new_inv := UnipotChevElem( Fam, 
                                               rec( list := Inverse(x)!.list ),
                                               "ByRoots");
                    SetInverse( new_obj, new_inv );
                    SetInverse( new_inv, new_obj );
                    if HasCanonicalForm(x^-1) then
                         if IsIdenticalObj(x^-1, CanonicalForm(x^-1)) then
                              SetCanonicalForm( new_obj^-1, new_obj^-1 );
                         else
                              new_canon :=
                                   UnipotChevElem( 
                                       Fam, 
                                       rec( list := CanonicalForm(x^-1)!.list ),
                                       "ByRoots");
                              SetCanonicalForm( new_inv,   new_canon );
                              SetCanonicalForm( new_canon, new_canon );
                              SetInverse( new_canon, new_obj );
                         fi;
                         
                    fi;
               fi;
          fi;
          if HasCanonicalForm(x) then
               if IsIdenticalObj(x, CanonicalForm(x)) then
                    SetCanonicalForm( new_obj, new_obj );
               else
                    new_canon := UnipotChevElem(
                                    Fam, 
                                    rec( list := CanonicalForm(x)!.list ), 
                                    "ByRoots");
                    SetCanonicalForm( new_obj,   new_canon );
                    SetCanonicalForm( new_canon, new_canon );
                    if HasInverse(new_obj) then
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
     ReturnTrue,
     [ IsUnipotChevElem ], 0,
     function( x )
     local Fam;
          if Length(x!.list) = 0 then
               Print("One( ", FamilyObj(x), " )");  
          else

               Fam := FamilyObj(x);
               if IsUnipotChevRepByRootNumbers(x) then
                    Print( "UnipotChevElemByRootNumbers( ",
                           "UnipotChevSubGr( \"",
                           Fam!.type, "\", ", 
                           Fam!.rang, ", ", 
                           Fam!.ring, " )", ", ", 
                           x!.list, " )");
               elif IsUnipotChevRepByFundamentalCoeffs(x) then
                    Print("UnipotChevElemByFundamentalCoeffs( ",
                          "UnipotChevSubGr( \"", 
                          Fam!.type, "\", ", 
                          Fam!.rang, ", ", 
                          Fam!.ring, " )", ", ", 
                          x!.fund_coeffs, " )");
               elif IsUnipotChevRepByRoots(x) then
                    Print("UnipotChevElemByRoots( ",
                          "UnipotChevSubGr( \"", 
                          Fam!.type, "\", ", 
                          Fam!.rang, ", ", 
                          Fam!.ring, " )", ", ", 
                          List(x!.list, l -> rec( r := Fam!.posroots[l.r],
                                                  x := l.x)), " )");
               fi;

          fi;
     end
);

################################################################################
##
#M  ViewObj( <x> )  . . . . . . . . . . . . . . . . . . . . for `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  We have different Methods for each Representation
##
#T  maybe should check for the length of the output and the SizeScreen
##

InstallMethod( ViewObj,
     "for a UnipotChevElem (UnipotChevRepByRootNumbers)",
     ReturnTrue,
     [ IsUnipotChevElem and IsUnipotChevRepByRootNumbers ], 0,
     function( x )
          local elem;

          if Length(x!.list) = 0 then
             Print("<identity>");  
          else
               for elem in [ 1 .. Length(x!.list) ] do
                    if elem > 1 then Print(" * "); fi;
                    Print( "x_{",  x!.list[elem].r,  "}",
                             "( ", x!.list[elem].x, " )"
                         );
               od;
          fi;
     end
);

InstallMethod( ViewObj,
     "for a UnipotChevElem (UnipotChevRepByFundamentalCoeffs)",
     ReturnTrue,
     [ IsUnipotChevElem and IsUnipotChevRepByFundamentalCoeffs ], 0,
     function( x )
          local elem;

          if Length(x!.list) = 0 then
             Print("<identity>");  
          else
               for elem in [ 1 .. Length(x!.fund_coeffs) ] do
                    if elem > 1 then Print(" * "); fi;
                    Print( "x_{",  x!.fund_coeffs[elem].coeffs, "}",
                             "( ", x!.fund_coeffs[elem].x,     " )"
                         );
               od;
          fi;
     end
);

InstallMethod( ViewObj,
     "for a UnipotChevElem (UnipotChevRepByRoots)",
     ReturnTrue,
     [ IsUnipotChevElem and IsUnipotChevRepByRoots ], 0,
     function( x )
          local roots, elem;

          if Length(x!.list) = 0 then
             Print("<identity>");  
          else
               roots := FamilyObj(x)!.posroots;
               for elem in [ 1 .. Length(x!.list) ] do
                    if elem > 1 then Print(" * "); fi;
                    Print( "x_{",  roots[x!.list[elem].r], "}",
                             "( ", x!.list[elem].x,       " )"
                         );
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
     ReturnTrue,
     [ IsUnipotChevElem ], 0,
     function( x )
          local copy;
          
          if IsUnipotChevRepByRootNumbers(x) then
               copy := UnipotChevElem( FamilyObj( x ), 
                                       rec( list := x!.list ), 
                                       "ByRootNumbers" );
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               copy := UnipotChevElem( FamilyObj( x ), 
                                       rec(        list := x!.list, 
                                            fund_coeffs := x!.fund_coeffs ),
                                       "ByFundamentalCoeffs" );
          elif IsUnipotChevRepByRoots(x) then
               copy := UnipotChevElem( FamilyObj( x ), 
                                       rec( list := x!.list ), 
                                       "ByRoots" );
          fi;
          
          if HasInverse(x) then
               SetInverse(copy, Inverse(x));
          fi;
          if HasCanonicalForm(x) then
               SetCanonicalForm(copy, CanonicalForm(x));
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
     0,
     function( x, y )
          if IsIdenticalObj(x,y) or x!.list = y!.list then
               return true;
          elif HasCanonicalForm(x) and HasCanonicalForm(y) then
               return CanonicalForm(x)!.list = CanonicalForm(y)!.list;
          fi;
##          
#N  We would like the following warnings to be seen even if InfoLevel of the
#N  class UnipotChevInfo is set to 0, but it is impossible.
##
          if not HasCanonicalForm(x) then
               Info(UnipotChevInfo, 1,
                          "CanonicalForm for the 1st argument is not known.");
               Info(UnipotChevInfo, 1,
                          "                  computing it may take a while.");
          fi;
          if not HasCanonicalForm(y) then
               Info(UnipotChevInfo, 1,
                          "CanonicalForm for the 2nd argument is not known.");
               Info(UnipotChevInfo, 1,
                          "                  computing it may take a while.");
          fi;

          return CanonicalForm(x)!.list = CanonicalForm(y)!.list;
     end
);

################################################################################
##
#M  <x> * <y>  . . . . . . . . . . . . . multiplication for two `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
#N  If both arguments have same representation, the product will nave it too.
#N  But if the representations are different, the representation of the product
#N  will be the representation of the first argument.
##

InstallMethod( \*,
     "for two UnipotChevElem",
     IsIdenticalObj,
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     0,
     function( x, y )
          local rep;
          
          if IsUnipotChevRepByRootNumbers(x) then
               rep := "ByRootNumbers";
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := "ByFundamentalCoeffs";
          elif IsUnipotChevRepByRoots(x) then
               rep := "ByRoots";
          fi;
          
          return UnipotChevElem( FamilyObj( x ),
                              rec(list := Concatenation( x!.list, y!.list )),
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
     ReturnTrue,
     [ IsUnipotChevElem and IsRootElement,
       IsInt ],
     0,
     function( x, i )
          local rep;
          
          if IsUnipotChevRepByRootNumbers(x) then
               rep := "ByRootNumbers";
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := "ByFundamentalCoeffs";
          elif IsUnipotChevRepByRoots(x) then
               rep := "ByRoots";
          fi;
          
          return UnipotChevElem( FamilyObj( x ),
                              rec(list := [ rec( r :=     x!.list[1].r,
                                                 x := i * x!.list[1].x )
                                          ] ),
                              rep
                            );
     end
);

InstallMethod( \^,
     "for a identity UnipotChevElem and an integer",
     ReturnTrue,
     [ IsUnipotChevElem and IsOne,
       IsInt ],
     0,
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
     ReturnTrue,
     [ IsUnipotChevElem ], 0,
     x -> One( FamilyObj( x ) )
);


################################################################################
##
#M  Inverse( <x> ) . . . . . . . . . . . . . . the inverse of a `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##

InstallMethod( Inverse,
     "for a UnipotChevElem",
     ReturnTrue,
     [ IsUnipotChevElem ], 0,
     function( x )
          local inv;
          if HasCanonicalForm( x ) and HasInverse(CanonicalForm( x )) then
               return Inverse(CanonicalForm( x ));
          fi;
          
          if IsUnipotChevRepByRootNumbers(x) then
               inv := UnipotChevElem( FamilyObj( x ),
                              rec( list := List( Reversed(x!.list),
                                                 e -> rec(r:=e.r, x:=-e.x) ) ),
                              "ByRootNumbers"
                            );
          elif IsUnipotChevRepByRoots(x) then
               inv := UnipotChevElem( FamilyObj( x ),
                              rec( list := List( Reversed(x!.list), 
                                                 e -> rec(r:=e.r, x:=-e.x) ) ),
                              "ByRoots"
                            );
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               inv := UnipotChevElem( FamilyObj( x ),
                         rec(       list := List( Reversed(x!.list),
                                                  e -> rec(      r := e.r, 
                                                                 x := -e.x )),
                             fund_coeffs := List( Reversed(x!.fund_coeffs), 
                                                  e -> rec( coeffs := e.coeffs,
                                                                 x := -e.x )) ),
                         "ByFundamentalCoeffs" );
          fi;
                            
          # We alredy know the inverse of inv: it's x itself.
          SetInverse(inv, x);
          
          # Unfortunately we don't know the CanonicalForm for inv even
          # if we know it for x. If we would, we would store it here.
          # (Note that ``know'' have to be understood in the sense that
          # it is very cheap to get such a value.)

          # but the inverse of the canonical form of x is the same as
          # the inverse of x:
          if HasCanonicalForm( x ) then
               SetInverse(CanonicalForm(x), inv);
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
     ReturnTrue,
     [ IsUnipotChevElem ], 0,
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
                                           "  [ = ", N[r][p][2][1], " ]" );
                    m := m * N[r][p][2][1];
               od;

               return m/Factorial(i);
          end;

          roots := Fam!.posroots;
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
#M  Comm( <x>, <y> )  . . . . . . . . . . . . commutator of two `UnipotChevElem'
##
##  Special Method for `UnipotChevElem'
##
##  Doesn't do anything more than the default Comm, but it stores the inverse
##  elements of <x> and <y>
##

InstallMethod( Comm,
     "for two UnipotChevElem",
     IsIdenticalObj,
     [ IsUnipotChevElem,
       IsUnipotChevElem ],
     0,
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
#    IsIdenticalObj,
#T   die ersten beiden Argumente sollten gleiche Family haben, das dritte
#T   Argument hat mit den beiden ersten nichts zu tun
     ReturnTrue,
#T  Don't know why following don't work (ApplicableMethod reports 
#T  `bad family relations')
#    function(a,b,c)
#         return IsIdenticalObj(a,b);
#    end,
     [ IsUnipotChevElem,
       IsUnipotChevElem,
       IsString ],
     0,
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
#    IsIdenticalObj,
#T   die ersten beiden Argumente sollten gleiche Family haben, das dritte
#T   Argument hat mit den beiden ersten nichts zu tun
#T   checken dies im Funktionsrumpf
     ReturnTrue,
     [ IsUnipotChevElem and IsRootElement,
       IsUnipotChevElem and IsRootElement,
       IsString ],
     0,
     function( x, y, canon)
          local rep, obj;

          if canon <> "canonical" then
               Error("3rd argument (if present) must be \"canonical\"");
          fi;
          if not IsIdenticalObj(FamilyObj(x), FamilyObj(y)) then
               Error("1st and 2nd arguments must have identical Families");
          fi;

          if IsUnipotChevRepByRootNumbers(x) then
               rep := "ByRootNumbers";
          elif IsUnipotChevRepByFundamentalCoeffs(x) then
               rep := "ByFundamentalCoeffs";
          elif IsUnipotChevRepByRoots(x) then
               rep := "ByRoots";
          fi;
     

          obj := UnipotChevElem(
                         FamilyObj(x),
                         rec( list := Comm( x!.list[1],  
                                            y!.list[1], 
                                            "as list", 
                                            FamilyObj(x) ) ),
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
#    IsIdenticalObj,
#T   die ersten beiden Argumente sollten gleiche Family haben, das dritte
#T   Argument hat mit den beiden ersten nichts zu tun
#T   checken dies im Funktionsrumpf
     ReturnTrue,
     [ IsRecord,
       IsRecord,
       IsString,
       IsUnipotChevFamily ],
     0,
     function( x, y, str, Fam )
          local comm,       # will be returned
                roots,      # Roots
                in12, in21, # Hilfsvariablen
                add;        # Hilfsfunktion

          if str <> "as list" then
               Error("3rd argument must be \"as list\"");
          fi;
          
          comm := [];
          roots := Fam!.posroots;

          add := function(i,j)
               Add(comm,
                     rec(
                         r := Position( roots, j*roots[x.r] + i*roots[y.r] ),
                         x := ChevalleyCommutatorConstant( i, j, y.r, x.r, Fam )
                              * (-y.x)^i * (x.x)^j
                         )
               );
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
                         # welche Wurzel kommt in roots zuerst vor?
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
          return comm;
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
     x -> (Length(x!.list) = 1)
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
     ReturnTrue,
     [ IsUnipotChevElem ], 0,
     function( x )
          local i,                 # used in afor loop
                canonical,         # the return value
                computeCanonical,  # subroutine
                rep,               # here we remember the representation of <x>
                F;                 # the ring

          F:=FamilyObj(x)!.ring;

          ###################################################################
          ##
          ##  computes the canonical form, the argument is a list
          ##  representing the unipotent element
          ##
          computeCanonical := function( list )
               local k, j, comm;


               k := 2;
               while k <= Length(list) do
                    Info(UnipotChevInfo, 6, " ENTERING k = ", k );
                    if list[k].r <= list[k-1].r then
                         Info(UnipotChevInfo, 6, " k (= ", k, ") <= k-1" );
                         j := k;

                         repeat
                              Info(UnipotChevInfo, 6, "   ENTERING j = ", j );

                              if list[j].r = list[j-1].r then
                                   Info(UnipotChevInfo, 6, " j (= ", j, ") = j-1" );
                                   list := Concatenation(
                                        # begin of the list
                                          list{[1 .. j-2]},           
                                        # collect neighbouring l[j-1] and l[j] 
                                          [ rec( r := list[j].r,
                                                 x := list[j].x 
                                                    + list[j-1].x ) ],
                                        # the rest of the list    
                                          list{[j+1 .. Length(list)]}
                                   );
                                   Info(UnipotChevInfo, 6, " --> ", list );
          
                                   k := k-1;
                                   Info(UnipotChevInfo, 6, " --> k = ", k );
                              else 
                                   Info(UnipotChevInfo, 6, " j (= ", j, ") < j-1" );
                                   comm := Comm( list[j-1], 
                                                 list[j],
                                                 "as list",
                                                 FamilyObj(x) );
                                   Info(UnipotChevInfo, 6, "   comm = ", comm );
                                   if comm = [] then
                                        list{[j-1,j]} := list{[j,j-1]};
                                        Info(UnipotChevInfo, 6, " --> ", list );
                                   else
                                        list := Concatenation(
                                             # begin of the list
                                               list{[1 .. j-2]},           
                                             # exchange l[j-1] and l[j]
                                               list{[j]}, list{[j-1]},
                                             # and don't forget the commutator
                                               comm,
                                             # the rest of the list    
                                               list{[j+1 .. Length(list)]}
                                        );
                                        Info(UnipotChevInfo, 6, " --> ", list );
                                        
                                        # The ordering is preserved until following k:
                                        k := j + Length(comm);
                                        Info(UnipotChevInfo, 6, " --> k = ", k );
                                   fi;
                              fi;
                              j := j-1;
                         until ( j=1 ) or ( list[j].r > list[j-1].r );
                    fi;
                    k := k+1;
               od;
               
               return list;
          end ;


          # first check if x alredy has the canonical form
          # and return x if so:
          
          canonical := x;
          for i in [1 .. Length(x!.list)-1] do
               # if Length(x!.list) < 2, the canonical form is
               # alredy stored;
               if x!.list[i].r > x!.list[i+1].r then
                    Unbind(canonical);
               fi;
          od;
          
          # otherwise, compute the canonical form,
          if not IsBound(canonical) then

               if IsUnipotChevRepByRootNumbers(x) then
                    rep := "ByRootNumbers";
               elif IsUnipotChevRepByFundamentalCoeffs(x) then
                    rep := "ByFundamentalCoeffs";
               elif IsUnipotChevRepByRoots(x) then
                    rep := "ByRoots";
               fi;

               canonical :=
                    UnipotChevElem( FamilyObj( x ),
                         rec( list := computeCanonical(ShallowCopy(x!.list)) ),
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
          if HasInverse(x) then
               SetInverse(canonical, Inverse(x));
          fi;
          
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
##
#E  unipot.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
