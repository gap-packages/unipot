################################################################################
##
#W  rootsys.gd         Some additional functionality for          Sergei Haller
##                               Root Systems
##
#H  @(#)$Id: rootsys.gd,v 2.4 2004/11/16 16:41:31 gc1007 Exp $
##
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##

############################################################################
##
#A  PositiveRootsFC( <R> )
#A  NegativeRootsFC( <R> )
##
##  The list of positive resp. negative roots of the root system <R>
##  represented   by   coefficients   of   linear  combinations   of
##  fundamental roots SimpleSystem( <R> ).
##
DeclareAttribute( "PositiveRootsFC", IsRootSystem );
DeclareAttribute( "NegativeRootsFC", IsRootSystem );


################################################################################
##
#E  rootsys.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
