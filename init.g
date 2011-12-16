################################################################################
##
#W  init.g                  share package 'unipot'                 Sergei Haller
##
#H  @(#)$Id: init.g,v 2.2 2000/07/13 11:38:20 gc1007 Exp $
##

DeclarePackage( "unipot", "1.1", 
     function()
          local ret;
          
          # requires at least GAP4.2, because of structure constants,
          # see Manual of unipot, Preface for further information.
          ret := CompareVersionNumbers( VERSION, "4.2" );

          if not ret then
               Info(InfoWarning, 1,
                            "This share package requires at least GAP4.2" );
          fi;
          return ret;
     end
);

# install the documentation immediately after GAP starts
DeclarePackageAutoDocumentation( "unipot", "doc" );

ReadPkg( "unipot", "lib/banner.g");

# read the declaration part.
ReadPkg( "unipot", "lib/unipot.gd");

################################################################################
##
#E  init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
