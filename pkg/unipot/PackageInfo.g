#############################################################################
##  
##  PackageInfo.g for the package `unipot'  by Sergei Haller 
##  

##  With a new release of the package at least the entries .Version, .Date and
##  .ArchiveURL must be updated.

SetPackageInfo( rec(
PackageName := "unipot",
Subtitle := "Computing in unipotent groups",
Version := "1.1",
Date := "17/03/2000",
ArchiveURL := 
          "http://www.math.rwth-aachen.de:8001/GAP/pkgs/unipot/unipot1r1",
ArchiveFormats := ".zoo",
Persons := [
  rec( 
    LastName      := "Haller",
    FirstNames    := "Sergei",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "Sergei.Haller@math.uni-giessen.de",
    WWWHome       := "http://www.uni-giessen.de/~gc1007/",
    Place         := "Giessen",
    #PostalAddress :=  "",
    Institution   := "Universität Giessen"
  )
],
Status := "deposited",
README_URL := 
  "http://www.math.rwth-aachen.de:8001/GAP/pkgs/unipot/README",
PackageInfoURL := 
  "http://www.math.rwth-aachen.de:8001/GAP/pkgs/unipot/PackageInfo.g",
AbstractHTML := 
"Tools for computing with elements of unipotent subgroups of Chevalley groups.",
PackageWWWHome := "http://www.math.rwth-aachen.de:8001/GAP/pkgs/unipot",
PackageDoc := rec(
  BookName  := "unipot",
  ArchiveURLSubset := ["doc"],
  #HTMLStart := "htm/chapters.htm",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "computing in unipotent groups",
  Autoload  := true
),
Dependencies := rec(
  GAP := ">=4.2",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [],
  ExternalConditions := []
                      
),
AvailabilityTest := ReturnTrue, 
#BannerString := "",
Autoload := false,
#TestFile := "tst/testall.g",
#Keywords := []
));


