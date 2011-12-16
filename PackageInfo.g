################################################################################
##  
#W  PackageInfo.g            for the package `Unipot'              Sergei Haller
##  
#H  @(#)$Id: PackageInfo.g,v 2.8 2004/11/16 16:44:10 gc1007 Exp $
## 
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##
#N  With a new release of the package at least the entries .Version, .Date and
#N  .ArchiveURL must be updated.
##

SetPackageInfo( 
  rec(
    PackageName     := "unipot",
    Subtitle        := "Computing with elements of unipotent subgroups of Chevalley groups",
    Version         := "1.2",
    Date            := "16/11/2004",
    PackageWWWHome  := "http://www.uni-giessen.de/~gc1007/unipot/",
    ArchiveURL      := Concatenation( ~.PackageWWWHome, "unipot-1.2"    ),
    README_URL      := Concatenation( ~.PackageWWWHome, "README"        ),
    PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
    ArchiveFormats  := ".tar.bz2",
    Status          := "deposited",
    Persons := [
      rec(
        LastName      := "Haller",
        FirstNames    := "Sergei",
        IsAuthor      := true,
        IsMaintainer  := true,
        Email         := "Sergei.Haller@math.uni-giessen.de",
        WWWHome       := "http://www.sergei-haller.de",
        Place         := "Gießen",
        Institution   := "Justus-Liebig-Universität Gießen",
        PostalAddress := "Justus-Liebig-Universität Gießen\nMathematisches Institut\nArndtstraße 2\nD-35392 Gießen\nGermany",
      )
    ],
    PackageDoc := rec(
      BookName         := "unipot",
      LongTitle        := "Computing with elements of unipotent subgroups of Chevalley groups",
      ArchiveURLSubset := ["doc","htm"],
      HTMLStart        := "htm/chapters.htm",
      PDFFile          := "doc/manual.pdf",
      SixFile          := "doc/manual.six",
      Autoload         := true
    ),
    Dependencies := rec(
      GAP                    := ">=4.4",
      NeededOtherPackages    := [],
      SuggestedOtherPackages := [],
      ExternalConditions     := []
    ),
    AvailabilityTest := ReturnTrue,
    AbstractHTML     := "Tools for computing with elements of unipotent subgroups of Chevalley groups.",
    BannerString     := Concatenation( " /======================================================\\\n",
                                       " !                                                      !\n",
                                       " !                GAP Package UNIPOT ", ~.Version, "                !\n",
                                       " !       (Computations with elements of unipotent       !\n",
                                       " !             subgroups of Chevalley Groups)           !\n",
                                       " !                                                      !\n",
                                       " ! by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName, " <", ~.Persons[1].Email, "> !\n",
                                       " !                                                      !\n",
                                       " !                    see ??unipot                      !\n",
                                       " \\======================================================/\n" ),

    Keywords         := [ "Chevalley", "unipotent elements", "unipot" ],
    TestFile         := "tst/littletest.tst",

    # Change the following to `true' if you wish the package
    # to be loaded automatically on every start of GAP
    Autoload := false,
  )
);

################################################################################
##
#E  PackageInfo.g  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
