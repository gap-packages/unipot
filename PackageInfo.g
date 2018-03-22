################################################################################
##  
#W  PackageInfo.g            for the package `Unipot'              Sergei Haller
##  
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##
#N  With a new release of the package at least the entries .Version, .Date and
#N  .ArchiveURL must be updated.
##

SetPackageInfo( 
  rec(
    PackageName     := "Unipot",
    Subtitle        := "Computing with elements of unipotent subgroups of Chevalley groups",
    Version         := "1.2dev",
    Date            := "16/11/2004",
    PackageWWWHome  := "https://gap-packages.github.io/unipot/",
    README_URL      := Concatenation( ~.PackageWWWHome, "README"        ),
    PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
    SourceRepository := rec(
        Type := "git",
        URL := "https://github.com/gap-packages/unipot",
    ),
    IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
    ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                     "/releases/download/v", ~.Version,
                                     "/unipot-", ~.Version ),
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
