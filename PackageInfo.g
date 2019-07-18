################################################################################
##  
#W  PackageInfo.g            for the package `Unipot'              Sergei Haller
##  
#Y  Copyright (C) 2000-2004, Sergei Haller
#Y  Arbeitsgruppe Algebra, Justus-Liebig-Universitaet Giessen
##

SetPackageInfo( 
  rec(
    PackageName     := "Unipot",
    Subtitle        := "Computing with elements of unipotent subgroups of Chevalley groups",
    Version         := "1.4dev",
    Date            := "09/04/2018", # dd/mm/yyyy format
    License         := "GPL-2.0-or-later",
    PackageWWWHome  := "https://gap-packages.github.io/unipot/",
    README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
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
        IsMaintainer  := false,
        Email         := "sergei@sergei-haller.de",
        WWWHome       := "http://www.sergei-haller.de",
      ),
      rec(
        LastName      := "Horn",
        FirstNames    := "Max",
        IsAuthor      := false,
        IsMaintainer  := true,
        Email         := "max.horn@uni-siegen.de",
        WWWHome       := "https://www.quendi.de/math",
        PostalAddress := Concatenation( "Department Mathematik\n",
                                        "Universität Siegen\n",
                                        "Walter-Flex-Straße 3\n",
                                        "57072 Siegen\n",
                                        "Germany" ),
        Place         := "Siegen, Germany",
        Institution   := "Universität Siegen"
      ),
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
      GAP                    := ">=4.7",
      NeededOtherPackages    := [],
      SuggestedOtherPackages := [],
      ExternalConditions     := []
    ),
    AvailabilityTest := ReturnTrue,
    AbstractHTML     := "Tools for computing with elements of unipotent subgroups of Chevalley groups.",
    Keywords         := [ "Chevalley", "unipotent elements", "unipot" ],
    TestFile         := "tst/testall.g",
  )
);

################################################################################
##
#E  PackageInfo.g  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
