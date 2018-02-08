LoadPackage("unipot");
dirs := DirectoriesPackageLibrary( "unipot", "tst" );
TestDirectory(dirs, rec(exitGAP := true));
