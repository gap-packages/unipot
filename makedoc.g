#############################################################################
##
##  makedoc.g
##
##  Builds the package documentation with AutoDoc/GAPDoc.
##
#############################################################################

LoadPackage("AutoDoc");

# Run this from the package's root directory: gap makedoc.g
AutoDoc(rec(
    autodoc := true,
    gapdoc := true,
    extract_examples := true,
    scaffold := rec(
        includes := [
            "preface.xml",
            "unipot.xml"
        ],
        entities := rec(
            Unipot := "<Package>Unipot</Package>",
        ),
        bib := "unipot.bib",
    ),
));

QuitGap();
