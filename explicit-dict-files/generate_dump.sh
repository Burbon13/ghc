#!/bin/bash
# Check: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html
# Check: https://www.aosabook.org/en/ghc.html

PROGRAM="$1.hs"
TMP_LOCATION="tmp/$1/"
GLOBAL_GHC="ghc"
CUSTOM_GHC="../inplace/bin/ghc-stage2"

GHC=$CUSTOM_GHC
#GHC=$GLOBAL_GHC

rm -r $TMP_LOCATION

$GHC -XDuplicateRecordFields -O2 -ddump-to-file -ddump-file-prefix="$TMP_LOCATION$1." \
  -ddump-parsed-ast -ddump-parsed \
  -ddump-rn-stats -ddump-rn -ddump-rn-ast \
  -ddump-tc -ddump-tc-ast -ddump-types \
  -ddump-ds -ddump-ds-preopt \
  -ddump-simpl-stats -ddump-simpl \
  -ddump-prep \
  $PROGRAM

# -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes \

PrintFile() {
  STAGE_NAME=$1
  IN_FILE=$2

  echo "<div>" >>$HTML_DUMP_FILE
  echo "<h3>$STAGE_NAME</h3>" >>$HTML_DUMP_FILE

  # Check: http://hilite.me/api
  # CODE=$(cat $IN_FILE)
  # MODIFIED_CODE=${CODE/"'GHC.Types.LiftedRep"/"apostrofGHC.Types.LiftedRep"}
  # curl -d "code=$MODIFIED_CODE&lexer=haskell&linenos=true" -X POST http://hilite.me/api > "$IN_FILE.pretty"
  # PRETTY_CODE=$(cat $IN_FILE.pretty)
  # UNMODIFIED_CODE=${PRETTY_CODE/"apostrofGHC"/"'GHC"}
  # cat <<< "$UNMODIFIED_CODE" >> $HTML_DUMP_FILE

  echo "<code><div style=\"border:1px solid black; white-space: pre-wrap\">" >>$HTML_DUMP_FILE
  cat $IN_FILE >>$HTML_DUMP_FILE
  echo "</div></code>" >>$HTML_DUMP_FILE

  echo "</div>" >>$HTML_DUMP_FILE
}

echo "Generating HTML file"

HTML_DUMP_FILE="$TMP_LOCATION$1-dump.html"
PARSER_OUT_FILE="$TMP_LOCATION$1.dump-parsed"
RENAME_OUT_FILE="$TMP_LOCATION$1.dump-rn"
TYPECHECK_OUT_FILE="$TMP_LOCATION$1.dump-tc"
TYPECHECK_SIGNATURES_OUT_FILE="$TMP_LOCATION$1.dump-types"
DESUGAR_BEFORE_OPT_OUT_FILE="$TMP_LOCATION$1.dump-ds-preopt"
DESUGAR_AFTER_OPT_OUT_FILE="$TMP_LOCATION$1.dump-ds"
CORE_2_CORE_OUT_FILE="$TMP_LOCATION$1.verbose-core2core"
TIDY_CORE_OUT_FILE="$TMP_LOCATION$1.dump-simpl"
CORE_PREP_OUT_FILE="$TMP_LOCATION$1.dump-prep"

echo "<html>" >$HTML_DUMP_FILE
echo "<body>" >>$HTML_DUMP_FILE

echo "<h4><a href=\"../../index.html\">GO BACK</a></h4>" >>$HTML_DUMP_FILE

PrintFile "Program" $PROGRAM
PrintFile "Parser" $PARSER_OUT_FILE
PrintFile "Rename" $RENAME_OUT_FILE
PrintFile "Typecheck" $TYPECHECK_OUT_FILE
PrintFile "Type Signatures" $TYPECHECK_SIGNATURES_OUT_FILE
PrintFile "Desugar (before optimization)" $DESUGAR_BEFORE_OPT_OUT_FILE
PrintFile "Desugar (after optimization)" $DESUGAR_AFTER_OPT_OUT_FILE
PrintFile "Core2Core Verbose" $CORE_2_CORE_OUT_FILE
PrintFile "Tidy core" $TIDY_CORE_OUT_FILE
PrintFile "Core prep" $CORE_PREP_OUT_FILE

echo "</body>" >>$HTML_DUMP_FILE
echo "</html>" >>$HTML_DUMP_FILE

rm "$1" "$1".o "$1".hi

INDEX_HTML="index.html"

echo "<html>" >$INDEX_HTML
echo "<body>" >>$INDEX_HTML

echo "<ul>" >>$INDEX_HTML

find . -name '*.html' | while read LINE; do
  echo "<li><a href=\"$LINE\">$LINE</a></li>" >>$INDEX_HTML
done

echo "</ul>" >>$INDEX_HTML

echo "</body>" >>$INDEX_HTML
echo "</html>" >>$INDEX_HTML

echo "Go to:"
echo "file:///$PWD/index.html"
