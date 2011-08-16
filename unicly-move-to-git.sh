#!/bin/sh
#
#
### ==============================
### :FILE-CREATED <Timestamp: #{2011-04-20T19:24:08-04:00Z}#{11163} - by MON KEY>
### :FILE unicly/unicly-move-to-git.sh
### ==============================
### :NOTE This script won't work unless $DEVHOME is in your environment 
### elisp> (getenv "DEVHOME")
### shell> $DEVHOME
###
### No doubt there are better ways of doing this with a shell script, but... 
### I F*CKING HATE SHELL SCRIPTING!!!
### ==============================


UNICLY_SRC=$CL_MON_CODE/unicly
UNICLY_GIT=$CL_MON_CODE/unicly-GIT

# TESTING
#NOT_GIT_DIR=$UNICLY_GIT/non-existent-dir 

UNICLY_FILES="unicly.asd
package.lisp
unicly-utils.lisp
unicly-specials.lisp
unicly-macros.lisp
unicly-types.lisp
unicly-class.lisp
unicly.lisp
unicly-bit-vectors.lisp
unicly-docs.lisp
unicly-loadtime-bind.lisp
unicly-tests.lisp
unicly-timings.lisp
unicly-compat.lisp
unicly-deprecated.lisp
unicly-conditions.lisp
LICENSE.txt
README
unicly-move-to-git.sh"

cd $UNICLY_SRC

ensure_abort_dirs () 
{
    for j in `echo "$UNICLY_SRC $UNICLY_GIT"`;do
        if [ ! -d "$j" ]
            then 
            echo "A required directory was non-existent: $j";
            printf "\tdeclining to proceed further\n";
            exit 1;
        fi
  done; 
}

ensure_readme ()
{
 if [ ! -e $UNICLY_GIT/README ]
  then
     echo "Creaating empty README file: $UNICLY_GIT/README"
     echo 
     touch $UNICLY_GIT/README
  fi
}

copy_unicly_files ()
{ 
 for f in $UNICLY_FILES; do 
     if [ ! -e "$UNICLY_SRC/$f" ]                     # Check if file exists.
     then
        echo ":FILE $UNICLY_SRC/$f does not exist";  
        echo
        else                         # On to next.
         cp "$UNICLY_SRC/$f" "$UNICLY_GIT/$f";
         echo "Copied :FILE $f"; 
         echo "From   :SOURCE $UNICLY_SRC/ to :DEST $UNICLY_GIT/";
         echo
     fi
 done;
}

etags_after_copy ()
{
 cd $UNICLY_SRC
 find . -name '*.lisp' -print | xargs etags -o ./TAGS --language=lisp
 echo "etags created $UNICLY_SRC/TAGS"
 echo
 # Don't copy tags from source directory!
 # cp $UNICLY_SRC/TAGS $UNICLY_GIT/TAGS
 # echo "Copied :FILE $UNICLY_SRC/TAGS --> $UNICLY_GIT/TAGS"
 cd $UNICLY_GIT
 find . -name '*.lisp' -print | xargs etags -o ./TAGS --language=lisp
 echo "etags created :FILE $UNICLY_GIT/TAGS"
 echo
}

ensure_abort_dirs
copy_unicly_files
ensure_readme
etags_after_copy

exit 0

### ==============================
### EOF

