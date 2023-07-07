#!/bin/sh

VERSION=`grep '^AC_INIT' ../configure.ac|sed -e 's/^.*, *\([^,]\+\),.*$/\1/g'`

echo "Updating version $VERSION"

scp README ../fortrangis-$VERSION.tar.gz \
    sffile:/home/frs/project/fortrangis/fortrangis/
scp *.php projnews.html *.css sffile:/home/project-web/fortrangis/htdocs
rsync -a --delete ../doc/html/ sffile:/home/project-web/fortrangis/htdocs/doc

